/-
  AgentMail.Share - Static bundle export/preview helpers.
-/
import AgentMail.Config
import AgentMail.Storage.Database
import Chronos
import Quarry
import Std

namespace AgentMail.Share

open AgentMail
open System
open Quarry

/-- Share export scrub presets. -/
inductive ScrubPreset where
  | standard
  | strict
  | archive
  deriving BEq, Inhabited, Repr

/-- Parse scrub preset string. -/
def ScrubPreset.parse (value : String) : Except String ScrubPreset :=
  match value.trim.toLower with
  | "standard" => .ok .standard
  | "strict" => .ok .strict
  | "archive" => .ok .archive
  | other => .error s!"Unknown scrub preset '{other}'. Supported: standard, strict, archive."

def ScrubPreset.name : ScrubPreset → String
  | .standard => "standard"
  | .strict => "strict"
  | .archive => "archive"

/-- Project metadata used in export manifests. -/
structure ProjectRecord where
  id : Nat
  slug : String
  humanKey : String
  deriving Repr

/-- Project scope summary for exports. -/
structure ProjectScopeResult where
  projects : Array ProjectRecord
  removedCount : Nat
  deriving Repr

/-- Summary of scrub operations applied to a snapshot. -/
structure ScrubSummary where
  preset : String
  pseudonymSalt : String
  agentsTotal : Nat
  agentsPseudonymized : Nat
  ackFlagsCleared : Nat
  recipientsCleared : Nat
  fileReservationsRemoved : Nat
  agentLinksRemoved : Nat
  secretsReplaced : Nat
  attachmentsSanitized : Nat
  bodiesRedacted : Nat
  attachmentsCleared : Nat
  deriving Repr

/-- Export result summary. -/
structure ExportResult where
  outputDir : String
  manifestPath : String
  ftsEnabled : Bool
  deriving Repr

private def nowIso : IO String := do
  let ts ← Chronos.Timestamp.now
  let dt ← Chronos.DateTime.fromTimestampUtc ts
  pure dt.toIso8601

private def escapeSql (value : String) : String :=
  value.replace "'" "''"

private def ensureDir (path : FilePath) : IO Unit :=
  IO.FS.createDirAll path.toString

private def ensureEmptyDir (path : FilePath) (allowExisting : Bool) : IO Unit := do
  if (← path.pathExists) then
    if !(← path.isDir) then
      throw (IO.userError s!"Export path {path} exists and is not a directory.")
    let entries ← path.readDir
    if !entries.isEmpty && !allowExisting then
      throw (IO.userError s!"Export path {path} is not empty; choose a new directory.")
    if allowExisting then
      for entry in entries do
        let full := path / entry.fileName
        if (← full.isDir) then
          IO.FS.removeDirAll full.toString
        else
          IO.FS.removeFile full.toString
  else
    ensureDir path

private partial def copyDir (src dest : FilePath) : IO Unit := do
  ensureDir dest
  let entries ← src.readDir
  for entry in entries do
    let srcPath := src / entry.fileName
    let destPath := dest / entry.fileName
    if (← srcPath.isDir) then
      copyDir srcPath destPath
    else
      let data ← IO.FS.readBinFile srcPath.toString
      IO.FS.writeBinFile destPath.toString data

private def findAssetsRoot : IO FilePath := do
  match ← IO.getEnv "AGENT_MAIL_SHARE_ASSETS" with
  | some envPath =>
    let root := FilePath.mk envPath
    if (← (root / "viewer_assets").isDir) then
      pure root
    else
      throw (IO.userError s!"AGENT_MAIL_SHARE_ASSETS does not contain viewer_assets: {envPath}")
  | none =>
    let candidates : Array FilePath := #[
      FilePath.mk "share_assets",
      FilePath.mk "apps/agent-mail/share_assets",
      FilePath.mk "../share_assets"
    ]
    for cand in candidates do
      if (← (cand / "viewer_assets").isDir) then
        return cand
    throw (IO.userError "Share assets not found. Set AGENT_MAIL_SHARE_ASSETS to the share_assets directory.")

private def copyViewerAssets (outputDir : FilePath) : IO Unit := do
  let assetsRoot ← findAssetsRoot
  let viewerSrc := assetsRoot / "viewer_assets"
  let viewerDest := outputDir / "viewer"
  copyDir viewerSrc viewerDest

private def copyTemplates (outputDir : FilePath) : IO Unit := do
  let assetsRoot ← findAssetsRoot
  let templatesSrc := assetsRoot / "templates"
  if (← templatesSrc.isDir) then
    let templatesDest := outputDir / "templates"
    copyDir templatesSrc templatesDest

private def writeTextFile (path : FilePath) (content : String) : IO Unit := do
  let parent := path.parent
  match parent with
  | some p => ensureDir p
  | none => pure ()
  IO.FS.writeFile path.toString content

private def sha256File (path : FilePath) : IO String := do
  let result ← IO.Process.output {
    cmd := "python3"
    args := #[
      "-c",
      "import hashlib,sys; data=open(sys.argv[1],'rb').read(); print(hashlib.sha256(data).hexdigest())",
      path.toString
    ]
  }
  if result.exitCode == 0 then
    pure result.stdout.trim
  else
    throw (IO.userError s!"sha256 failed: {result.stderr}")

private def getFileSize (path : FilePath) : IO Nat := do
  let data ← IO.FS.readBinFile path.toString
  pure data.size

private def queryCount (db : Quarry.Database) (sql : String) : IO Nat := do
  let rows ← db.query sql
  match rows[0]? with
  | some row =>
    match row.get? 0 with
    | some (Quarry.Value.integer n) => pure n.toNat
    | _ => pure 0
  | none => pure 0

private def resolveProjectRow (db : Quarry.Database) (key : String) : IO (Option ProjectRecord) := do
  let raw := key.trim
  let mut variants : Array String := #[raw]
  let path := FilePath.mk raw
  if path.isAbsolute then
    try
      let canonical ← IO.FS.realPath raw
      if canonical.toString != raw then
        variants := variants.push canonical.toString
    catch _ => pure ()
  let clauses := variants.toList.map (fun v => s!"slug = '{escapeSql v}' OR human_key = '{escapeSql v}'")
  let whereClause := String.intercalate " OR " clauses
  let row ← db.queryOne s!"SELECT id, slug, human_key FROM projects WHERE {whereClause} LIMIT 1"
  pure <| row.bind fun r => do
    let id ← r.get? 0 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let slug ← r.get? 1 >>= fun v => match v with | .text s => some s | _ => none
    let humanKey ← r.get? 2 >>= fun v => match v with | .text s => some s | _ => none
    some { id, slug, humanKey }

private def applyProjectScope (snapshotPath : FilePath) (filters : Array String) : IO ProjectScopeResult := do
  let db ← Quarry.Database.openFile snapshotPath.toString
  try
    let totalProjects ← queryCount db "SELECT COUNT(*) FROM projects"
    let mut records : Array ProjectRecord := #[]
    if filters.isEmpty then
      let rows ← db.query "SELECT id, slug, human_key FROM projects ORDER BY slug"
      for row in rows do
        match row.get? 0, row.get? 1, row.get? 2 with
        | some (.integer id), some (.text slug), some (.text humanKey) =>
          records := records.push { id := id.toNat, slug, humanKey }
        | _, _, _ => pure ()
      pure { projects := records, removedCount := 0 }
    else
      for key in filters do
        match ← resolveProjectRow db key with
        | some record =>
          if !records.any (fun r => r.id == record.id) then
            records := records.push record
        | none => throw (IO.userError s!"Project not found: {key}")
      let keepIds := records.map (·.id)
      let idList := String.intercalate "," (keepIds.toList.map toString)
      let guard := if idList.isEmpty then "(-1)" else s!"({idList})"
      -- Delete dependent rows first to respect foreign keys.
      db.execRaw s!"DELETE FROM message_recipients WHERE message_id NOT IN (SELECT id FROM messages WHERE project_id IN {guard})"
      db.execRaw s!"DELETE FROM messages WHERE project_id NOT IN {guard}"
      db.execRaw s!"DELETE FROM file_reservations WHERE project_id NOT IN {guard}"
      db.execRaw s!"DELETE FROM build_slots WHERE project_id NOT IN {guard}"
      db.execRaw s!"DELETE FROM contact_requests WHERE project_id NOT IN {guard}"
      db.execRaw s!"DELETE FROM contacts WHERE project_id NOT IN {guard}"
      db.execRaw s!"DELETE FROM agents WHERE project_id NOT IN {guard}"
      db.execRaw s!"DELETE FROM product_projects WHERE project_id NOT IN {guard}"
      db.execRaw "DELETE FROM products WHERE id NOT IN (SELECT DISTINCT product_db_id FROM product_projects)"
      db.execRaw s!"DELETE FROM projects WHERE id NOT IN {guard}"
      let removed := totalProjects - keepIds.size
      pure { projects := records, removedCount := removed }
  finally
    db.close

private def scrubSnapshot (snapshotPath : FilePath) (preset : ScrubPreset) : IO ScrubSummary := do
  let db ← Quarry.Database.openFile snapshotPath.toString
  try
    let agentsTotal ← queryCount db "SELECT COUNT(*) FROM agents"
    let ackFlagsCleared ←
      if preset != .archive then
        do
          db.execRaw "UPDATE messages SET ack_required = 0"
          db.changes
      else
        pure 0
    let recipientsCleared ←
      if preset != .archive then
        do
          db.execRaw "UPDATE message_recipients SET read_at = NULL, acked_at = NULL"
          db.changes
      else
        pure 0
    let fileReservationsRemoved ←
      if preset != .archive then
        do
          db.execRaw "DELETE FROM file_reservations"
          db.changes
      else
        pure 0
    if preset != .archive then
      db.execRaw "DELETE FROM build_slots"
    let bodiesRedacted ←
      if preset == .strict then
        do
          db.execRaw "UPDATE messages SET body_md = '[Message body redacted]'"
          db.changes
      else
        pure 0
    let attachmentsCleared ←
      if preset == .strict then
        do
          db.execRaw "UPDATE messages SET attachments = '[]'"
          db.changes
      else
        pure 0
    pure {
      preset := preset.name
      pseudonymSalt := preset.name
      agentsTotal := agentsTotal
      agentsPseudonymized := 0
      ackFlagsCleared := ackFlagsCleared.toNat
      recipientsCleared := recipientsCleared.toNat
      fileReservationsRemoved := fileReservationsRemoved.toNat
      agentLinksRemoved := 0
      secretsReplaced := 0
      attachmentsSanitized := 0
      bodiesRedacted := bodiesRedacted.toNat
      attachmentsCleared := attachmentsCleared.toNat
    }
  finally
    db.close

private def hasColumn (db : Quarry.Database) (table column : String) : IO Bool := do
  let rows ← db.query s!"PRAGMA table_info({table})"
  for row in rows do
    match row.get? 1 with
    | some (Quarry.Value.text name) =>
      if name.toLower == column.toLower then
        return true
    | _ => pure ()
  return false

private def buildSearchIndexes (snapshotPath : FilePath) : IO Bool := do
  let db ← Quarry.Database.openFile snapshotPath.toString
  try
    let ok ← (do
      try
        db.execRaw "CREATE VIRTUAL TABLE IF NOT EXISTS fts_messages USING fts5(subject, body, importance UNINDEXED, project_slug UNINDEXED, thread_key UNINDEXED, created_ts UNINDEXED)"
        db.execRaw "DELETE FROM fts_messages"
        let hasThreadId ← hasColumn db "messages" "thread_id"
        if hasThreadId then
          db.execRaw "
            INSERT INTO fts_messages(rowid, subject, body, importance, project_slug, thread_key, created_ts)
            SELECT
              m.id,
              COALESCE(m.subject, ''),
              COALESCE(m.body_md, ''),
              COALESCE(m.importance, ''),
              COALESCE(p.slug, ''),
              CASE
                WHEN m.thread_id IS NULL OR m.thread_id = '' THEN printf('msg:%d', m.id)
                ELSE m.thread_id
              END,
              COALESCE(m.created_ts, '')
            FROM messages AS m
            LEFT JOIN projects AS p ON p.id = m.project_id
          "
        else
          db.execRaw "
            INSERT INTO fts_messages(rowid, subject, body, importance, project_slug, thread_key, created_ts)
            SELECT
              m.id,
              COALESCE(m.subject, ''),
              COALESCE(m.body_md, ''),
              COALESCE(m.importance, ''),
              COALESCE(p.slug, ''),
              printf('msg:%d', m.id),
              COALESCE(m.created_ts, '')
            FROM messages AS m
            LEFT JOIN projects AS p ON p.id = m.project_id
          "
        db.execRaw "INSERT INTO fts_messages(fts_messages) VALUES('optimize')"
        pure true
      catch _ =>
        pure false)
    pure ok
  finally
    db.close

private def buildMaterializedViews (snapshotPath : FilePath) : IO Unit := do
  let db ← Quarry.Database.openFile snapshotPath.toString
  try
    (do
      let hasThreadId ← hasColumn db "messages" "thread_id"
      let threadExpr := if hasThreadId then "m.thread_id" else "printf('msg:%d', m.id)"
      let script :=
        s!"
        DROP TABLE IF EXISTS message_overview_mv;
        CREATE TABLE message_overview_mv AS
        SELECT
          m.id,
          m.project_id,
          {threadExpr} AS thread_id,
          m.subject,
          m.importance,
          m.ack_required,
          m.created_ts,
          a.name AS sender_name,
          LENGTH(m.body_md) AS body_length,
          json_array_length(m.attachments) AS attachment_count,
          SUBSTR(COALESCE(m.body_md, ''), 1, 280) AS latest_snippet,
          COALESCE(r.recipients, '') AS recipients
        FROM messages m
        JOIN agents a ON m.sender_id = a.id
        LEFT JOIN (
          SELECT
            mr.message_id,
            GROUP_CONCAT(COALESCE(ag.name, ''), ', ') AS recipients
          FROM message_recipients mr
          LEFT JOIN agents ag ON ag.id = mr.agent_id
          GROUP BY mr.message_id
        ) r ON r.message_id = m.id
        ORDER BY m.created_ts DESC;

        CREATE INDEX IF NOT EXISTS idx_msg_overview_created ON message_overview_mv(created_ts DESC);
        CREATE INDEX IF NOT EXISTS idx_msg_overview_thread ON message_overview_mv(thread_id, created_ts DESC);
        CREATE INDEX IF NOT EXISTS idx_msg_overview_project ON message_overview_mv(project_id, created_ts DESC);
        CREATE INDEX IF NOT EXISTS idx_msg_overview_importance ON message_overview_mv(importance, created_ts DESC);
        "
      db.execRaw script
      let attachmentScript :=
        if hasThreadId then
          "
          DROP TABLE IF EXISTS attachments_by_message_mv;
          CREATE TABLE attachments_by_message_mv AS
          SELECT
            m.id AS message_id,
            m.project_id,
            m.thread_id,
            m.created_ts,
            json_extract(value, '$.type') AS attachment_type,
            json_extract(value, '$.media_type') AS media_type,
            json_extract(value, '$.path') AS path,
            CAST(json_extract(value, '$.bytes') AS INTEGER) AS size_bytes
          FROM messages m,
               json_each(m.attachments)
          WHERE m.attachments != '[]';

          CREATE INDEX IF NOT EXISTS idx_attach_by_msg ON attachments_by_message_mv(message_id);
          CREATE INDEX IF NOT EXISTS idx_attach_by_type ON attachments_by_message_mv(attachment_type, created_ts DESC);
          CREATE INDEX IF NOT EXISTS idx_attach_by_project ON attachments_by_message_mv(project_id, created_ts DESC);
          "
        else
          "
          DROP TABLE IF EXISTS attachments_by_message_mv;
          CREATE TABLE attachments_by_message_mv AS
          SELECT
            m.id AS message_id,
            m.project_id,
            NULL AS thread_id,
            m.created_ts,
            json_extract(value, '$.type') AS attachment_type,
            json_extract(value, '$.media_type') AS media_type,
            json_extract(value, '$.path') AS path,
            CAST(json_extract(value, '$.bytes') AS INTEGER) AS size_bytes
          FROM messages m,
               json_each(m.attachments)
          WHERE m.attachments != '[]';

          CREATE INDEX IF NOT EXISTS idx_attach_by_msg ON attachments_by_message_mv(message_id);
          CREATE INDEX IF NOT EXISTS idx_attach_by_type ON attachments_by_message_mv(attachment_type, created_ts DESC);
          CREATE INDEX IF NOT EXISTS idx_attach_by_project ON attachments_by_message_mv(project_id, created_ts DESC);
          "
      try
        db.execRaw attachmentScript
      catch _ => pure ()
      try
        db.execRaw "
          DROP TABLE IF EXISTS fts_search_overview_mv;
          CREATE TABLE fts_search_overview_mv AS
          SELECT
            m.rowid,
            m.id,
            m.subject,
            m.created_ts,
            m.importance,
            a.name AS sender_name,
            SUBSTR(m.body_md, 1, 200) AS snippet
          FROM messages m
          JOIN agents a ON m.sender_id = a.id
          ORDER BY m.created_ts DESC;

          CREATE INDEX IF NOT EXISTS idx_fts_overview_rowid ON fts_search_overview_mv(rowid);
          CREATE INDEX IF NOT EXISTS idx_fts_overview_created ON fts_search_overview_mv(created_ts DESC);
        "
      catch _ => pure ()
    )
  catch e =>
    db.close
    throw e
  db.close

private def createPerformanceIndexes (snapshotPath : FilePath) : IO Unit := do
  let db ← Quarry.Database.openFile snapshotPath.toString
  try
    (do
      try
        db.execRaw "ALTER TABLE messages ADD COLUMN subject_lower TEXT"
      catch _ => pure ()
      try
        db.execRaw "ALTER TABLE messages ADD COLUMN sender_lower TEXT"
      catch _ => pure ()
      db.execRaw "
        UPDATE messages
        SET
          subject_lower = LOWER(COALESCE(subject, '')),
          sender_lower = LOWER(
            COALESCE(
              (SELECT name FROM agents WHERE agents.id = messages.sender_id),
              ''
            )
          )
      "
      db.execRaw "
        CREATE INDEX IF NOT EXISTS idx_messages_created_ts
          ON messages(created_ts DESC);

        CREATE INDEX IF NOT EXISTS idx_messages_subject_lower
          ON messages(subject_lower);

        CREATE INDEX IF NOT EXISTS idx_messages_sender_lower
          ON messages(sender_lower);
      "
      try
        db.execRaw "CREATE INDEX IF NOT EXISTS idx_messages_sender ON messages(sender_id, created_ts DESC)"
      catch _ => pure ()
      try
        db.execRaw "CREATE INDEX IF NOT EXISTS idx_messages_thread ON messages(thread_id, created_ts DESC)"
      catch _ => pure ()
    )
  catch e =>
    db.close
    throw e
  db.close

private def finalizeSnapshotForExport (snapshotPath : FilePath) : IO Unit := do
  let db ← Quarry.Database.openFile snapshotPath.toString
  try
    (do
      db.execRaw "PRAGMA journal_mode=DELETE"
      db.execRaw "PRAGMA page_size=1024"
      db.execRaw "VACUUM"
      db.execRaw "PRAGMA analysis_limit=400"
      db.execRaw "ANALYZE"
      db.execRaw "PRAGMA optimize"
    )
  catch e =>
    db.close
    throw e
  db.close

private def createSnapshot (sourcePath : FilePath) (snapshotPath : FilePath) : IO Unit := do
  let db ← Quarry.Database.openFile sourcePath.toString
  try
    (do
      try
        db.execRaw s!"VACUUM INTO '{escapeSql snapshotPath.toString}'"
      catch _ =>
        let data ← IO.FS.readBinFile sourcePath.toString
        IO.FS.writeBinFile snapshotPath.toString data
    )
  catch e =>
    db.close
    throw e
  db.close

private def indexRedirectHtml : String := "<!doctype html>\n<html lang=\"en\">\n\n<head>\n  <meta charset=\"utf-8\" />\n  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" />\n  <meta http-equiv=\"refresh\" content=\"0; url=./viewer/\" />\n  <title>MCP Agent Mail Viewer</title>\n  <link rel=\"canonical\" href=\"./viewer/\" />\n  <style>\n    :root { color-scheme: light dark; }\n    body {\n      margin: 0;\n      font-family: system-ui, -apple-system, BlinkMacSystemFont, \"Segoe UI\", sans-serif;\n      background: #0f172a;\n      color: #f8fafc;\n      display: grid;\n      place-items: center;\n      min-height: 100vh;\n    }\n    main {\n      text-align: center;\n      padding: 2.5rem;\n      border-radius: 1.25rem;\n      background: rgba(15, 23, 42, 0.85);\n      box-shadow: 0 25px 50px -12px rgba(15, 23, 42, 0.65);\n      max-width: 32rem;\n      backdrop-filter: blur(18px);\n    }\n    h1 {\n      margin-bottom: 1rem;\n      font-size: clamp(1.75rem, 5vw, 2.5rem);\n      font-weight: 700;\n      color: #e0e7ff;\n    }\n    p {\n      margin: 0.75rem 0;\n      line-height: 1.6;\n      color: rgba(226, 232, 240, 0.88);\n    }\n    a { color: #6366f1; text-decoration: none; font-weight: 600; }\n    a:hover { text-decoration: underline; }\n  </style>\n</head>\n\n<body>\n  <main>\n    <h1>MCP Agent Mail Viewer</h1>\n    <p>You are being redirected to the hosted viewer experience.</p>\n    <p>If you are not redirected automatically, <a href=\"./viewer/\">click here to open the viewer</a>.</p>\n  </main>\n  <script>\n    try {\n      const target = new URL(\"./viewer/\", window.location.href);\n      window.location.replace(target.toString());\n    } catch (error) {\n      window.location.href = \"./viewer/\";\n    }\n  </script>\n</body>\n\n</html>\n"

private def headersFile : String :=
  "# Cross-Origin Isolation headers for OPFS and SharedArrayBuffer support\n" ++
  "# Compatible with Cloudflare Pages and Netlify\n" ++
  "# See: https://web.dev/coop-coep/\n\n" ++
  "/*\n" ++
  "  Cross-Origin-Opener-Policy: same-origin\n" ++
  "  Cross-Origin-Embedder-Policy: require-corp\n\n" ++
  "# Allow viewer assets to be loaded\n" ++
  "/viewer/*\n" ++
  "  Cross-Origin-Resource-Policy: same-origin\n\n" ++
  "# SQLite database and chunks\n" ++
  "/*.sqlite3\n" ++
  "  Cross-Origin-Resource-Policy: same-origin\n" ++
  "  Content-Type: application/x-sqlite3\n\n" ++
  "/chunks/*\n" ++
  "  Cross-Origin-Resource-Policy: same-origin\n" ++
  "  Content-Type: application/octet-stream\n\n" ++
  "# Attachments\n" ++
  "/attachments/*\n" ++
  "  Cross-Origin-Resource-Policy: same-origin\n"

private def buildHowToDeploy : String :=
  String.intercalate "\n" [
    "# HOW_TO_DEPLOY",
    "",
    "## Quick Local Preview",
    "1. Run `agent-mail share preview ./` from this bundle directory.",
    "2. Open the printed URL (default `http://127.0.0.1:9000/`).",
    "3. Press Ctrl+C to stop the preview server when finished.",
    "",
    "## Hosting Notes",
    "- For best performance, enable COOP/COEP headers. The included `_headers` file is recognized by Cloudflare Pages and Netlify.",
    "- GitHub Pages does not support `_headers`. Uncomment `coi-serviceworker.js` in `viewer/index.html` to enable isolation via service worker.",
    "",
    "## Troubleshooting",
    "- If `.wasm` is served as text/plain, ensure `.nojekyll` is present and host MIME types are configured.",
    "- If the viewer warns about OPFS, check that cross-origin isolation headers are applied."
  ]

private def writeManifest (outputDir : FilePath) (snapshotPath : FilePath)
    (scope : ProjectScopeResult) (scrub : ScrubSummary) (ftsEnabled : Bool)
    (projectFilters : Array String) : IO FilePath := do
  let size ← getFileSize snapshotPath
  let sha ← sha256File snapshotPath
  let generated ← nowIso
  let projectEntries := scope.projects.map fun p =>
    Lean.Json.mkObj [
      ("slug", Lean.Json.str p.slug),
      ("human_key", Lean.Json.str p.humanKey)
    ]
  let manifest := Lean.Json.mkObj [
    ("schema_version", Lean.Json.str "0.1.0"),
    ("generated_at", Lean.Json.str generated),
    ("exporter_version", Lean.Json.str "lean-share"),
    ("database", Lean.Json.mkObj [
      ("path", Lean.Json.str "mailbox.sqlite3"),
      ("size_bytes", Lean.Json.num size),
      ("sha256", Lean.Json.str sha),
      ("chunked", Lean.Json.bool false),
      ("fts_enabled", Lean.Json.bool ftsEnabled)
    ]),
    ("project_scope", Lean.Json.mkObj [
      ("requested", Lean.Json.arr (projectFilters.map Lean.Json.str)),
      ("included", Lean.Json.arr projectEntries),
      ("removed_count", Lean.Json.num scope.removedCount)
    ]),
    ("scrub", Lean.Json.mkObj [
      ("preset", Lean.Json.str scrub.preset),
      ("pseudonym_salt", Lean.Json.str scrub.pseudonymSalt),
      ("agents_total", Lean.Json.num scrub.agentsTotal),
      ("agents_pseudonymized", Lean.Json.num scrub.agentsPseudonymized),
      ("ack_flags_cleared", Lean.Json.num scrub.ackFlagsCleared),
      ("recipients_cleared", Lean.Json.num scrub.recipientsCleared),
      ("file_reservations_removed", Lean.Json.num scrub.fileReservationsRemoved),
      ("agent_links_removed", Lean.Json.num scrub.agentLinksRemoved),
      ("secrets_replaced", Lean.Json.num scrub.secretsReplaced),
      ("attachments_sanitized", Lean.Json.num scrub.attachmentsSanitized),
      ("bodies_redacted", Lean.Json.num scrub.bodiesRedacted),
      ("attachments_cleared", Lean.Json.num scrub.attachmentsCleared)
    ]),
    ("attachments", Lean.Json.mkObj [
      ("mode", Lean.Json.str "none")
    ]),
    ("notes", Lean.Json.arr #[
      Lean.Json.str "Static export for MCP Agent Mail (Lean).",
      Lean.Json.str "Viewer assets are bundled under viewer/."
    ])
  ]
  let manifestPath := outputDir / "manifest.json"
  writeTextFile manifestPath (Lean.Json.pretty manifest)
  pure manifestPath

private def writeScaffold (outputDir : FilePath) : IO Unit := do
  writeTextFile (outputDir / "README.md") (String.intercalate "\n" [
    "# MCP Agent Mail — Shared Mailbox Snapshot",
    "",
    "This directory contains a scrubbed SQLite snapshot plus a static viewer.",
    "",
    "- `mailbox.sqlite3` — scrubbed mailbox database",
    "- `viewer/` — static web viewer (Alpine.js + Tailwind)",
    "- `manifest.json` — machine-readable metadata",
    "- `_headers` — COOP/COEP headers for compatible hosts",
    "- `.nojekyll` — disable GitHub Pages Jekyll processing",
    "- `HOW_TO_DEPLOY.md` — deployment checklist",
    ""
  ])
  writeTextFile (outputDir / "index.html") indexRedirectHtml
  writeTextFile (outputDir / ".nojekyll") ""
  writeTextFile (outputDir / "HOW_TO_DEPLOY.md") buildHowToDeploy
  writeTextFile (outputDir / "_headers") headersFile

structure ExportOptions where
  outputDir : String
  projects : Array String := #[]
  scrubPreset : ScrubPreset := .standard
  allowExisting : Bool := false
  deriving Repr

/-- Build a share bundle into outputDir. -/
def exportBundle (cfg : Config) (opts : ExportOptions) : IO ExportResult := do
  let outputPath := FilePath.mk opts.outputDir
  ensureEmptyDir outputPath opts.allowExisting
  let dbPath := FilePath.mk cfg.databasePath
  if !(← dbPath.pathExists) then
    throw (IO.userError s!"Database not found: {cfg.databasePath}")
  let snapshotPath := outputPath / "mailbox.sqlite3"
  createSnapshot dbPath snapshotPath
  let scope ← applyProjectScope snapshotPath opts.projects
  let scrub ← scrubSnapshot snapshotPath opts.scrubPreset
  let ftsEnabled ← buildSearchIndexes snapshotPath
  buildMaterializedViews snapshotPath
  createPerformanceIndexes snapshotPath
  finalizeSnapshotForExport snapshotPath
  copyViewerAssets outputPath
  copyTemplates outputPath
  let manifestPath ← writeManifest outputPath snapshotPath scope scrub ftsEnabled opts.projects
  writeScaffold outputPath
  pure { outputDir := outputPath.toString, manifestPath := manifestPath.toString, ftsEnabled }

/-- Update an existing bundle in-place (clears output directory). -/
def updateBundle (cfg : Config) (opts : ExportOptions) : IO ExportResult := do
  exportBundle cfg { opts with allowExisting := true }

/-- Preview a bundle directory using Python's http.server. -/
def previewBundle (root : String) (port : Nat := 9000) : IO Unit := do
  let path := FilePath.mk root
  if !(← path.isDir) then
    throw (IO.userError s!"Preview path is not a directory: {root}")
  IO.println s!"Serving {root} at http://127.0.0.1:{port}/ (Ctrl+C to stop)"
  let child ← IO.Process.spawn {
    cmd := "python3"
    args := #["-m", "http.server", toString port, "--directory", path.toString]
  }
  let _ ← child.wait
  pure ()

/-- Verify that a bundle has a valid manifest and matching database hash. -/
def verifyBundle (root : String) : IO Unit := do
  let path := FilePath.mk root
  if !(← path.isDir) then
    throw (IO.userError s!"Verify path is not a directory: {root}")
  let manifestPath := path / "manifest.json"
  if !(← manifestPath.pathExists) then
    throw (IO.userError s!"manifest.json not found in {root}")
  let manifestText ← IO.FS.readFile manifestPath.toString
  let manifestJson ← match Lean.Json.parse manifestText with
    | .ok v => pure v
    | .error e => throw (IO.userError s!"manifest.json invalid JSON: {e}")
  let dbPath := path / "mailbox.sqlite3"
  if !(← dbPath.pathExists) then
    throw (IO.userError s!"mailbox.sqlite3 not found in {root}")
  let expected := match Lean.Json.getObjVal? manifestJson "database" with
    | Except.ok dbObj => dbObj.getObjValAs? String "sha256"
    | Except.error _ => Except.error "database section missing"
  match expected with
  | Except.ok shaExpected =>
    let shaActual ← sha256File dbPath
    if shaExpected != shaActual then
      throw (IO.userError s!"Database sha256 mismatch: expected {shaExpected}, got {shaActual}")
  | Except.error _ => pure ()

/-- Share wizard placeholder (guidance). -/
def wizardMessage : String :=
  "Share wizard not yet implemented in Lean. Use `agent-mail share export` and deploy the output directory."

end AgentMail.Share
