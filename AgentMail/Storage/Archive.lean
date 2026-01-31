/-
  AgentMail.Storage.Archive - Git archive storage for artifacts
-/
import Chronos
import AgentMail.Config
import AgentMail.Models.Agent
import AgentMail.Models.FileReservation
import AgentMail.Models.BuildSlot
import AgentMail.Models.Message

namespace AgentMail.Storage

structure ProjectArchive where
  repoRoot : String
  projectRoot : String
  slug : String
  gitAuthorName : String
  gitAuthorEmail : String
  lockPath : String
  deriving Repr

private def expandUser (path : String) : IO String := do
  if path.startsWith "~/" then
    match ← IO.getEnv "HOME" with
    | some home => pure (home ++ path.drop 1)
    | none => pure path
  else
    pure path

private def runGit (repoRoot : String) (args : Array String) (env : Array (String × String) := #[]) : IO String := do
  let envOpt := env.map (fun (k, v) => (k, some v))
  let result ← IO.Process.output {
    cmd := "git"
    args := #["-C", repoRoot] ++ args
    env := envOpt
  }
  if result.exitCode == 0 then
    pure result.stdout
  else
    throw (IO.userError s!"git {String.intercalate " " args.toList} failed: {result.stderr}")

private def ensureRepo (repoRoot : String) (cfg : Config) : IO Unit := do
  IO.FS.createDirAll repoRoot
  let repoCheck ← IO.Process.output {
    cmd := "git"
    args := #["-C", repoRoot, "rev-parse", "--git-dir"]
  }
  if repoCheck.exitCode != 0 then
    let _ ← runGit repoRoot #["init"]
    let attributesPath := s!"{repoRoot}/.gitattributes"
    if !(← (System.FilePath.mk attributesPath).pathExists) then
      IO.FS.writeFile attributesPath "*.json text\n*.md text\n"
    let env := #[
      ("GIT_AUTHOR_NAME", cfg.gitAuthorName),
      ("GIT_AUTHOR_EMAIL", cfg.gitAuthorEmail),
      ("GIT_COMMITTER_NAME", cfg.gitAuthorName),
      ("GIT_COMMITTER_EMAIL", cfg.gitAuthorEmail)
    ]
    let _ ← runGit repoRoot #["add", ".gitattributes"] env
    let diff ← IO.Process.output { cmd := "git", args := #["-C", repoRoot, "diff", "--cached", "--quiet"] }
    if diff.exitCode == 1 then
      let _ ← runGit repoRoot #["commit", "-m", "chore: initialize archive"] env
    else
      pure ()
  else
    pure ()

def ensureProjectArchive (cfg : Config) (slug : String) : IO ProjectArchive := do
  let rootRaw := cfg.storageRoot
  let repoRoot ← expandUser rootRaw
  ensureRepo repoRoot cfg
  let projectRoot := s!"{repoRoot}/projects/{slug}"
  IO.FS.createDirAll projectRoot
  let _ ← IO.FS.createDirAll s!"{projectRoot}/agents"
  let _ ← IO.FS.createDirAll s!"{projectRoot}/messages"
  let _ ← IO.FS.createDirAll s!"{projectRoot}/file_reservations"
  let _ ← IO.FS.createDirAll s!"{projectRoot}/build_slots"
  let _ ← IO.FS.createDirAll s!"{projectRoot}/attachments"
  let _ ← IO.FS.createDirAll s!"{projectRoot}/threads"
  let _ ← IO.FS.createDirAll s!"{repoRoot}/products"
  pure {
    repoRoot := repoRoot
    projectRoot := projectRoot
    slug := slug
    gitAuthorName := cfg.gitAuthorName
    gitAuthorEmail := cfg.gitAuthorEmail
    lockPath := s!"{projectRoot}/.archive.lock"
  }

private def writeJsonFile (path : String) (json : Lean.Json) : IO Unit := do
  let parent := (System.FilePath.mk path).parent
  match parent with
  | some p => IO.FS.createDirAll p.toString
  | none => pure ()
  IO.FS.writeFile path (Lean.Json.pretty json)

private def writeTextFile (path : String) (content : String) : IO Unit := do
  let parent := (System.FilePath.mk path).parent
  match parent with
  | some p => IO.FS.createDirAll p.toString
  | none => pure ()
  IO.FS.writeFile path content

def timestampToIso (ts : Chronos.Timestamp) : IO String := do
  let dt ← Chronos.DateTime.fromTimestampUtc ts
  pure dt.toIso8601

private def sha1Hex (input : String) : IO String := do
  let result ← IO.Process.output {
    cmd := "python3"
    args := #["-c", "import hashlib,sys; print(hashlib.sha1(sys.argv[1].encode('utf-8')).hexdigest())", input]
  }
  if result.exitCode == 0 then
    pure result.stdout.trim
  else
    throw (IO.userError s!"sha1 failed: {result.stderr}")

private def commitPaths (archive : ProjectArchive) (paths : Array String) (message : String) : IO Unit := do
  let env := #[
    ("GIT_AUTHOR_NAME", archive.gitAuthorName),
    ("GIT_AUTHOR_EMAIL", archive.gitAuthorEmail),
    ("GIT_COMMITTER_NAME", archive.gitAuthorName),
    ("GIT_COMMITTER_EMAIL", archive.gitAuthorEmail)
  ]
  for path in paths do
    let _ ← runGit archive.repoRoot #["add", path] env
  let diff ← IO.Process.output { cmd := "git", args := #["-C", archive.repoRoot, "diff", "--cached", "--quiet"] }
  if diff.exitCode == 1 then
    let _ ← runGit archive.repoRoot #["commit", "-m", message] env
  else
    pure ()

def writeAgentProfile (archive : ProjectArchive) (agent : Agent) : IO Unit := do
  let inceptionIso ← timestampToIso agent.inceptionTs
  let lastActiveIso ← timestampToIso agent.lastActiveTs
  let json := Lean.Json.mkObj [
    ("id", Lean.Json.num agent.id),
    ("name", Lean.Json.str agent.name),
    ("program", Lean.Json.str agent.program),
    ("model", Lean.Json.str agent.model),
    ("task_description", Lean.Json.str agent.taskDescription),
    ("inception_ts", Lean.Json.str inceptionIso),
    ("last_active_ts", Lean.Json.str lastActiveIso),
    ("project_id", Lean.Json.num agent.projectId),
    ("attachments_policy", Lean.toJson agent.attachmentsPolicy)
  ]
  let path := s!"{archive.projectRoot}/agents/{agent.name}/profile.json"
  writeJsonFile path json
  let rel := s!"projects/{archive.slug}/agents/{agent.name}/profile.json"
  commitPaths archive #[rel] s!"agent: profile {agent.name}"

def writeFileReservationRecords (archive : ProjectArchive) (records : Array Lean.Json) : IO Unit := do
  if records.isEmpty then
    pure ()
  else
    let mut relPaths : Array String := #[]
    let mut entries : Array (String × String) := #[]
    for record in records do
      let pathPattern ← match record.getObjValAs? String "path_pattern" with
        | Except.ok p => pure p
        | Except.error _ => throw (IO.userError "file reservation record missing path_pattern")
      let agentName := match record.getObjValAs? String "agent" with
        | Except.ok a => a
        | Except.error _ => "unknown"
      let digest ← sha1Hex pathPattern
      let legacyPath := s!"{archive.projectRoot}/file_reservations/{digest}.json"
      writeJsonFile legacyPath record
      relPaths := relPaths.push s!"projects/{archive.slug}/file_reservations/{digest}.json"
      match record.getObjValAs? Nat "id" with
      | Except.ok id =>
          let idPath := s!"{archive.projectRoot}/file_reservations/id-{id}.json"
          writeJsonFile idPath record
          relPaths := relPaths.push s!"projects/{archive.slug}/file_reservations/id-{id}.json"
      | Except.error _ => pure ()
      entries := entries.push (agentName, pathPattern)
    let commitMessage :=
      match entries.toList with
      | [] => "file_reservation: update"
      | (firstAgent, firstPattern) :: rest =>
          if rest.isEmpty then
            s!"file_reservation: {firstAgent} {firstPattern}"
          else
            let extra := rest.length
            let subject := s!"file_reservation: {firstAgent} {firstPattern} (+{extra} more)"
            let body := String.intercalate "\n" (rest.map (fun (a, p) => s!"- {a} {p}"))
            subject ++ "\n\n" ++ body
    commitPaths archive relPaths commitMessage

private def trimDashes (s : String) : String :=
  let chars := s.toList
  let leftTrim := chars.dropWhile (· == '-')
  let rightTrim := leftTrim.reverse.dropWhile (· == '-')
  String.ofList rightTrim.reverse

private def subjectSlug (subject : String) : String :=
  let lower := subject.toLower
  let (out, _) :=
    lower.toList.foldl
      (fun (acc : List Char × Bool) c =>
        let (chars, lastDash) := acc
        if c.isAlphanum then
          (chars ++ [c], false)
        else if !lastDash then
          (chars ++ ['-'], true)
        else
          (chars, lastDash))
      ([], false)
  let s := String.ofList out
  let trimmed := trimDashes s
  if trimmed.isEmpty then "message" else trimmed

def writeMessageBundle
    (archive : ProjectArchive)
    (messageJson : Lean.Json)
    (bodyMd : String)
    (sender : String)
    (recipients : Array String)
    (createdTs : Chronos.Timestamp)
    (subject : String)
    (threadId : Option String) : IO Unit := do
  let createdIso ← timestampToIso createdTs
  let dt ← Chronos.DateTime.fromTimestampUtc createdTs
  let iso := dt.toIso8601
  let yDir := iso.take 4
  let mDir := iso.drop 5 |>.take 2
  let subjectSlug := subjectSlug subject
  let idSuffix := match messageJson.getObjValAs? Nat "id" with
    | Except.ok id => s!"__{id}"
    | Except.error _ => ""
  let fileName := s!"{iso.replace ":" "-"}__{subjectSlug}{idSuffix}.md"
  let canonicalDir := s!"{archive.projectRoot}/messages/{yDir}/{mDir}"
  let outboxDir := s!"{archive.projectRoot}/agents/{sender}/outbox/{yDir}/{mDir}"
  let inboxDirs := recipients.map (fun r => s!"{archive.projectRoot}/agents/{r}/inbox/{yDir}/{mDir}")
  let frontmatter := Lean.Json.pretty messageJson
  let content := s!"---json\n{frontmatter}\n---\n\n{bodyMd.trim}\n"
  let canonicalPath := s!"{canonicalDir}/{fileName}"
  writeTextFile canonicalPath content
  let outboxPath := s!"{outboxDir}/{fileName}"
  writeTextFile outboxPath content
  for dir in inboxDirs do
    let inboxPath := s!"{dir}/{fileName}"
    writeTextFile inboxPath content
  let relPaths : Array String :=
    #[s!"projects/{archive.slug}/messages/{yDir}/{mDir}/{fileName}",
      s!"projects/{archive.slug}/agents/{sender}/outbox/{yDir}/{mDir}/{fileName}"] ++
    (inboxDirs.map fun dir =>
      let suffix := dir.drop (archive.projectRoot.length + 1)
      s!"projects/{archive.slug}/{suffix}/{fileName}")
  let recipientList := String.intercalate ", " recipients.toList
  let commitSubject := s!"mail: {sender} -> {recipientList} | {subject}"
  let threadKey := match threadId with | some t => t | none => ""
  let commitBody :=
    String.intercalate "\n" [
      "TOOL: send_message",
      s!"Agent: {sender}",
      s!"Project: {archive.slug}",
      s!"Started: {createdIso}",
      "Status: SUCCESS",
      s!"Thread: {threadKey}"
    ]
  commitPaths archive relPaths (commitSubject ++ "\n\n" ++ commitBody ++ "\n")

def writeBuildSlotRecords (archive : ProjectArchive) (records : Array Lean.Json) : IO Unit := do
  if records.isEmpty then
    pure ()
  else
    let mut relPaths : Array String := #[]
    let mut entries : Array (String × String) := #[]
    for record in records do
      let slotName ← match record.getObjValAs? String "slot_name" with
        | Except.ok s => pure s
        | Except.error _ => throw (IO.userError "build slot record missing slot_name")
      let agentName := match record.getObjValAs? String "agent" with
        | Except.ok a => a
        | Except.error _ => "unknown"
      let digest ← sha1Hex slotName
      let legacyPath := s!"{archive.projectRoot}/build_slots/{digest}.json"
      writeJsonFile legacyPath record
      relPaths := relPaths.push s!"projects/{archive.slug}/build_slots/{digest}.json"
      match record.getObjValAs? Nat "id" with
      | Except.ok id =>
          let idPath := s!"{archive.projectRoot}/build_slots/id-{id}.json"
          writeJsonFile idPath record
          relPaths := relPaths.push s!"projects/{archive.slug}/build_slots/id-{id}.json"
      | Except.error _ => pure ()
      entries := entries.push (agentName, slotName)
    let commitMessage :=
      match entries.toList with
      | [] => "build_slot: update"
      | (firstAgent, firstSlot) :: rest =>
          if rest.isEmpty then
            s!"build_slot: {firstAgent} {firstSlot}"
          else
            let extra := rest.length
            let subject := s!"build_slot: {firstAgent} {firstSlot} (+{extra} more)"
            let body := String.intercalate "\n" (rest.map (fun (a, s) => s!"- {a} {s}"))
            subject ++ "\n\n" ++ body
    commitPaths archive relPaths commitMessage

end AgentMail.Storage
