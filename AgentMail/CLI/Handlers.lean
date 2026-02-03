/-
  AgentMail.CLI.Handlers - CLI command handlers.
-/
import AgentMail.Config
import AgentMail.Storage.Database
import AgentMail.Server.Server
import AgentMail.CLI.Output
import AgentMail.CLI.Commands
import Parlance
import Quarry

namespace AgentMail.CLI.Handlers

open AgentMail
open AgentMail.CLI.Output
open Parlance

/-- Result of command execution -/
inductive Result where
  | success (output : String)
  | error (message : String)
  | runServer  -- Signal to run HTTP server

/-- Get output mode from parse result -/
def getMode (result : ParseResult) : Mode :=
  if result.getBool "json" then .json else .text

/-- Handle 'serve' command - start HTTP server -/
def handleServe : IO Result := do
  pure .runServer

/-- Handle 'list-projects' command -/
def handleListProjects (mode : Mode) : IO Result := do
  let cfg ← Config.fromEnv
  let db ← Storage.Database.openFile cfg.databasePath
  try
    let projects ← db.queryAllProjects
    pure (.success (formatProjects projects mode))
  finally
    db.close

/-- Handle 'list-acks' command -/
def handleListAcks (result : ParseResult) (mode : Mode) : IO Result := do
  let cfg ← Config.fromEnv
  let db ← Storage.Database.openFile cfg.databasePath
  let projectKey ← match result.get (α := String) "project" with
    | some value => pure value
    | none => pure "" -- placeholder; handled below
  let agentName ← match result.get (α := String) "agent" with
    | some value => pure value
    | none => pure ""
  let outcome ←
    if projectKey.isEmpty || agentName.isEmpty then
      pure (.error (formatError "Both --project and --agent are required" mode (some "Usage: agent-mail list-acks --project <slug> --agent <name>")))
    else
      let limit := result.getNat "limit" |>.getD 20
      try
        let acks ← queryPendingAcks db projectKey agentName limit
        pure (.success (formatAcks acks mode))
      catch e =>
        pure (.error (formatError e.toString mode))
  db.close
  pure outcome
where
  resolveProject (db : Storage.Database) (projectKey : String) : IO (Option Project) := do
    let trimmed := projectKey.trim
    let mut candidates : Array String := #[trimmed]
    let path := System.FilePath.mk trimmed
    if path.isAbsolute then
      try
        let canonical ← IO.FS.realPath trimmed
        if canonical.toString != trimmed then
          candidates := candidates.push canonical.toString
      catch _ => pure ()
    for key in candidates do
      match ← db.queryProjectByHumanKey key with
      | some p => return some p
      | none => pure ()
    for key in candidates do
      match ← db.queryProjectBySlug key with
      | some p => return some p
      | none => pure ()
    pure none

  queryPendingAcks (db : Storage.Database) (projectKey : String) (agentName : String) (limit : Nat) : IO (Array PendingAck) := do
    let project ← match ← resolveProject db projectKey with
      | some p => pure p
      | none => throw (IO.userError s!"Project not found: {projectKey}")
    let agents ← db.queryAgentsByProject project.id
    let agentOpt := agents.find? (fun a => a.name.toLower == agentName.toLower)
    let agent ← match agentOpt with
      | some a => pure a
      | none => throw (IO.userError s!"Agent '{agentName}' not found in project '{project.humanKey}'")
    let rows ← db.queryAckRequiredMessages project.id agent.id limit
    pure (rows.map fun entry => {
      messageId := entry.id
      projectSlug := project.slug
      senderName := entry.senderName
      recipientName := agent.name
      subject := entry.subject
      createdTs := entry.createdTs
    })


/-- Handle 'config show-port' command -/
def handleConfigShowPort (mode : Mode) : IO Result := do
  let cfg ← Config.fromEnv
  pure (.success (formatPort cfg.port mode))

/-- Handle 'config set-port' command -/
def handleConfigSetPort (result : ParseResult) (mode : Mode) : IO Result := do
  match result.getNat "port" with
  | none => pure (.error (formatError "Port number is required" mode (some "Usage: agent-mail config set-port <port>")))
  | some port =>
    if port < 1 || port > 65535 then
      pure (.error (formatError s!"Invalid port number: {port}" mode (some "Port must be between 1 and 65535")))
    else
      -- We can't modify .env directly, so provide instructions
      let msg := match mode with
        | .json => s!"\{\"status\": \"info\", \"message\": \"Set AGENT_MAIL_PORT={port} in your environment\"}"
        | .text => s!"To set the port to {port}, add or update this in your environment:\n\n  export AGENT_MAIL_PORT={port}\n\nOr add to your .env file:\n\n  AGENT_MAIL_PORT={port}"
      pure (.success msg)

/-- Handle 'doctor check' command -/
def handleDoctorCheck (mode : Mode) : IO Result := do
  let cfg ← Config.fromEnv
  let db ← Storage.Database.openFile cfg.databasePath
  try
    -- Run SQLite integrity check
    let integrityRows ← db.query "PRAGMA integrity_check"
    let integrityOk := match integrityRows[0]? with
      | some row =>
        match row.get? 0 with
        | some (Quarry.Value.text "ok") => true
        | _ => false
      | none => false

    -- Check for orphan agents (agents with no valid project)
    let orphanAgentRows ← db.query "SELECT COUNT(*) FROM agents WHERE project_id NOT IN (SELECT id FROM projects)"
    let orphanAgents := match orphanAgentRows[0]? with
      | some row =>
        match row.get? 0 with
        | some (Quarry.Value.integer n) => n.toNat
        | _ => 0
      | none => 0

    -- Check for orphan messages (messages with no valid project or sender)
    let orphanMsgRows ← db.query "SELECT COUNT(*) FROM messages WHERE project_id NOT IN (SELECT id FROM projects) OR sender_id NOT IN (SELECT id FROM agents)"
    let orphanMessages := match orphanMsgRows[0]? with
      | some row =>
        match row.get? 0 with
        | some (Quarry.Value.integer n) => n.toNat
        | _ => 0
      | none => 0

    -- Check for orphan recipients
    let orphanRecipRows ← db.query "SELECT COUNT(*) FROM message_recipients WHERE message_id NOT IN (SELECT id FROM messages) OR agent_id NOT IN (SELECT id FROM agents)"
    let orphanRecipients := match orphanRecipRows[0]? with
      | some row =>
        match row.get? 0 with
        | some (Quarry.Value.integer n) => n.toNat
        | _ => 0
      | none => 0

    let result : DoctorResult := {
      integrityOk
      orphanAgents
      orphanMessages
      orphanRecipients
    }
    pure (.success (formatDoctorResult result mode))
  finally
    db.close

/-- Handle 'doctor repair' command -/
def handleDoctorRepair (mode : Mode) : IO Result := do
  let cfg ← Config.fromEnv
  let db ← Storage.Database.openFile cfg.databasePath
  try
    -- VACUUM the database
    db.conn.execRaw "VACUUM"

    -- Reindex (analyze for query optimization)
    db.conn.execRaw "ANALYZE"

    pure (.success (formatSuccess "Database repaired: VACUUM and ANALYZE completed" mode))
  finally
    db.close

/-- Handle 'clear-and-reset' command -/
def handleClearAndReset (result : ParseResult) (mode : Mode) : IO Result := do
  if !result.getBool "force" then
    pure (.error (formatError "This is a destructive operation" mode (some "Use --force to confirm: agent-mail clear-and-reset --force")))
  else
    let cfg ← Config.fromEnv
    let db ← Storage.Database.openFile cfg.databasePath
    try
      -- Drop all tables in reverse dependency order
      db.conn.execSqlDdl "DROP TABLE IF EXISTS message_recipients"
      db.conn.execSqlDdl "DROP TABLE IF EXISTS messages"
      db.conn.execSqlDdl "DROP TABLE IF EXISTS file_reservations"
      db.conn.execSqlDdl "DROP TABLE IF EXISTS build_slots"
      db.conn.execSqlDdl "DROP TABLE IF EXISTS contact_requests"
      db.conn.execSqlDdl "DROP TABLE IF EXISTS contacts"
      db.conn.execSqlDdl "DROP TABLE IF EXISTS product_projects"
      db.conn.execSqlDdl "DROP TABLE IF EXISTS products"
      db.conn.execSqlDdl "DROP TABLE IF EXISTS agents"
      db.conn.execSqlDdl "DROP TABLE IF EXISTS projects"

      -- Reinitialize schema
      for stmt in Storage.schema do
        db.conn.execSqlDdl stmt

      pure (.success (formatSuccess "Database cleared and reinitialized" mode))
    finally
      db.close

/-- Dispatch to the appropriate handler based on command path -/
def dispatch (parseResult : ParseResult) : IO Result := do
  let mode := getMode parseResult
  match parseResult.commandPath with
  | ["serve"] => handleServe
  | ["list-projects"] => handleListProjects mode
  | ["list-acks"] => handleListAcks parseResult mode
  | ["config", "show-port"] => handleConfigShowPort mode
  | ["config", "set-port"] => handleConfigSetPort parseResult mode
  | ["doctor", "check"] => handleDoctorCheck mode
  | ["doctor", "repair"] => handleDoctorRepair mode
  | ["clear-and-reset"] => handleClearAndReset parseResult mode
  | [] => pure .runServer  -- No subcommand = run server (backwards compatible)
  | path => pure (.error (formatError s!"Unknown command: {String.intercalate " " path}" mode (some "Run 'agent-mail --help' for available commands")))

/-- Print help message using Parlance's auto-generation -/
def printHelp : IO Unit :=
  agentMailCommand.printHelp

/-- Run the CLI with given arguments -/
def run (args : List String) : IO UInt32 := do
  -- Check for help/version before parsing (only when no subcommand)
  match args with
  | ["--help"] | ["-h"] =>
    printHelp
    return 0
  | ["--version"] | ["-V"] =>
    IO.println "agent-mail 0.1.0"
    return 0
  | _ => pure ()

  match Parlance.parse agentMailCommand args with
  | .error .helpRequested =>
    printHelp
    pure 0
  | .error .versionRequested =>
    IO.println "agent-mail 0.1.0"
    pure 0
  | .error e =>
    IO.eprintln s!"Error: {e}"
    IO.eprintln "Run 'agent-mail --help' for usage."
    pure 1
  | .ok parseResult =>
    match ← dispatch parseResult with
    | .success output =>
      IO.println output
      pure 0
    | .error msg =>
      IO.eprintln msg
      pure 1
    | .runServer =>
      -- Load configuration and run server
      let cfg ← Config.fromEnv
      let db ← Storage.Database.openFile cfg.databasePath
      try
        Server.run cfg db
        pure 0
      finally
        db.close

end AgentMail.CLI.Handlers
