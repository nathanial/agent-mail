/-
  AgentMail.Storage.Database - SQLite database connection and schema
-/
import Quarry
import Chronos
import AgentMail.Models.Project
import AgentMail.Models.Agent
import AgentMail.Models.Types

namespace AgentMail.Storage

/-- SQL schema for agent-mail database -/
def schema : Array String := #[
  -- Projects table
  "CREATE TABLE IF NOT EXISTS projects (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    slug TEXT NOT NULL UNIQUE,
    human_key TEXT NOT NULL UNIQUE,
    created_at INTEGER NOT NULL
  )",

  -- Agents table
  "CREATE TABLE IF NOT EXISTS agents (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    project_id INTEGER NOT NULL REFERENCES projects(id),
    name TEXT NOT NULL,
    program TEXT NOT NULL,
    model TEXT NOT NULL,
    task_description TEXT DEFAULT '',
    contact_policy TEXT DEFAULT 'auto',
    attachments_policy TEXT DEFAULT 'auto',
    inception_ts INTEGER NOT NULL,
    last_active_ts INTEGER NOT NULL,
    UNIQUE(project_id, name)
  )",

  -- Messages table
  "CREATE TABLE IF NOT EXISTS messages (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    project_id INTEGER NOT NULL REFERENCES projects(id),
    sender_id INTEGER NOT NULL REFERENCES agents(id),
    subject TEXT NOT NULL,
    body_md TEXT NOT NULL,
    importance TEXT DEFAULT 'normal',
    ack_required INTEGER DEFAULT 0,
    thread_id TEXT,
    created_ts INTEGER NOT NULL
  )",

  -- Message recipients junction table
  "CREATE TABLE IF NOT EXISTS message_recipients (
    message_id INTEGER NOT NULL REFERENCES messages(id),
    agent_id INTEGER NOT NULL REFERENCES agents(id),
    recipient_type TEXT DEFAULT 'to',
    read_at INTEGER,
    acked_at INTEGER,
    PRIMARY KEY (message_id, agent_id)
  )",

  -- File reservations table
  "CREATE TABLE IF NOT EXISTS file_reservations (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    project_id INTEGER NOT NULL REFERENCES projects(id),
    agent_id INTEGER NOT NULL REFERENCES agents(id),
    path_pattern TEXT NOT NULL,
    exclusive INTEGER DEFAULT 1,
    reason TEXT DEFAULT '',
    created_ts INTEGER NOT NULL,
    expires_ts INTEGER NOT NULL,
    released_ts INTEGER
  )",

  -- Indexes for performance
  "CREATE INDEX IF NOT EXISTS idx_agents_project ON agents(project_id)",
  "CREATE INDEX IF NOT EXISTS idx_messages_project ON messages(project_id)",
  "CREATE INDEX IF NOT EXISTS idx_messages_sender ON messages(sender_id)",
  "CREATE INDEX IF NOT EXISTS idx_messages_thread ON messages(thread_id)",
  "CREATE INDEX IF NOT EXISTS idx_recipients_agent ON message_recipients(agent_id)",
  "CREATE INDEX IF NOT EXISTS idx_reservations_project ON file_reservations(project_id)",
  "CREATE INDEX IF NOT EXISTS idx_reservations_agent ON file_reservations(agent_id)",
  "CREATE INDEX IF NOT EXISTS idx_reservations_active ON file_reservations(expires_ts, released_ts)"
]

/-- Database connection wrapper -/
structure Database where
  conn : Quarry.Database
  path : String

namespace Database

/-- Open a database connection and initialize schema -/
def openFile (path : String) : IO Database := do
  let conn ← Quarry.Database.openFile path
  -- Enable WAL mode for better concurrent access (use execRaw for PRAGMA)
  conn.execRaw "PRAGMA journal_mode=WAL"
  -- Enable foreign keys
  conn.execRaw "PRAGMA foreign_keys=ON"
  -- Initialize schema
  for stmt in schema do
    conn.execSqlDdl stmt
  pure { conn, path }

/-- Open an in-memory database (for testing) -/
def openMemory : IO Database := do
  let conn ← Quarry.Database.openMemory
  -- Enable foreign keys (use execRaw for PRAGMA)
  conn.execRaw "PRAGMA foreign_keys=ON"
  -- Initialize schema
  for stmt in schema do
    conn.execSqlDdl stmt
  pure { conn, path := ":memory:" }

/-- Close the database connection -/
def close (db : Database) : IO Unit :=
  db.conn.close

/-- Execute a DDL statement -/
def execDdl (db : Database) (sql : String) : IO Unit :=
  db.conn.execSqlDdl sql

/-- Execute an insert and return the last rowid -/
def insert (db : Database) (sql : String) : IO Int :=
  db.conn.execSqlInsert sql

/-- Execute a modification (update/delete) and return rows affected -/
def modify (db : Database) (sql : String) : IO Int :=
  db.conn.execSqlModify sql

/-- Execute a query and return all rows -/
def query (db : Database) (sql : String) : IO (Array Quarry.Row) :=
  db.conn.query sql

/-- Execute a query and return the first row if any -/
def queryOne (db : Database) (sql : String) : IO (Option Quarry.Row) :=
  db.conn.queryOne sql

/-- Run a function inside a transaction -/
def transaction (db : Database) (action : IO α) : IO α :=
  db.conn.transaction action

-- =============================================================================
-- Project queries
-- =============================================================================

/-- Query a project by its human_key (path) -/
def queryProjectByHumanKey (db : Database) (humanKey : String) : IO (Option AgentMail.Project) := do
  let escaped := humanKey.replace "'" "''"
  let row ← db.queryOne s!"SELECT id, slug, human_key, created_at FROM projects WHERE human_key = '{escaped}'"
  pure (row.bind rowToProject)
where
  rowToProject (row : Quarry.Row) : Option AgentMail.Project := do
    let id ← row.get? 0 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let slug ← row.get? 1 >>= fun v => match v with | .text s => some s | _ => none
    let humanKey ← row.get? 2 >>= fun v => match v with | .text s => some s | _ => none
    let createdAt ← row.get? 3 >>= fun v => match v with | .integer n => some n | _ => none
    some { id, slug, humanKey, createdAt := Chronos.Timestamp.fromSeconds createdAt }

/-- Query a project by its ID -/
def queryProjectById (db : Database) (id : Nat) : IO (Option AgentMail.Project) := do
  let row ← db.queryOne s!"SELECT id, slug, human_key, created_at FROM projects WHERE id = {id}"
  pure (row.bind rowToProject)
where
  rowToProject (row : Quarry.Row) : Option AgentMail.Project := do
    let id ← row.get? 0 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let slug ← row.get? 1 >>= fun v => match v with | .text s => some s | _ => none
    let humanKey ← row.get? 2 >>= fun v => match v with | .text s => some s | _ => none
    let createdAt ← row.get? 3 >>= fun v => match v with | .integer n => some n | _ => none
    some { id, slug, humanKey, createdAt := Chronos.Timestamp.fromSeconds createdAt }

/-- Query a project by its slug -/
def queryProjectBySlug (db : Database) (slug : String) : IO (Option AgentMail.Project) := do
  let slugEsc := slug.replace "'" "''"
  let row ← db.queryOne s!"SELECT id, slug, human_key, created_at FROM projects WHERE slug = '{slugEsc}'"
  pure (row.bind rowToProject)
where
  rowToProject (row : Quarry.Row) : Option AgentMail.Project := do
    let id ← row.get? 0 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let slug ← row.get? 1 >>= fun v => match v with | .text s => some s | _ => none
    let humanKey ← row.get? 2 >>= fun v => match v with | .text s => some s | _ => none
    let createdAt ← row.get? 3 >>= fun v => match v with | .integer n => some n | _ => none
    some { id, slug, humanKey, createdAt := Chronos.Timestamp.fromSeconds createdAt }

/-- Insert a new project and return its ID -/
def insertProject (db : Database) (slug : String) (humanKey : String) (createdAt : Chronos.Timestamp) : IO Nat := do
  let slugEsc := slug.replace "'" "''"
  let keyEsc := humanKey.replace "'" "''"
  let id ← db.insert s!"INSERT INTO projects (slug, human_key, created_at) VALUES ('{slugEsc}', '{keyEsc}', {createdAt.seconds})"
  pure id.toNat

-- =============================================================================
-- Agent queries
-- =============================================================================

/-- Query an agent by name within a project -/
def queryAgentByName (db : Database) (projectId : Nat) (name : String) : IO (Option AgentMail.Agent) := do
  let nameEsc := name.replace "'" "''"
  let row ← db.queryOne s!"SELECT id, project_id, name, program, model, task_description, contact_policy, attachments_policy, inception_ts, last_active_ts FROM agents WHERE project_id = {projectId} AND name = '{nameEsc}'"
  pure (row.bind rowToAgent)
where
  rowToAgent (row : Quarry.Row) : Option AgentMail.Agent := do
    let id ← row.get? 0 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let projectId ← row.get? 1 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let name ← row.get? 2 >>= fun v => match v with | .text s => some s | _ => none
    let program ← row.get? 3 >>= fun v => match v with | .text s => some s | _ => none
    let model ← row.get? 4 >>= fun v => match v with | .text s => some s | _ => none
    let taskDescription ← row.get? 5 >>= fun v => match v with | .text s => some s | _ => none
    let contactPolicyStr ← row.get? 6 >>= fun v => match v with | .text s => some s | _ => none
    let attachmentsPolicyStr ← row.get? 7 >>= fun v => match v with | .text s => some s | _ => none
    let inceptionTs ← row.get? 8 >>= fun v => match v with | .integer n => some n | _ => none
    let lastActiveTs ← row.get? 9 >>= fun v => match v with | .integer n => some n | _ => none
    let contactPolicy := AgentMail.ContactPolicy.fromString? contactPolicyStr |>.getD .auto
    let attachmentsPolicy := AgentMail.AttachmentsPolicy.fromString? attachmentsPolicyStr |>.getD .auto
    some {
      id, projectId, name, program, model, taskDescription,
      contactPolicy, attachmentsPolicy,
      inceptionTs := Chronos.Timestamp.fromSeconds inceptionTs,
      lastActiveTs := Chronos.Timestamp.fromSeconds lastActiveTs
    }

/-- Query an agent by its ID -/
def queryAgentById (db : Database) (id : Nat) : IO (Option AgentMail.Agent) := do
  let row ← db.queryOne s!"SELECT id, project_id, name, program, model, task_description, contact_policy, attachments_policy, inception_ts, last_active_ts FROM agents WHERE id = {id}"
  pure (row.bind rowToAgent)
where
  rowToAgent (row : Quarry.Row) : Option AgentMail.Agent := do
    let id ← row.get? 0 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let projectId ← row.get? 1 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let name ← row.get? 2 >>= fun v => match v with | .text s => some s | _ => none
    let program ← row.get? 3 >>= fun v => match v with | .text s => some s | _ => none
    let model ← row.get? 4 >>= fun v => match v with | .text s => some s | _ => none
    let taskDescription ← row.get? 5 >>= fun v => match v with | .text s => some s | _ => none
    let contactPolicyStr ← row.get? 6 >>= fun v => match v with | .text s => some s | _ => none
    let attachmentsPolicyStr ← row.get? 7 >>= fun v => match v with | .text s => some s | _ => none
    let inceptionTs ← row.get? 8 >>= fun v => match v with | .integer n => some n | _ => none
    let lastActiveTs ← row.get? 9 >>= fun v => match v with | .integer n => some n | _ => none
    let contactPolicy := AgentMail.ContactPolicy.fromString? contactPolicyStr |>.getD .auto
    let attachmentsPolicy := AgentMail.AttachmentsPolicy.fromString? attachmentsPolicyStr |>.getD .auto
    some {
      id, projectId, name, program, model, taskDescription,
      contactPolicy, attachmentsPolicy,
      inceptionTs := Chronos.Timestamp.fromSeconds inceptionTs,
      lastActiveTs := Chronos.Timestamp.fromSeconds lastActiveTs
    }

/-- Insert a new agent and return its ID -/
def insertAgent (db : Database) (agent : AgentMail.Agent) : IO Nat := do
  let nameEsc := agent.name.replace "'" "''"
  let programEsc := agent.program.replace "'" "''"
  let modelEsc := agent.model.replace "'" "''"
  let taskEsc := agent.taskDescription.replace "'" "''"
  let contactStr := agent.contactPolicy.toString
  let attachStr := agent.attachmentsPolicy.toString
  let id ← db.insert s!"INSERT INTO agents (project_id, name, program, model, task_description, contact_policy, attachments_policy, inception_ts, last_active_ts) VALUES ({agent.projectId}, '{nameEsc}', '{programEsc}', '{modelEsc}', '{taskEsc}', '{contactStr}', '{attachStr}', {agent.inceptionTs.seconds}, {agent.lastActiveTs.seconds})"
  pure id.toNat

/-- Update an agent's last_active_ts -/
def updateAgentLastActive (db : Database) (agentId : Nat) (ts : Chronos.Timestamp) : IO Unit := do
  let _ ← db.modify s!"UPDATE agents SET last_active_ts = {ts.seconds} WHERE id = {agentId}"
  pure ()

/-- Update agent profile fields and last_active_ts -/
def updateAgentProfile (db : Database) (agent : AgentMail.Agent) : IO Unit := do
  let programEsc := agent.program.replace "'" "''"
  let modelEsc := agent.model.replace "'" "''"
  let taskEsc := agent.taskDescription.replace "'" "''"
  let attachStr := agent.attachmentsPolicy.toString
  let _ ← db.modify s!"UPDATE agents SET program = '{programEsc}', model = '{modelEsc}', task_description = '{taskEsc}', attachments_policy = '{attachStr}', last_active_ts = {agent.lastActiveTs.seconds} WHERE id = {agent.id}"
  pure ()

end Database

end AgentMail.Storage
