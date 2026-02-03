/-
  AgentMail.Storage.Database - SQLite database connection and schema
-/
import Quarry
import Lean.Data.Json
import Chronos
import AgentMail.Models.Project
import AgentMail.Models.Agent
import AgentMail.Models.Message
import AgentMail.Models.Types
import AgentMail.Models.ContactRequest
import AgentMail.Models.Contact
import AgentMail.Models.FileReservation
import AgentMail.Models.BuildSlot
import AgentMail.Models.Product

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
    attachments TEXT DEFAULT '[]',
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

  -- Build slots table
  "CREATE TABLE IF NOT EXISTS build_slots (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    project_id INTEGER NOT NULL REFERENCES projects(id),
    agent_id INTEGER NOT NULL REFERENCES agents(id),
    slot_name TEXT NOT NULL,
    created_ts INTEGER NOT NULL,
    expires_ts INTEGER NOT NULL,
    released_ts INTEGER
  )",

  -- Contact requests table
  "CREATE TABLE IF NOT EXISTS contact_requests (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    project_id INTEGER NOT NULL REFERENCES projects(id),
    from_agent_id INTEGER NOT NULL REFERENCES agents(id),
    to_agent_id INTEGER NOT NULL REFERENCES agents(id),
    message TEXT DEFAULT '',
    status TEXT DEFAULT 'pending',
    created_ts INTEGER NOT NULL,
    responded_at INTEGER,
    UNIQUE(project_id, from_agent_id, to_agent_id)
  )",

  -- Contacts table (bidirectional relationships)
  "CREATE TABLE IF NOT EXISTS contacts (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    project_id INTEGER NOT NULL REFERENCES projects(id),
    agent_id_1 INTEGER NOT NULL REFERENCES agents(id),
    agent_id_2 INTEGER NOT NULL REFERENCES agents(id),
    created_ts INTEGER NOT NULL,
    UNIQUE(project_id, agent_id_1, agent_id_2)
  )",

  -- Products table (for cross-project namespaces)
  "CREATE TABLE IF NOT EXISTS products (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    product_id TEXT NOT NULL UNIQUE,
    created_at INTEGER NOT NULL
  )",

  -- Product-project links
  "CREATE TABLE IF NOT EXISTS product_projects (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    product_db_id INTEGER NOT NULL REFERENCES products(id),
    project_id INTEGER NOT NULL REFERENCES projects(id),
    linked_at INTEGER NOT NULL,
    UNIQUE(product_db_id, project_id)
  )",

  -- Indexes for performance
  "CREATE INDEX IF NOT EXISTS idx_agents_project ON agents(project_id)",
  "CREATE INDEX IF NOT EXISTS idx_messages_project ON messages(project_id)",
  "CREATE INDEX IF NOT EXISTS idx_messages_sender ON messages(sender_id)",
  "CREATE INDEX IF NOT EXISTS idx_messages_thread ON messages(thread_id)",
  "CREATE INDEX IF NOT EXISTS idx_recipients_agent ON message_recipients(agent_id)",
  "CREATE INDEX IF NOT EXISTS idx_reservations_project ON file_reservations(project_id)",
  "CREATE INDEX IF NOT EXISTS idx_reservations_agent ON file_reservations(agent_id)",
  "CREATE INDEX IF NOT EXISTS idx_reservations_active ON file_reservations(expires_ts, released_ts)",
  "CREATE INDEX IF NOT EXISTS idx_build_slots_active ON build_slots(project_id, slot_name, expires_ts)",
  "CREATE INDEX IF NOT EXISTS idx_build_slots_agent ON build_slots(agent_id)",
  "CREATE INDEX IF NOT EXISTS idx_contact_requests_to ON contact_requests(to_agent_id)",
  "CREATE INDEX IF NOT EXISTS idx_contact_requests_from ON contact_requests(from_agent_id)",
  "CREATE INDEX IF NOT EXISTS idx_contacts_agent1 ON contacts(agent_id_1)",
  "CREATE INDEX IF NOT EXISTS idx_contacts_agent2 ON contacts(agent_id_2)",
  "CREATE INDEX IF NOT EXISTS idx_product_projects_product ON product_projects(product_db_id)",
  "CREATE INDEX IF NOT EXISTS idx_product_projects_project ON product_projects(project_id)"
]

/-- Database connection wrapper -/
structure Database where
  conn : Quarry.Database
  path : String

namespace Database

/-- Ensure the messages table includes an attachments column. -/
def ensureMessageAttachmentsColumn (db : Database) : IO Unit := do
  let rows ← db.conn.query "PRAGMA table_info(messages)"
  let mut hasAttachments := false
  for row in rows do
    match row.get? 1 with
    | some (Quarry.Value.text name) =>
      if name == "attachments" then
        hasAttachments := true
    | _ => pure ()
  if !hasAttachments then
    db.conn.execSqlDdl "ALTER TABLE messages ADD COLUMN attachments TEXT DEFAULT '[]'"

/-- Ensure build slot uniqueness for active (unreleased) slots. -/
def ensureBuildSlotUniqueIndex (db : Database) : IO Unit := do
  let now ← Chronos.Timestamp.now
  -- Release older duplicate active slots, keeping the newest id per (project_id, slot_name)
  let _ ← db.conn.execSqlModify s!"
    UPDATE build_slots
    SET released_ts = {now.seconds}
    WHERE released_ts IS NULL
      AND id NOT IN (
        SELECT MAX(id)
        FROM build_slots
        WHERE released_ts IS NULL
        GROUP BY project_id, slot_name
      )
  "
  db.conn.execSqlDdl "CREATE UNIQUE INDEX IF NOT EXISTS idx_build_slots_unreleased ON build_slots(project_id, slot_name) WHERE released_ts IS NULL"

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
  let db : Database := { conn, path }
  ensureMessageAttachmentsColumn db
  ensureBuildSlotUniqueIndex db
  pure db

/-- Open an in-memory database (for testing) -/
def openMemory : IO Database := do
  let conn ← Quarry.Database.openMemory
  -- Enable foreign keys (use execRaw for PRAGMA)
  conn.execRaw "PRAGMA foreign_keys=ON"
  -- Initialize schema
  for stmt in schema do
    conn.execSqlDdl stmt
  let db : Database := { conn, path := ":memory:" }
  ensureMessageAttachmentsColumn db
  ensureBuildSlotUniqueIndex db
  pure db

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

-- =============================================================================
-- Message queries
-- =============================================================================

/-- Insert a new message and return its ID -/
def insertMessage (db : Database) (msg : AgentMail.Message) : IO Nat := do
  let subjectEsc := msg.subject.replace "'" "''"
  let bodyEsc := msg.bodyMd.replace "'" "''"
  let attachmentsJson := Lean.Json.compress (Lean.toJson msg.attachments)
  let attachmentsEsc := attachmentsJson.replace "'" "''"
  let importanceStr := msg.importance.toString
  let ackRequired := if msg.ackRequired then 1 else 0
  let threadIdSql := match msg.threadId with
    | some t => s!"'{t.replace "'" "''"}'"
    | none => "NULL"
  let id ← db.insert s!"INSERT INTO messages (project_id, sender_id, subject, body_md, attachments, importance, ack_required, thread_id, created_ts) VALUES ({msg.projectId}, {msg.senderId}, '{subjectEsc}', '{bodyEsc}', '{attachmentsEsc}', '{importanceStr}', {ackRequired}, {threadIdSql}, {msg.createdTs.seconds})"
  pure id.toNat

/-- Insert a message recipient -/
def insertMessageRecipient (db : Database) (rec : AgentMail.MessageRecipient) : IO Unit := do
  let typeStr := rec.recipientType.toString
  let _ ← db.insert s!"INSERT INTO message_recipients (message_id, agent_id, recipient_type) VALUES ({rec.messageId}, {rec.agentId}, '{typeStr}')"
  pure ()

/-- Query a message by its ID -/
def queryMessageById (db : Database) (id : Nat) : IO (Option AgentMail.Message) := do
  let row ← db.queryOne s!"SELECT id, project_id, sender_id, subject, body_md, attachments, importance, ack_required, thread_id, created_ts FROM messages WHERE id = {id}"
  pure (row.bind rowToMessage)
where
  rowToMessage (row : Quarry.Row) : Option AgentMail.Message := do
    let id ← row.get? 0 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let projectId ← row.get? 1 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let senderId ← row.get? 2 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let subject ← row.get? 3 >>= fun v => match v with | .text s => some s | _ => none
    let bodyMd ← row.get? 4 >>= fun v => match v with | .text s => some s | _ => none
    let attachmentsRaw ← row.get? 5 >>= fun v => match v with | .text s => some s | _ => none
    let importanceStr ← row.get? 6 >>= fun v => match v with | .text s => some s | _ => none
    let ackRequiredInt ← row.get? 7 >>= fun v => match v with | .integer n => some n | _ => none
    let threadId : Option String := row.get? 8 >>= fun v => match v with | .text s => some s | _ => none
    let createdTs ← row.get? 9 >>= fun v => match v with | .integer n => some n | _ => none
    let importance := AgentMail.Importance.fromString? importanceStr |>.getD .normal
    let attachments :=
      match Lean.Json.parse attachmentsRaw with
      | Except.ok json =>
        match json.getArr? with
        | Except.ok arr => arr.filterMap (fun v => v.getStr?.toOption)
        | Except.error _ => #[]
      | Except.error _ => #[]
    some {
      id, projectId, senderId, subject, bodyMd, importance,
      ackRequired := ackRequiredInt != 0,
      threadId,
      attachments,
      createdTs := Chronos.Timestamp.fromSeconds createdTs
    }

/-- Inbox entry for query results -/
structure InboxEntry where
  id : Nat
  senderName : String
  subject : String
  importance : AgentMail.Importance
  ackRequired : Bool
  threadId : Option String
  createdTs : Chronos.Timestamp
  readAt : Option Chronos.Timestamp
  ackedAt : Option Chronos.Timestamp
  bodyMd : Option String
  recipientType : AgentMail.RecipientType
  deriving Repr

instance : Inhabited InboxEntry where
  default := {
    id := 0
    senderName := ""
    subject := ""
    importance := .normal
    ackRequired := false
    threadId := none
    createdTs := Chronos.Timestamp.fromSeconds 0
    readAt := none
    ackedAt := none
    bodyMd := none
    recipientType := .toRecipient
  }

/-- Query inbox messages for an agent -/
def queryInbox (db : Database) (projectId agentId : Nat) (limit : Nat) (urgentOnly : Bool) (sinceTsOpt : Option Int) : IO (Array InboxEntry) := do
  let urgentClause := if urgentOnly then " AND m.importance IN ('high', 'urgent')" else ""
  let sinceClause := match sinceTsOpt with
    | some ts => s!" AND m.created_ts > {ts}"
    | none => ""
  let sql := s!"SELECT m.id, a.name, m.subject, m.importance, m.ack_required, m.thread_id, m.created_ts, m.body_md, r.read_at, r.acked_at, r.recipient_type FROM messages m JOIN message_recipients r ON m.id = r.message_id JOIN agents a ON m.sender_id = a.id WHERE m.project_id = {projectId} AND r.agent_id = {agentId}{urgentClause}{sinceClause} ORDER BY m.created_ts DESC LIMIT {limit}"
  let rows ← db.query sql
  pure (rows.filterMap rowToInboxEntry)
where
  rowToInboxEntry (row : Quarry.Row) : Option InboxEntry := do
    let id ← row.get? 0 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let senderName ← row.get? 1 >>= fun v => match v with | .text s => some s | _ => none
    let subject ← row.get? 2 >>= fun v => match v with | .text s => some s | _ => none
    let importanceStr ← row.get? 3 >>= fun v => match v with | .text s => some s | _ => none
    let ackRequiredInt ← row.get? 4 >>= fun v => match v with | .integer n => some n | _ => none
    let threadId : Option String := row.get? 5 >>= fun v => match v with | .text s => some s | _ => none
    let createdTs ← row.get? 6 >>= fun v => match v with | .integer n => some n | _ => none
    let bodyMd : Option String := row.get? 7 >>= fun v => match v with | .text s => some s | _ => none
    let readAt : Option Int := row.get? 8 >>= fun v => match v with | .integer n => some n | _ => none
    let ackedAt : Option Int := row.get? 9 >>= fun v => match v with | .integer n => some n | _ => none
    let recipientTypeStr ← row.get? 10 >>= fun v => match v with | .text s => some s | _ => none
    let importance := AgentMail.Importance.fromString? importanceStr |>.getD .normal
    let recipientType := AgentMail.RecipientType.fromString? recipientTypeStr |>.getD .toRecipient
    some {
      id, senderName, subject, importance,
      ackRequired := ackRequiredInt != 0,
      threadId,
      createdTs := Chronos.Timestamp.fromSeconds createdTs,
      readAt := readAt.map Chronos.Timestamp.fromSeconds,
      ackedAt := ackedAt.map Chronos.Timestamp.fromSeconds,
      bodyMd,
      recipientType
    }

/-- Update read_at timestamp for a message recipient -/
def updateMessageReadAt (db : Database) (messageId agentId : Nat) (ts : Chronos.Timestamp) : IO Bool := do
  let affected ← db.modify s!"UPDATE message_recipients SET read_at = {ts.seconds} WHERE message_id = {messageId} AND agent_id = {agentId} AND read_at IS NULL"
  pure (affected > 0)

/-- Update acked_at timestamp for a message recipient -/
def updateMessageAckedAt (db : Database) (messageId agentId : Nat) (ts : Chronos.Timestamp) : IO Bool := do
  let affected ← db.modify s!"UPDATE message_recipients SET acked_at = {ts.seconds} WHERE message_id = {messageId} AND agent_id = {agentId} AND acked_at IS NULL"
  pure (affected > 0)

/-- Query recipient status for a specific message and agent -/
def queryRecipientStatus (db : Database) (messageId agentId : Nat) : IO (Option AgentMail.MessageRecipient) := do
  let row ← db.queryOne s!"SELECT message_id, agent_id, recipient_type, read_at, acked_at FROM message_recipients WHERE message_id = {messageId} AND agent_id = {agentId}"
  pure (row.bind rowToRecipient)
where
  rowToRecipient (row : Quarry.Row) : Option AgentMail.MessageRecipient := do
    let messageId ← row.get? 0 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let agentId ← row.get? 1 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let typeStr ← row.get? 2 >>= fun v => match v with | .text s => some s | _ => none
    let readAt : Option Int := row.get? 3 >>= fun v => match v with | .integer n => some n | _ => none
    let ackedAt : Option Int := row.get? 4 >>= fun v => match v with | .integer n => some n | _ => none
    let recipientType := AgentMail.RecipientType.fromString? typeStr |>.getD .toRecipient
    some {
      messageId, agentId, recipientType,
      readAt := readAt.map Chronos.Timestamp.fromSeconds,
      ackedAt := ackedAt.map Chronos.Timestamp.fromSeconds
    }

/-- Count total messages in inbox for an agent -/
def countInbox (db : Database) (projectId agentId : Nat) : IO Nat := do
  let row ← db.queryOne s!"SELECT COUNT(*) FROM messages m JOIN message_recipients r ON m.id = r.message_id WHERE m.project_id = {projectId} AND r.agent_id = {agentId}"
  match row with
  | some r =>
    match r.get? 0 with
    | some (.integer n) => pure n.toNat
    | _ => pure 0
  | none => pure 0

-- =============================================================================
-- Contact request queries
-- =============================================================================

/-- Insert a new contact request and return its ID -/
def insertContactRequest (db : Database) (projectId fromAgentId toAgentId : Nat) (message : String) (createdTs : Chronos.Timestamp) : IO Nat := do
  let messageEsc := message.replace "'" "''"
  let id ← db.insert s!"INSERT INTO contact_requests (project_id, from_agent_id, to_agent_id, message, status, created_ts) VALUES ({projectId}, {fromAgentId}, {toAgentId}, '{messageEsc}', 'pending', {createdTs.seconds})"
  pure id.toNat

/-- Query a contact request by ID -/
def queryContactRequestById (db : Database) (id : Nat) : IO (Option AgentMail.ContactRequest) := do
  let row ← db.queryOne s!"SELECT id, project_id, from_agent_id, to_agent_id, message, status, created_ts, responded_at FROM contact_requests WHERE id = {id}"
  pure (row.bind rowToContactRequest)
where
  rowToContactRequest (row : Quarry.Row) : Option AgentMail.ContactRequest := do
    let id ← row.get? 0 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let projectId ← row.get? 1 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let fromAgentId ← row.get? 2 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let toAgentId ← row.get? 3 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let message ← row.get? 4 >>= fun v => match v with | .text s => some s | _ => none
    let statusStr ← row.get? 5 >>= fun v => match v with | .text s => some s | _ => none
    let createdTs ← row.get? 6 >>= fun v => match v with | .integer n => some n | _ => none
    let respondedAt : Option Int := row.get? 7 >>= fun v => match v with | .integer n => some n | _ => none
    let status := AgentMail.ContactRequestStatus.fromString? statusStr |>.getD .pending
    some {
      id, projectId, fromAgentId, toAgentId, message, status
      createdTs := Chronos.Timestamp.fromSeconds createdTs
      respondedAt := respondedAt.map Chronos.Timestamp.fromSeconds
    }

/-- Query existing contact request between two agents -/
def queryContactRequestBetween (db : Database) (projectId fromAgentId toAgentId : Nat) : IO (Option AgentMail.ContactRequest) := do
  let row ← db.queryOne s!"SELECT id, project_id, from_agent_id, to_agent_id, message, status, created_ts, responded_at FROM contact_requests WHERE project_id = {projectId} AND from_agent_id = {fromAgentId} AND to_agent_id = {toAgentId}"
  pure (row.bind rowToContactRequest)
where
  rowToContactRequest (row : Quarry.Row) : Option AgentMail.ContactRequest := do
    let id ← row.get? 0 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let projectId ← row.get? 1 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let fromAgentId ← row.get? 2 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let toAgentId ← row.get? 3 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let message ← row.get? 4 >>= fun v => match v with | .text s => some s | _ => none
    let statusStr ← row.get? 5 >>= fun v => match v with | .text s => some s | _ => none
    let createdTs ← row.get? 6 >>= fun v => match v with | .integer n => some n | _ => none
    let respondedAt : Option Int := row.get? 7 >>= fun v => match v with | .integer n => some n | _ => none
    let status := AgentMail.ContactRequestStatus.fromString? statusStr |>.getD .pending
    some {
      id, projectId, fromAgentId, toAgentId, message, status
      createdTs := Chronos.Timestamp.fromSeconds createdTs
      respondedAt := respondedAt.map Chronos.Timestamp.fromSeconds
    }

/-- Query pending contact requests for an agent (where they are the recipient) -/
def queryPendingContactRequests (db : Database) (projectId agentId : Nat) : IO (Array AgentMail.ContactRequest) := do
  let rows ← db.query s!"SELECT id, project_id, from_agent_id, to_agent_id, message, status, created_ts, responded_at FROM contact_requests WHERE project_id = {projectId} AND to_agent_id = {agentId} AND status = 'pending' ORDER BY created_ts DESC"
  pure (rows.filterMap rowToContactRequest)
where
  rowToContactRequest (row : Quarry.Row) : Option AgentMail.ContactRequest := do
    let id ← row.get? 0 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let projectId ← row.get? 1 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let fromAgentId ← row.get? 2 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let toAgentId ← row.get? 3 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let message ← row.get? 4 >>= fun v => match v with | .text s => some s | _ => none
    let statusStr ← row.get? 5 >>= fun v => match v with | .text s => some s | _ => none
    let createdTs ← row.get? 6 >>= fun v => match v with | .integer n => some n | _ => none
    let respondedAt : Option Int := row.get? 7 >>= fun v => match v with | .integer n => some n | _ => none
    let status := AgentMail.ContactRequestStatus.fromString? statusStr |>.getD .pending
    some {
      id, projectId, fromAgentId, toAgentId, message, status
      createdTs := Chronos.Timestamp.fromSeconds createdTs
      respondedAt := respondedAt.map Chronos.Timestamp.fromSeconds
    }

/-- Update contact request status -/
def updateContactRequestStatus (db : Database) (id : Nat) (status : AgentMail.ContactRequestStatus) (respondedAt : Chronos.Timestamp) : IO Unit := do
  let statusStr := status.toString
  let _ ← db.modify s!"UPDATE contact_requests SET status = '{statusStr}', responded_at = {respondedAt.seconds} WHERE id = {id}"
  pure ()

/-- Reset an existing contact request back to pending with a new message/timestamp. -/
def resetContactRequestToPending (db : Database) (id : Nat) (message : String) (createdTs : Chronos.Timestamp) : IO Unit := do
  let messageEsc := message.replace "'" "''"
  let _ ← db.modify s!"UPDATE contact_requests SET status = 'pending', message = '{messageEsc}', created_ts = {createdTs.seconds}, responded_at = NULL WHERE id = {id}"
  pure ()

-- =============================================================================
-- Contact queries
-- =============================================================================

/-- Insert a new contact and return its ID (ensures agentId1 < agentId2) -/
def insertContact (db : Database) (projectId agent1 agent2 : Nat) (createdTs : Chronos.Timestamp) : IO Nat := do
  let (a1, a2) := if agent1 < agent2 then (agent1, agent2) else (agent2, agent1)
  let id ← db.insert s!"INSERT INTO contacts (project_id, agent_id_1, agent_id_2, created_ts) VALUES ({projectId}, {a1}, {a2}, {createdTs.seconds})"
  pure id.toNat

/-- Query contact between two agents -/
def queryContactBetween (db : Database) (projectId agent1 agent2 : Nat) : IO (Option AgentMail.Contact) := do
  let (a1, a2) := if agent1 < agent2 then (agent1, agent2) else (agent2, agent1)
  let row ← db.queryOne s!"SELECT id, project_id, agent_id_1, agent_id_2, created_ts FROM contacts WHERE project_id = {projectId} AND agent_id_1 = {a1} AND agent_id_2 = {a2}"
  pure (row.bind rowToContact)
where
  rowToContact (row : Quarry.Row) : Option AgentMail.Contact := do
    let id ← row.get? 0 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let projectId ← row.get? 1 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let agentId1 ← row.get? 2 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let agentId2 ← row.get? 3 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let createdTs ← row.get? 4 >>= fun v => match v with | .integer n => some n | _ => none
    some { id, projectId, agentId1, agentId2, createdTs := Chronos.Timestamp.fromSeconds createdTs }

/-- Contact entry with agent name for query results -/
structure ContactEntry where
  contactId : Nat
  agentId : Nat
  agentName : String
  sinceTs : Chronos.Timestamp
  deriving Repr

/-- Query all contacts for an agent -/
def queryContacts (db : Database) (projectId agentId : Nat) : IO (Array ContactEntry) := do
  -- Query contacts where agent is either agent_id_1 or agent_id_2
  let sql := s!"SELECT c.id, CASE WHEN c.agent_id_1 = {agentId} THEN c.agent_id_2 ELSE c.agent_id_1 END as other_agent_id, a.name, c.created_ts FROM contacts c JOIN agents a ON a.id = CASE WHEN c.agent_id_1 = {agentId} THEN c.agent_id_2 ELSE c.agent_id_1 END WHERE c.project_id = {projectId} AND (c.agent_id_1 = {agentId} OR c.agent_id_2 = {agentId}) ORDER BY c.created_ts DESC"
  let rows ← db.query sql
  pure (rows.filterMap rowToContactEntry)
where
  rowToContactEntry (row : Quarry.Row) : Option ContactEntry := do
    let contactId ← row.get? 0 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let agentId ← row.get? 1 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let agentName ← row.get? 2 >>= fun v => match v with | .text s => some s | _ => none
    let sinceTs ← row.get? 3 >>= fun v => match v with | .integer n => some n | _ => none
    some { contactId, agentId, agentName, sinceTs := Chronos.Timestamp.fromSeconds sinceTs }

/-- Update an agent's contact policy -/
def updateAgentContactPolicy (db : Database) (agentId : Nat) (policy : AgentMail.ContactPolicy) : IO Unit := do
  let policyStr := policy.toString
  let _ ← db.modify s!"UPDATE agents SET contact_policy = '{policyStr}' WHERE id = {agentId}"
  pure ()

-- =============================================================================
-- File reservation queries
-- =============================================================================

/-- Insert a new file reservation and return its ID -/
def insertFileReservation (db : Database) (projectId agentId : Nat) (pathPattern : String)
    (exclusive : Bool) (reason : String) (createdTs expiresTs : Chronos.Timestamp) : IO Nat := do
  let pathEsc := pathPattern.replace "'" "''"
  let reasonEsc := reason.replace "'" "''"
  let exclusiveInt := if exclusive then 1 else 0
  let id ← db.insert s!"INSERT INTO file_reservations (project_id, agent_id, path_pattern, exclusive, reason, created_ts, expires_ts) VALUES ({projectId}, {agentId}, '{pathEsc}', {exclusiveInt}, '{reasonEsc}', {createdTs.seconds}, {expiresTs.seconds})"
  pure id.toNat

/-- Query a file reservation by ID -/
def queryFileReservationById (db : Database) (id : Nat) : IO (Option AgentMail.FileReservation) := do
  let row ← db.queryOne s!"SELECT id, project_id, agent_id, path_pattern, exclusive, reason, created_ts, expires_ts, released_ts FROM file_reservations WHERE id = {id}"
  pure (row.bind rowToFileReservation)
where
  rowToFileReservation (row : Quarry.Row) : Option AgentMail.FileReservation := do
    let id ← row.get? 0 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let projectId ← row.get? 1 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let agentId ← row.get? 2 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let pathPattern ← row.get? 3 >>= fun v => match v with | .text s => some s | _ => none
    let exclusiveInt ← row.get? 4 >>= fun v => match v with | .integer n => some n | _ => none
    let reason ← row.get? 5 >>= fun v => match v with | .text s => some s | _ => none
    let createdTs ← row.get? 6 >>= fun v => match v with | .integer n => some n | _ => none
    let expiresTs ← row.get? 7 >>= fun v => match v with | .integer n => some n | _ => none
    let releasedTs : Option Int := row.get? 8 >>= fun v => match v with | .integer n => some n | _ => none
    some {
      id, projectId, agentId, pathPattern
      exclusive := exclusiveInt != 0
      reason
      createdTs := Chronos.Timestamp.fromSeconds createdTs
      expiresTs := Chronos.Timestamp.fromSeconds expiresTs
      releasedTs := releasedTs.map Chronos.Timestamp.fromSeconds
    }

/-- Query active (non-expired, non-released) reservations for a project -/
def queryActiveFileReservations (db : Database) (projectId : Nat) (now : Chronos.Timestamp) : IO (Array AgentMail.FileReservation) := do
  let rows ← db.query s!"SELECT id, project_id, agent_id, path_pattern, exclusive, reason, created_ts, expires_ts, released_ts FROM file_reservations WHERE project_id = {projectId} AND expires_ts > {now.seconds} AND released_ts IS NULL"
  pure (rows.filterMap rowToFileReservation)
where
  rowToFileReservation (row : Quarry.Row) : Option AgentMail.FileReservation := do
    let id ← row.get? 0 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let projectId ← row.get? 1 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let agentId ← row.get? 2 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let pathPattern ← row.get? 3 >>= fun v => match v with | .text s => some s | _ => none
    let exclusiveInt ← row.get? 4 >>= fun v => match v with | .integer n => some n | _ => none
    let reason ← row.get? 5 >>= fun v => match v with | .text s => some s | _ => none
    let createdTs ← row.get? 6 >>= fun v => match v with | .integer n => some n | _ => none
    let expiresTs ← row.get? 7 >>= fun v => match v with | .integer n => some n | _ => none
    let releasedTs : Option Int := row.get? 8 >>= fun v => match v with | .integer n => some n | _ => none
    some {
      id, projectId, agentId, pathPattern
      exclusive := exclusiveInt != 0
      reason
      createdTs := Chronos.Timestamp.fromSeconds createdTs
      expiresTs := Chronos.Timestamp.fromSeconds expiresTs
      releasedTs := releasedTs.map Chronos.Timestamp.fromSeconds
    }

/-- Query reservations by agent (including expired/released for history) -/
def queryFileReservationsByAgent (db : Database) (projectId agentId : Nat) : IO (Array AgentMail.FileReservation) := do
  let rows ← db.query s!"SELECT id, project_id, agent_id, path_pattern, exclusive, reason, created_ts, expires_ts, released_ts FROM file_reservations WHERE project_id = {projectId} AND agent_id = {agentId} ORDER BY created_ts DESC"
  pure (rows.filterMap rowToFileReservation)
where
  rowToFileReservation (row : Quarry.Row) : Option AgentMail.FileReservation := do
    let id ← row.get? 0 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let projectId ← row.get? 1 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let agentId ← row.get? 2 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let pathPattern ← row.get? 3 >>= fun v => match v with | .text s => some s | _ => none
    let exclusiveInt ← row.get? 4 >>= fun v => match v with | .integer n => some n | _ => none
    let reason ← row.get? 5 >>= fun v => match v with | .text s => some s | _ => none
    let createdTs ← row.get? 6 >>= fun v => match v with | .integer n => some n | _ => none
    let expiresTs ← row.get? 7 >>= fun v => match v with | .integer n => some n | _ => none
    let releasedTs : Option Int := row.get? 8 >>= fun v => match v with | .integer n => some n | _ => none
    some {
      id, projectId, agentId, pathPattern
      exclusive := exclusiveInt != 0
      reason
      createdTs := Chronos.Timestamp.fromSeconds createdTs
      expiresTs := Chronos.Timestamp.fromSeconds expiresTs
      releasedTs := releasedTs.map Chronos.Timestamp.fromSeconds
    }

/-- Query active reservations for a specific agent -/
def queryActiveFileReservationsByAgent (db : Database) (projectId agentId : Nat) (now : Chronos.Timestamp) : IO (Array AgentMail.FileReservation) := do
  let rows ← db.query s!"SELECT id, project_id, agent_id, path_pattern, exclusive, reason, created_ts, expires_ts, released_ts FROM file_reservations WHERE project_id = {projectId} AND agent_id = {agentId} AND expires_ts > {now.seconds} AND released_ts IS NULL ORDER BY created_ts DESC"
  pure (rows.filterMap rowToFileReservation)
where
  rowToFileReservation (row : Quarry.Row) : Option AgentMail.FileReservation := do
    let id ← row.get? 0 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let projectId ← row.get? 1 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let agentId ← row.get? 2 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let pathPattern ← row.get? 3 >>= fun v => match v with | .text s => some s | _ => none
    let exclusiveInt ← row.get? 4 >>= fun v => match v with | .integer n => some n | _ => none
    let reason ← row.get? 5 >>= fun v => match v with | .text s => some s | _ => none
    let createdTs ← row.get? 6 >>= fun v => match v with | .integer n => some n | _ => none
    let expiresTs ← row.get? 7 >>= fun v => match v with | .integer n => some n | _ => none
    let releasedTs : Option Int := row.get? 8 >>= fun v => match v with | .integer n => some n | _ => none
    some {
      id, projectId, agentId, pathPattern
      exclusive := exclusiveInt != 0
      reason
      createdTs := Chronos.Timestamp.fromSeconds createdTs
      expiresTs := Chronos.Timestamp.fromSeconds expiresTs
      releasedTs := releasedTs.map Chronos.Timestamp.fromSeconds
    }

/-- Mark reservation as released -/
def updateFileReservationReleased (db : Database) (id : Nat) (releasedTs : Chronos.Timestamp) : IO Bool := do
  let affected ← db.modify s!"UPDATE file_reservations SET released_ts = {releasedTs.seconds} WHERE id = {id} AND released_ts IS NULL"
  pure (affected > 0)

/-- Extend reservation TTL -/
def updateFileReservationExpires (db : Database) (id : Nat) (expiresTs : Chronos.Timestamp) : IO Bool := do
  let affected ← db.modify s!"UPDATE file_reservations SET expires_ts = {expiresTs.seconds} WHERE id = {id} AND released_ts IS NULL"
  pure (affected > 0)

/-- Query active reservations matching a path pattern -/
def queryActiveFileReservationsByPath (db : Database) (projectId : Nat) (pathPattern : String) (now : Chronos.Timestamp) : IO (Array AgentMail.FileReservation) := do
  let pathEsc := pathPattern.replace "'" "''"
  let rows ← db.query s!"SELECT id, project_id, agent_id, path_pattern, exclusive, reason, created_ts, expires_ts, released_ts FROM file_reservations WHERE project_id = {projectId} AND path_pattern = '{pathEsc}' AND expires_ts > {now.seconds} AND released_ts IS NULL"
  pure (rows.filterMap rowToFileReservation)
where
  rowToFileReservation (row : Quarry.Row) : Option AgentMail.FileReservation := do
    let id ← row.get? 0 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let projectId ← row.get? 1 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let agentId ← row.get? 2 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let pathPattern ← row.get? 3 >>= fun v => match v with | .text s => some s | _ => none
    let exclusiveInt ← row.get? 4 >>= fun v => match v with | .integer n => some n | _ => none
    let reason ← row.get? 5 >>= fun v => match v with | .text s => some s | _ => none
    let createdTs ← row.get? 6 >>= fun v => match v with | .integer n => some n | _ => none
    let expiresTs ← row.get? 7 >>= fun v => match v with | .integer n => some n | _ => none
    let releasedTs : Option Int := row.get? 8 >>= fun v => match v with | .integer n => some n | _ => none
    some {
      id, projectId, agentId, pathPattern
      exclusive := exclusiveInt != 0
      reason
      createdTs := Chronos.Timestamp.fromSeconds createdTs
      expiresTs := Chronos.Timestamp.fromSeconds expiresTs
      releasedTs := releasedTs.map Chronos.Timestamp.fromSeconds
    }

-- =============================================================================
-- Search queries
-- =============================================================================

/-- Message with sender name for search results -/
structure MessageWithSender where
  id : Nat
  subject : String
  bodyMd : String
  importance : AgentMail.Importance
  ackRequired : Bool
  threadId : Option String
  createdTs : Chronos.Timestamp
  senderName : String
  deriving Repr, Inhabited

/-- Thread summary for UI listings. -/
structure ThreadSummary where
  threadId : String
  messageCount : Nat
  lastMessageId : Nat
  lastSubject : String
  lastSenderName : String
  lastImportance : AgentMail.Importance
  lastAckRequired : Bool
  lastCreatedTs : Chronos.Timestamp
  lastBodyMd : Option String
  unreadCount : Option Nat
  deriving Repr

instance : Inhabited ThreadSummary where
  default := {
    threadId := ""
    messageCount := 0
    lastMessageId := 0
    lastSubject := ""
    lastSenderName := ""
    lastImportance := .normal
    lastAckRequired := false
    lastCreatedTs := Chronos.Timestamp.fromSeconds 0
    lastBodyMd := none
    unreadCount := none
  }

/-- Search result entry -/
structure SearchResult where
  id : Nat
  subject : String
  importance : AgentMail.Importance
  ackRequired : Bool
  createdTs : Chronos.Timestamp
  threadId : Option String
  senderName : String
  deriving Repr, Inhabited

/-- Query all messages in a thread with sender names -/
def queryMessagesByThread (db : Database) (projectId : Nat) (threadId : String) (limit : Nat) : IO (Array MessageWithSender) := do
  let threadEsc := threadId.replace "'" "''"
  -- Pull the most recent messages, then return them in chronological order.
  let sql := s!"SELECT m.id, m.subject, m.body_md, m.importance, m.ack_required, m.thread_id, m.created_ts, a.name FROM messages m JOIN agents a ON m.sender_id = a.id WHERE m.project_id = {projectId} AND m.thread_id = '{threadEsc}' ORDER BY m.created_ts DESC LIMIT {limit}"
  let rows ← db.query sql
  pure (rows.filterMap rowToMessage |>.reverse)
where
  rowToMessage (row : Quarry.Row) : Option MessageWithSender := do
    let id ← row.get? 0 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let subject ← row.get? 1 >>= fun v => match v with | .text s => some s | _ => none
    let bodyMd ← row.get? 2 >>= fun v => match v with | .text s => some s | _ => none
    let importanceStr ← row.get? 3 >>= fun v => match v with | .text s => some s | _ => none
    let ackRequiredInt ← row.get? 4 >>= fun v => match v with | .integer n => some n | _ => none
    let threadId : Option String := row.get? 5 >>= fun v => match v with | .text s => some s | _ => none
    let createdTs ← row.get? 6 >>= fun v => match v with | .integer n => some n | _ => none
    let senderName ← row.get? 7 >>= fun v => match v with | .text s => some s | _ => none
    let importance := AgentMail.Importance.fromString? importanceStr |>.getD .normal
    some {
      id, subject, bodyMd, importance
      ackRequired := ackRequiredInt != 0
      threadId
      createdTs := Chronos.Timestamp.fromSeconds createdTs
      senderName
    }

/-- Query thread summaries for a project (optionally scoped to an agent). -/
def queryThreadSummaries (db : Database) (projectId : Nat) (limit : Nat) (agentIdOpt : Option Nat := none) : IO (Array ThreadSummary) := do
  let agentClause := match agentIdOpt with
    | some agentId =>
      s!" AND (m.sender_id = {agentId} OR EXISTS (SELECT 1 FROM message_recipients r WHERE r.message_id = m.id AND r.agent_id = {agentId}))"
    | none => ""
  let unreadSelect := match agentIdOpt with
    | some agentId =>
      s!"(SELECT COUNT(*) FROM messages mx JOIN message_recipients r ON mx.id = r.message_id WHERE mx.project_id = m.project_id AND mx.thread_id = m.thread_id AND r.agent_id = {agentId} AND r.read_at IS NULL) AS unread_count"
    | none => "NULL AS unread_count"
  let sql :=
    s!"SELECT m.thread_id,
              COUNT(*) AS message_count,
              MAX(m.created_ts) AS last_created_ts,
              (SELECT m2.id FROM messages m2 WHERE m2.project_id = m.project_id AND m2.thread_id = m.thread_id ORDER BY m2.created_ts DESC LIMIT 1) AS last_id,
              (SELECT m2.subject FROM messages m2 WHERE m2.project_id = m.project_id AND m2.thread_id = m.thread_id ORDER BY m2.created_ts DESC LIMIT 1) AS last_subject,
              (SELECT m2.body_md FROM messages m2 WHERE m2.project_id = m.project_id AND m2.thread_id = m.thread_id ORDER BY m2.created_ts DESC LIMIT 1) AS last_body,
              (SELECT m2.importance FROM messages m2 WHERE m2.project_id = m.project_id AND m2.thread_id = m.thread_id ORDER BY m2.created_ts DESC LIMIT 1) AS last_importance,
              (SELECT m2.ack_required FROM messages m2 WHERE m2.project_id = m.project_id AND m2.thread_id = m.thread_id ORDER BY m2.created_ts DESC LIMIT 1) AS last_ack_required,
              (SELECT a.name FROM messages m2 JOIN agents a ON m2.sender_id = a.id WHERE m2.project_id = m.project_id AND m2.thread_id = m.thread_id ORDER BY m2.created_ts DESC LIMIT 1) AS last_sender,
              {unreadSelect}
       FROM messages m
       WHERE m.project_id = {projectId} AND m.thread_id IS NOT NULL AND m.thread_id != ''{agentClause}
       GROUP BY m.thread_id
       ORDER BY last_created_ts DESC
       LIMIT {limit}"
  let rows ← db.query sql
  pure (rows.filterMap rowToSummary)
where
  rowToSummary (row : Quarry.Row) : Option ThreadSummary := do
    let threadId ← row.get? 0 >>= fun v => match v with | .text s => some s | _ => none
    let messageCountInt ← row.get? 1 >>= fun v => match v with | .integer n => some n | _ => none
    let lastCreatedTsInt ← row.get? 2 >>= fun v => match v with | .integer n => some n | _ => none
    let lastMessageId ← row.get? 3 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let lastSubject ← row.get? 4 >>= fun v => match v with | .text s => some s | _ => none
    let lastBodyMd : Option String := row.get? 5 >>= fun v => match v with | .text s => some s | _ => none
    let lastImportanceStr ← row.get? 6 >>= fun v => match v with | .text s => some s | _ => none
    let lastAckRequiredInt ← row.get? 7 >>= fun v => match v with | .integer n => some n | _ => none
    let lastSenderName ← row.get? 8 >>= fun v => match v with | .text s => some s | _ => none
    let unreadCount : Option Nat := row.get? 9 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let importance := AgentMail.Importance.fromString? lastImportanceStr |>.getD .normal
    some {
      threadId
      messageCount := messageCountInt.toNat
      lastMessageId
      lastSubject
      lastSenderName
      lastImportance := importance
      lastAckRequired := lastAckRequiredInt != 0
      lastCreatedTs := Chronos.Timestamp.fromSeconds lastCreatedTsInt
      lastBodyMd
      unreadCount
    }

/-- Search messages using LIKE on subject and body -/
def searchMessages (db : Database) (projectId : Nat) (query : String) (limit : Nat) (threadIdOpt : Option String) (agentIdOpt : Option Nat) : IO (Array SearchResult) := do
  let queryEsc := query.replace "\\" "\\\\" |>.replace "'" "''" |>.replace "%" "\\%" |>.replace "_" "\\_"
  let threadClause := match threadIdOpt with
    | some t => s!" AND m.thread_id = '{t.replace "'" "''"}'"
    | none => ""
  let agentClause := match agentIdOpt with
    | some agentId =>
      s!" AND (m.sender_id = {agentId} OR EXISTS (SELECT 1 FROM message_recipients r WHERE r.message_id = m.id AND r.agent_id = {agentId}))"
    | none => ""
  let sql := s!"SELECT m.id, m.subject, m.importance, m.ack_required, m.created_ts, m.thread_id, a.name FROM messages m JOIN agents a ON m.sender_id = a.id WHERE m.project_id = {projectId} AND (m.subject LIKE '%{queryEsc}%' ESCAPE '\\' OR m.body_md LIKE '%{queryEsc}%' ESCAPE '\\'){threadClause}{agentClause} ORDER BY m.created_ts DESC LIMIT {limit}"
  let rows ← db.query sql
  pure (rows.filterMap rowToResult)
where
  rowToResult (row : Quarry.Row) : Option SearchResult := do
    let id ← row.get? 0 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let subject ← row.get? 1 >>= fun v => match v with | .text s => some s | _ => none
    let importanceStr ← row.get? 2 >>= fun v => match v with | .text s => some s | _ => none
    let ackRequiredInt ← row.get? 3 >>= fun v => match v with | .integer n => some n | _ => none
    let createdTs ← row.get? 4 >>= fun v => match v with | .integer n => some n | _ => none
    let threadId : Option String := row.get? 5 >>= fun v => match v with | .text s => some s | _ => none
    let senderName ← row.get? 6 >>= fun v => match v with | .text s => some s | _ => none
    let importance := AgentMail.Importance.fromString? importanceStr |>.getD .normal
    some {
      id, subject, importance
      ackRequired := ackRequiredInt != 0
      createdTs := Chronos.Timestamp.fromSeconds createdTs
      threadId
      senderName
    }

-- =============================================================================
-- Build slot queries
-- =============================================================================

/-- Insert a new build slot and return its ID -/
def insertBuildSlot (db : Database) (projectId agentId : Nat) (slotName : String)
    (createdTs expiresTs : Chronos.Timestamp) : IO Nat := do
  let slotEsc := slotName.replace "'" "''"
  let id ← db.insert s!"INSERT INTO build_slots (project_id, agent_id, slot_name, created_ts, expires_ts) VALUES ({projectId}, {agentId}, '{slotEsc}', {createdTs.seconds}, {expiresTs.seconds})"
  pure id.toNat

/-- Query a build slot by ID -/
def queryBuildSlotById (db : Database) (id : Nat) : IO (Option AgentMail.BuildSlot) := do
  let row ← db.queryOne s!"SELECT id, project_id, agent_id, slot_name, created_ts, expires_ts, released_ts FROM build_slots WHERE id = {id}"
  pure (row.bind rowToBuildSlot)
where
  rowToBuildSlot (row : Quarry.Row) : Option AgentMail.BuildSlot := do
    let id ← row.get? 0 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let projectId ← row.get? 1 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let agentId ← row.get? 2 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let slotName ← row.get? 3 >>= fun v => match v with | .text s => some s | _ => none
    let createdTs ← row.get? 4 >>= fun v => match v with | .integer n => some n | _ => none
    let expiresTs ← row.get? 5 >>= fun v => match v with | .integer n => some n | _ => none
    let releasedTs : Option Int := row.get? 6 >>= fun v => match v with | .integer n => some n | _ => none
    some {
      id, projectId, agentId, slotName
      createdTs := Chronos.Timestamp.fromSeconds createdTs
      expiresTs := Chronos.Timestamp.fromSeconds expiresTs
      releasedTs := releasedTs.map Chronos.Timestamp.fromSeconds
    }

/-- Query active (non-expired, non-released) build slot for a project and slot name -/
def queryActiveBuildSlot (db : Database) (projectId : Nat) (slotName : String) (now : Chronos.Timestamp) : IO (Option AgentMail.BuildSlot) := do
  let slotEsc := slotName.replace "'" "''"
  let row ← db.queryOne s!"SELECT id, project_id, agent_id, slot_name, created_ts, expires_ts, released_ts FROM build_slots WHERE project_id = {projectId} AND slot_name = '{slotEsc}' AND expires_ts > {now.seconds} AND released_ts IS NULL ORDER BY created_ts DESC LIMIT 1"
  pure (row.bind rowToBuildSlot)
where
  rowToBuildSlot (row : Quarry.Row) : Option AgentMail.BuildSlot := do
    let id ← row.get? 0 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let projectId ← row.get? 1 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let agentId ← row.get? 2 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let slotName ← row.get? 3 >>= fun v => match v with | .text s => some s | _ => none
    let createdTs ← row.get? 4 >>= fun v => match v with | .integer n => some n | _ => none
    let expiresTs ← row.get? 5 >>= fun v => match v with | .integer n => some n | _ => none
    let releasedTs : Option Int := row.get? 6 >>= fun v => match v with | .integer n => some n | _ => none
    some {
      id, projectId, agentId, slotName
      createdTs := Chronos.Timestamp.fromSeconds createdTs
      expiresTs := Chronos.Timestamp.fromSeconds expiresTs
      releasedTs := releasedTs.map Chronos.Timestamp.fromSeconds
    }

/-- Query all build slots held by an agent -/
def queryBuildSlotsByAgent (db : Database) (projectId agentId : Nat) : IO (Array AgentMail.BuildSlot) := do
  let rows ← db.query s!"SELECT id, project_id, agent_id, slot_name, created_ts, expires_ts, released_ts FROM build_slots WHERE project_id = {projectId} AND agent_id = {agentId} ORDER BY created_ts DESC"
  pure (rows.filterMap rowToBuildSlot)
where
  rowToBuildSlot (row : Quarry.Row) : Option AgentMail.BuildSlot := do
    let id ← row.get? 0 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let projectId ← row.get? 1 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let agentId ← row.get? 2 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let slotName ← row.get? 3 >>= fun v => match v with | .text s => some s | _ => none
    let createdTs ← row.get? 4 >>= fun v => match v with | .integer n => some n | _ => none
    let expiresTs ← row.get? 5 >>= fun v => match v with | .integer n => some n | _ => none
    let releasedTs : Option Int := row.get? 6 >>= fun v => match v with | .integer n => some n | _ => none
    some {
      id, projectId, agentId, slotName
      createdTs := Chronos.Timestamp.fromSeconds createdTs
      expiresTs := Chronos.Timestamp.fromSeconds expiresTs
      releasedTs := releasedTs.map Chronos.Timestamp.fromSeconds
    }

/-- Query active build slots held by an agent -/
def queryActiveBuildSlotsByAgent (db : Database) (projectId agentId : Nat) (now : Chronos.Timestamp) : IO (Array AgentMail.BuildSlot) := do
  let rows ← db.query s!"SELECT id, project_id, agent_id, slot_name, created_ts, expires_ts, released_ts FROM build_slots WHERE project_id = {projectId} AND agent_id = {agentId} AND expires_ts > {now.seconds} AND released_ts IS NULL ORDER BY created_ts DESC"
  pure (rows.filterMap rowToBuildSlot)
where
  rowToBuildSlot (row : Quarry.Row) : Option AgentMail.BuildSlot := do
    let id ← row.get? 0 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let projectId ← row.get? 1 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let agentId ← row.get? 2 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let slotName ← row.get? 3 >>= fun v => match v with | .text s => some s | _ => none
    let createdTs ← row.get? 4 >>= fun v => match v with | .integer n => some n | _ => none
    let expiresTs ← row.get? 5 >>= fun v => match v with | .integer n => some n | _ => none
    let releasedTs : Option Int := row.get? 6 >>= fun v => match v with | .integer n => some n | _ => none
    some {
      id, projectId, agentId, slotName
      createdTs := Chronos.Timestamp.fromSeconds createdTs
      expiresTs := Chronos.Timestamp.fromSeconds expiresTs
      releasedTs := releasedTs.map Chronos.Timestamp.fromSeconds
    }

/-- Extend build slot TTL -/
def updateBuildSlotExpires (db : Database) (id : Nat) (expiresTs : Chronos.Timestamp) : IO Bool := do
  let affected ← db.modify s!"UPDATE build_slots SET expires_ts = {expiresTs.seconds} WHERE id = {id} AND released_ts IS NULL"
  pure (affected > 0)

/-- Mark build slot as released -/
def updateBuildSlotReleased (db : Database) (id : Nat) (releasedTs : Chronos.Timestamp) : IO Bool := do
  let affected ← db.modify s!"UPDATE build_slots SET released_ts = {releasedTs.seconds} WHERE id = {id} AND released_ts IS NULL"
  pure (affected > 0)

/-- Release expired build slots for a project + slot name. -/
def releaseExpiredBuildSlots (db : Database) (projectId : Nat) (slotName : String) (now : Chronos.Timestamp) : IO Nat := do
  let slotEsc := slotName.replace "'" "''"
  let affected ← db.modify s!"UPDATE build_slots SET released_ts = {now.seconds} WHERE project_id = {projectId} AND slot_name = '{slotEsc}' AND released_ts IS NULL AND expires_ts <= {now.seconds}"
  pure affected.toNat

-- =============================================================================
-- Product queries
-- =============================================================================

/-- Insert a new product and return its ID -/
def insertProduct (db : Database) (productId : String) (createdAt : Chronos.Timestamp) : IO Nat := do
  let productIdEsc := productId.replace "'" "''"
  let id ← db.insert s!"INSERT INTO products (product_id, created_at) VALUES ('{productIdEsc}', {createdAt.seconds})"
  pure id.toNat

/-- Query a product by its string ID -/
def queryProductByProductId (db : Database) (productId : String) : IO (Option AgentMail.Product) := do
  let productIdEsc := productId.replace "'" "''"
  let row ← db.queryOne s!"SELECT id, product_id, created_at FROM products WHERE product_id = '{productIdEsc}'"
  pure (row.bind rowToProduct)
where
  rowToProduct (row : Quarry.Row) : Option AgentMail.Product := do
    let id ← row.get? 0 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let productId ← row.get? 1 >>= fun v => match v with | .text s => some s | _ => none
    let createdAt ← row.get? 2 >>= fun v => match v with | .integer n => some n | _ => none
    some { id, productId, createdAt := Chronos.Timestamp.fromSeconds createdAt }

/-- Query a product by its database ID -/
def queryProductById (db : Database) (id : Nat) : IO (Option AgentMail.Product) := do
  let row ← db.queryOne s!"SELECT id, product_id, created_at FROM products WHERE id = {id}"
  pure (row.bind rowToProduct)
where
  rowToProduct (row : Quarry.Row) : Option AgentMail.Product := do
    let id ← row.get? 0 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let productId ← row.get? 1 >>= fun v => match v with | .text s => some s | _ => none
    let createdAt ← row.get? 2 >>= fun v => match v with | .integer n => some n | _ => none
    some { id, productId, createdAt := Chronos.Timestamp.fromSeconds createdAt }

/-- Insert a product-project link and return its ID -/
def insertProductProject (db : Database) (productDbId projectId : Nat) (linkedAt : Chronos.Timestamp) : IO Nat := do
  let id ← db.insert s!"INSERT INTO product_projects (product_db_id, project_id, linked_at) VALUES ({productDbId}, {projectId}, {linkedAt.seconds})"
  pure id.toNat

/-- Query a product-project link -/
def queryProductProjectLink (db : Database) (productDbId projectId : Nat) : IO (Option AgentMail.ProductProject) := do
  let row ← db.queryOne s!"SELECT id, product_db_id, project_id, linked_at FROM product_projects WHERE product_db_id = {productDbId} AND project_id = {projectId}"
  pure (row.bind rowToProductProject)
where
  rowToProductProject (row : Quarry.Row) : Option AgentMail.ProductProject := do
    let id ← row.get? 0 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let productDbId ← row.get? 1 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let projectId ← row.get? 2 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let linkedAt ← row.get? 3 >>= fun v => match v with | .integer n => some n | _ => none
    some { id, productDbId, projectId, linkedAt := Chronos.Timestamp.fromSeconds linkedAt }

/-- Query all projects linked to a product -/
def queryProjectsByProduct (db : Database) (productDbId : Nat) : IO (Array AgentMail.Project) := do
  let rows ← db.query s!"SELECT p.id, p.slug, p.human_key, p.created_at FROM projects p JOIN product_projects pp ON p.id = pp.project_id WHERE pp.product_db_id = {productDbId} ORDER BY pp.linked_at"
  pure (rows.filterMap rowToProject)
where
  rowToProject (row : Quarry.Row) : Option AgentMail.Project := do
    let id ← row.get? 0 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let slug ← row.get? 1 >>= fun v => match v with | .text s => some s | _ => none
    let humanKey ← row.get? 2 >>= fun v => match v with | .text s => some s | _ => none
    let createdAt ← row.get? 3 >>= fun v => match v with | .integer n => some n | _ => none
    some { id, slug, humanKey, createdAt := Chronos.Timestamp.fromSeconds createdAt }

/-- Query all products a project belongs to -/
def queryProductsByProject (db : Database) (projectId : Nat) : IO (Array AgentMail.Product) := do
  let rows ← db.query s!"SELECT p.id, p.product_id, p.created_at FROM products p JOIN product_projects pp ON p.id = pp.product_db_id WHERE pp.project_id = {projectId} ORDER BY pp.linked_at"
  pure (rows.filterMap rowToProduct)
where
  rowToProduct (row : Quarry.Row) : Option AgentMail.Product := do
    let id ← row.get? 0 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let productId ← row.get? 1 >>= fun v => match v with | .text s => some s | _ => none
    let createdAt ← row.get? 2 >>= fun v => match v with | .integer n => some n | _ => none
    some { id, productId, createdAt := Chronos.Timestamp.fromSeconds createdAt }

/-- Extended inbox entry with project information for cross-project queries -/
structure InboxEntryWithProject where
  id : Nat
  senderName : String
  subject : String
  importance : AgentMail.Importance
  ackRequired : Bool
  threadId : Option String
  createdTs : Chronos.Timestamp
  readAt : Option Chronos.Timestamp
  ackedAt : Option Chronos.Timestamp
  bodyMd : Option String
  recipientType : AgentMail.RecipientType
  projectId : Nat
  projectSlug : String
  projectKey : String
  deriving Repr

instance : Inhabited InboxEntryWithProject where
  default := {
    id := 0
    senderName := ""
    subject := ""
    importance := .normal
    ackRequired := false
    threadId := none
    createdTs := Chronos.Timestamp.fromSeconds 0
    readAt := none
    ackedAt := none
    bodyMd := none
    recipientType := .toRecipient
    projectId := 0
    projectSlug := ""
    projectKey := ""
  }

/-- Extended search result with project information -/
structure SearchResultWithProject where
  id : Nat
  subject : String
  importance : AgentMail.Importance
  ackRequired : Bool
  createdTs : Chronos.Timestamp
  threadId : Option String
  senderName : String
  projectId : Nat
  projectSlug : String
  projectKey : String
  deriving Repr, Inhabited

-- =============================================================================
-- Resource queries
-- =============================================================================

/-- Query all projects -/
def queryAllProjects (db : Database) : IO (Array AgentMail.Project) := do
  let rows ← db.query "SELECT id, slug, human_key, created_at FROM projects ORDER BY created_at DESC"
  pure (rows.filterMap rowToProject)
where
  rowToProject (row : Quarry.Row) : Option AgentMail.Project := do
    let id ← row.get? 0 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let slug ← row.get? 1 >>= fun v => match v with | .text s => some s | _ => none
    let humanKey ← row.get? 2 >>= fun v => match v with | .text s => some s | _ => none
    let createdAt ← row.get? 3 >>= fun v => match v with | .integer n => some n | _ => none
    some { id, slug, humanKey, createdAt := Chronos.Timestamp.fromSeconds createdAt }

/-- Query all agents in a project -/
def queryAgentsByProject (db : Database) (projectId : Nat) : IO (Array AgentMail.Agent) := do
  let rows ← db.query s!"SELECT id, project_id, name, program, model, task_description, contact_policy, attachments_policy, inception_ts, last_active_ts FROM agents WHERE project_id = {projectId} ORDER BY name"
  pure (rows.filterMap rowToAgent)
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

/-- Outbox entry for query results -/
structure OutboxEntry where
  id : Nat
  subject : String
  importance : AgentMail.Importance
  ackRequired : Bool
  threadId : Option String
  createdTs : Chronos.Timestamp
  bodyMd : Option String
  recipients : Array String
  deriving Repr, Inhabited

/-- Query outbox (sent messages) for an agent -/
def queryOutbox (db : Database) (projectId senderId : Nat) (limit : Nat) : IO (Array OutboxEntry) := do
  let sql := s!"SELECT m.id, m.subject, m.importance, m.ack_required, m.thread_id, m.created_ts, m.body_md FROM messages m WHERE m.project_id = {projectId} AND m.sender_id = {senderId} ORDER BY m.created_ts DESC LIMIT {limit}"
  let rows ← db.query sql
  let mut entries := #[]
  for row in rows do
    match rowToOutboxEntry row with
    | some entry =>
      let recipientRows ← db.query s!"SELECT a.name FROM message_recipients r JOIN agents a ON r.agent_id = a.id WHERE r.message_id = {entry.id}"
      let recipients := recipientRows.filterMap fun r =>
        r.get? 0 >>= fun v => match v with | .text s => some s | _ => none
      entries := entries.push { entry with recipients := recipients }
    | none => pure ()
  pure entries
where
  rowToOutboxEntry (row : Quarry.Row) : Option OutboxEntry := do
    let id ← row.get? 0 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let subject ← row.get? 1 >>= fun v => match v with | .text s => some s | _ => none
    let importanceStr ← row.get? 2 >>= fun v => match v with | .text s => some s | _ => none
    let ackRequiredInt ← row.get? 3 >>= fun v => match v with | .integer n => some n | _ => none
    let threadId : Option String := row.get? 4 >>= fun v => match v with | .text s => some s | _ => none
    let createdTs ← row.get? 5 >>= fun v => match v with | .integer n => some n | _ => none
    let bodyMd : Option String := row.get? 6 >>= fun v => match v with | .text s => some s | _ => none
    let importance := AgentMail.Importance.fromString? importanceStr |>.getD .normal
    some {
      id, subject, importance
      ackRequired := ackRequiredInt != 0
      threadId
      createdTs := Chronos.Timestamp.fromSeconds createdTs
      bodyMd
      recipients := #[]
    }

/-- Query unread urgent messages for an agent -/
def queryUnreadUrgentMessages (db : Database) (projectId agentId : Nat) (limit : Nat) : IO (Array InboxEntry) := do
  let sql := s!"SELECT m.id, a.name, m.subject, m.importance, m.ack_required, m.thread_id, m.created_ts, m.body_md, r.read_at, r.acked_at, r.recipient_type FROM messages m JOIN message_recipients r ON m.id = r.message_id JOIN agents a ON m.sender_id = a.id WHERE m.project_id = {projectId} AND r.agent_id = {agentId} AND r.read_at IS NULL AND m.importance IN ('high', 'urgent') ORDER BY m.created_ts DESC LIMIT {limit}"
  let rows ← db.query sql
  pure (rows.filterMap rowToInboxEntry)
where
  rowToInboxEntry (row : Quarry.Row) : Option InboxEntry := do
    let id ← row.get? 0 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let senderName ← row.get? 1 >>= fun v => match v with | .text s => some s | _ => none
    let subject ← row.get? 2 >>= fun v => match v with | .text s => some s | _ => none
    let importanceStr ← row.get? 3 >>= fun v => match v with | .text s => some s | _ => none
    let ackRequiredInt ← row.get? 4 >>= fun v => match v with | .integer n => some n | _ => none
    let threadId : Option String := row.get? 5 >>= fun v => match v with | .text s => some s | _ => none
    let createdTs ← row.get? 6 >>= fun v => match v with | .integer n => some n | _ => none
    let bodyMd : Option String := row.get? 7 >>= fun v => match v with | .text s => some s | _ => none
    let readAt : Option Int := row.get? 8 >>= fun v => match v with | .integer n => some n | _ => none
    let ackedAt : Option Int := row.get? 9 >>= fun v => match v with | .integer n => some n | _ => none
    let recipientTypeStr ← row.get? 10 >>= fun v => match v with | .text s => some s | _ => none
    let importance := AgentMail.Importance.fromString? importanceStr |>.getD .normal
    let recipientType := AgentMail.RecipientType.fromString? recipientTypeStr |>.getD .toRecipient
    some {
      id, senderName, subject, importance,
      ackRequired := ackRequiredInt != 0,
      threadId,
      createdTs := Chronos.Timestamp.fromSeconds createdTs,
      readAt := readAt.map Chronos.Timestamp.fromSeconds,
      ackedAt := ackedAt.map Chronos.Timestamp.fromSeconds,
      bodyMd,
      recipientType
    }

/-- Query messages requiring acknowledgment for an agent -/
def queryAckRequiredMessages (db : Database) (projectId agentId : Nat) (limit : Nat) : IO (Array InboxEntry) := do
  let sql := s!"SELECT m.id, a.name, m.subject, m.importance, m.ack_required, m.thread_id, m.created_ts, m.body_md, r.read_at, r.acked_at, r.recipient_type FROM messages m JOIN message_recipients r ON m.id = r.message_id JOIN agents a ON m.sender_id = a.id WHERE m.project_id = {projectId} AND r.agent_id = {agentId} AND m.ack_required = 1 AND r.acked_at IS NULL ORDER BY m.created_ts DESC LIMIT {limit}"
  let rows ← db.query sql
  pure (rows.filterMap rowToInboxEntry)
where
  rowToInboxEntry (row : Quarry.Row) : Option InboxEntry := do
    let id ← row.get? 0 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let senderName ← row.get? 1 >>= fun v => match v with | .text s => some s | _ => none
    let subject ← row.get? 2 >>= fun v => match v with | .text s => some s | _ => none
    let importanceStr ← row.get? 3 >>= fun v => match v with | .text s => some s | _ => none
    let ackRequiredInt ← row.get? 4 >>= fun v => match v with | .integer n => some n | _ => none
    let threadId : Option String := row.get? 5 >>= fun v => match v with | .text s => some s | _ => none
    let createdTs ← row.get? 6 >>= fun v => match v with | .integer n => some n | _ => none
    let bodyMd : Option String := row.get? 7 >>= fun v => match v with | .text s => some s | _ => none
    let readAt : Option Int := row.get? 8 >>= fun v => match v with | .integer n => some n | _ => none
    let ackedAt : Option Int := row.get? 9 >>= fun v => match v with | .integer n => some n | _ => none
    let recipientTypeStr ← row.get? 10 >>= fun v => match v with | .text s => some s | _ => none
    let importance := AgentMail.Importance.fromString? importanceStr |>.getD .normal
    let recipientType := AgentMail.RecipientType.fromString? recipientTypeStr |>.getD .toRecipient
    some {
      id, senderName, subject, importance,
      ackRequired := ackRequiredInt != 0,
      threadId,
      createdTs := Chronos.Timestamp.fromSeconds createdTs,
      readAt := readAt.map Chronos.Timestamp.fromSeconds,
      ackedAt := ackedAt.map Chronos.Timestamp.fromSeconds,
      bodyMd,
      recipientType
    }

/-- Query overdue acknowledgment messages (read but not acked within threshold) -/
def queryAckOverdueMessages (db : Database) (projectId agentId : Nat) (thresholdMinutes : Nat) (limit : Nat) : IO (Array InboxEntry) := do
  let now ← Chronos.Timestamp.now
  let thresholdSeconds := thresholdMinutes * 60
  let cutoff := now.seconds - thresholdSeconds
  let sql := s!"SELECT m.id, a.name, m.subject, m.importance, m.ack_required, m.thread_id, m.created_ts, m.body_md, r.read_at, r.acked_at, r.recipient_type FROM messages m JOIN message_recipients r ON m.id = r.message_id JOIN agents a ON m.sender_id = a.id WHERE m.project_id = {projectId} AND r.agent_id = {agentId} AND m.ack_required = 1 AND r.acked_at IS NULL AND r.read_at IS NOT NULL AND r.read_at < {cutoff} ORDER BY r.read_at ASC LIMIT {limit}"
  let rows ← db.query sql
  pure (rows.filterMap rowToInboxEntry)
where
  rowToInboxEntry (row : Quarry.Row) : Option InboxEntry := do
    let id ← row.get? 0 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let senderName ← row.get? 1 >>= fun v => match v with | .text s => some s | _ => none
    let subject ← row.get? 2 >>= fun v => match v with | .text s => some s | _ => none
    let importanceStr ← row.get? 3 >>= fun v => match v with | .text s => some s | _ => none
    let ackRequiredInt ← row.get? 4 >>= fun v => match v with | .integer n => some n | _ => none
    let threadId : Option String := row.get? 5 >>= fun v => match v with | .text s => some s | _ => none
    let createdTs ← row.get? 6 >>= fun v => match v with | .integer n => some n | _ => none
    let bodyMd : Option String := row.get? 7 >>= fun v => match v with | .text s => some s | _ => none
    let readAt : Option Int := row.get? 8 >>= fun v => match v with | .integer n => some n | _ => none
    let ackedAt : Option Int := row.get? 9 >>= fun v => match v with | .integer n => some n | _ => none
    let recipientTypeStr ← row.get? 10 >>= fun v => match v with | .text s => some s | _ => none
    let importance := AgentMail.Importance.fromString? importanceStr |>.getD .normal
    let recipientType := AgentMail.RecipientType.fromString? recipientTypeStr |>.getD .toRecipient
    some {
      id, senderName, subject, importance,
      ackRequired := ackRequiredInt != 0,
      threadId,
      createdTs := Chronos.Timestamp.fromSeconds createdTs,
      readAt := readAt.map Chronos.Timestamp.fromSeconds,
      ackedAt := ackedAt.map Chronos.Timestamp.fromSeconds,
      bodyMd,
      recipientType
    }

/-- Query stale acknowledgments (acked but stale based on threshold) -/
def queryStaleAckMessages (db : Database) (projectId agentId : Nat) (thresholdSeconds : Int) (limit : Nat) : IO (Array InboxEntry) := do
  let now ← Chronos.Timestamp.now
  let cutoff := now.seconds - thresholdSeconds
  let sql := s!"SELECT m.id, a.name, m.subject, m.importance, m.ack_required, m.thread_id, m.created_ts, m.body_md, r.read_at, r.acked_at, r.recipient_type FROM messages m JOIN message_recipients r ON m.id = r.message_id JOIN agents a ON m.sender_id = a.id WHERE m.project_id = {projectId} AND r.agent_id = {agentId} AND m.ack_required = 1 AND r.acked_at IS NOT NULL AND r.acked_at < {cutoff} ORDER BY r.acked_at ASC LIMIT {limit}"
  let rows ← db.query sql
  pure (rows.filterMap rowToInboxEntry)
where
  rowToInboxEntry (row : Quarry.Row) : Option InboxEntry := do
    let id ← row.get? 0 >>= fun v => match v with | .integer n => some n.toNat | _ => none
    let senderName ← row.get? 1 >>= fun v => match v with | .text s => some s | _ => none
    let subject ← row.get? 2 >>= fun v => match v with | .text s => some s | _ => none
    let importanceStr ← row.get? 3 >>= fun v => match v with | .text s => some s | _ => none
    let ackRequiredInt ← row.get? 4 >>= fun v => match v with | .integer n => some n | _ => none
    let threadId : Option String := row.get? 5 >>= fun v => match v with | .text s => some s | _ => none
    let createdTs ← row.get? 6 >>= fun v => match v with | .integer n => some n | _ => none
    let bodyMd : Option String := row.get? 7 >>= fun v => match v with | .text s => some s | _ => none
    let readAt : Option Int := row.get? 8 >>= fun v => match v with | .integer n => some n | _ => none
    let ackedAt : Option Int := row.get? 9 >>= fun v => match v with | .integer n => some n | _ => none
    let recipientTypeStr ← row.get? 10 >>= fun v => match v with | .text s => some s | _ => none
    let importance := AgentMail.Importance.fromString? importanceStr |>.getD .normal
    let recipientType := AgentMail.RecipientType.fromString? recipientTypeStr |>.getD .toRecipient
    some {
      id, senderName, subject, importance,
      ackRequired := ackRequiredInt != 0,
      threadId,
      createdTs := Chronos.Timestamp.fromSeconds createdTs,
      readAt := readAt.map Chronos.Timestamp.fromSeconds,
      ackedAt := ackedAt.map Chronos.Timestamp.fromSeconds,
      bodyMd,
      recipientType
    }

end Database

end AgentMail.Storage
