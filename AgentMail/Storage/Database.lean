/-
  AgentMail.Storage.Database - SQLite database connection and schema
-/
import Quarry

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
    contact_policy TEXT DEFAULT 'open',
    attachments_policy TEXT DEFAULT 'accept',
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

end Database

end AgentMail.Storage
