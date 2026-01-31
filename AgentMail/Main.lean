/-
  AgentMail.Main - Entry point for agent-mail server
-/
import AgentMail.Config
import AgentMail.Storage.Database
import AgentMail.Server.Server

namespace AgentMail

def main : IO Unit := do
  -- Load configuration from environment
  let cfg ← Config.fromEnv

  -- Open database connection
  let db ← Storage.Database.openFile cfg.databasePath

  -- Run server (blocking)
  Server.run cfg db

  -- Close database on shutdown
  db.close

end AgentMail

def main : IO Unit := AgentMail.main
