/-
  AgentMail.Main - Entry point for agent-mail server and CLI
-/
import AgentMail.Config
import AgentMail.Storage.Database
import AgentMail.Server.Server
import AgentMail.CLI.Commands
import AgentMail.CLI.Handlers

namespace AgentMail

def main (args : List String) : IO UInt32 := do
  -- CLI mode when arguments provided, otherwise run HTTP server
  CLI.Handlers.run args

end AgentMail

def main (args : List String) : IO UInt32 := AgentMail.main args
