/-
  AgentMail.Resources.Config - Configuration MCP resources
-/
import Citadel
import AgentMail.Config
import AgentMail.Resources.Core

open Citadel

namespace AgentMail.Resources.Config

/-- Handle GET /resource/config/environment -/
def handleEnvironment (cfg : AgentMail.Config) (req : ServerRequest) : IO Response := do
  let result := Lean.Json.mkObj [
    ("environment", Lean.Json.str cfg.environment),
    ("host", Lean.Json.str cfg.host),
    ("port", Lean.Json.num cfg.port.toNat),
    ("database_path", Lean.Json.str cfg.databasePath),
    ("storage_root", Lean.Json.str cfg.storageRoot),
    ("worktrees_enabled", Lean.Json.bool cfg.worktreesEnabled),
    ("auth_configured", Lean.Json.bool cfg.authToken.isSome)
  ]
  Core.resourceOkFormatted cfg req "config.environment" result

end AgentMail.Resources.Config
