/-
  AgentMail.Config - Server configuration
-/

namespace AgentMail

/-- Server configuration -/
structure Config where
  environment : String := "development"
  port : UInt16 := 8765
  host : String := "127.0.0.1"
  databasePath : String := "agent_mail.db"
  authToken : Option String := none
  deriving Repr

namespace Config

/-- Default configuration -/
def default : Config := {}

/-- Load configuration from environment variables -/
def fromEnv : IO Config := do
  let env ← IO.getEnv "AGENT_MAIL_ENV"
  let port ← IO.getEnv "AGENT_MAIL_PORT"
  let host ← IO.getEnv "AGENT_MAIL_HOST"
  let dbPath ← IO.getEnv "AGENT_MAIL_DB"
  let token ← IO.getEnv "AGENT_MAIL_TOKEN"

  let portVal : UInt16 := match port with
    | some p => match p.toNat? with
      | some n => if n > 0 && n < 65536 then n.toUInt16 else 8765
      | none => 8765
    | none => 8765

  pure {
    environment := env.getD "development"
    port := portVal
    host := host.getD "127.0.0.1"
    databasePath := dbPath.getD "agent_mail.db"
    authToken := token
  }

/-- Display configuration (hiding auth token) -/
def display (cfg : Config) : String :=
  let tokenDisplay := match cfg.authToken with
    | some _ => "(set)"
    | none => "(none)"
  s!"Config \{ env: {cfg.environment}, host: {cfg.host}, port: {cfg.port}, database: {cfg.databasePath}, token: {tokenDisplay} }"

end Config

end AgentMail
