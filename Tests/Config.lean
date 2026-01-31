import Crucible
import AgentMail

open Crucible
open AgentMail

namespace Tests.Config

testSuite "Config"

test "Default config" := do
  let cfg := Config.default
  cfg.environment ≡ "development"
  cfg.port ≡ 8765
  cfg.host ≡ "127.0.0.1"
  cfg.databasePath ≡ "agent_mail.db"
  shouldSatisfy cfg.authToken.isNone "authToken should be none"

test "Display hides token" := do
  let cfg := { Config.default with authToken := some "secret" }
  let display := cfg.display
  shouldSatisfy (display.find? "(set)" |>.isSome) "should show (set)"
  shouldSatisfy (display.find? "secret" |>.isNone) "should hide secret"

end Tests.Config
