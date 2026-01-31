import Lake
open Lake DSL

package «agent-mail» where
  version := v!"0.1.0"
  moreLinkArgs := #["-L/opt/homebrew/opt/openssl@3/lib", "-lssl", "-lcrypto", "-L/opt/homebrew/opt/curl/lib", "-lcurl"]

require crucible from git "https://github.com/nathanial/crucible" @ "v0.0.9"
require quarry from git "https://github.com/nathanial/quarry" @ "v0.0.3"
require citadel from git "https://github.com/nathanial/citadel" @ "v0.0.2"
require chronos from git "https://github.com/nathanial/chronos-lean" @ "v0.0.7"
require oracle from git "https://github.com/nathanial/oracle" @ "v0.2.0"
require rune from "../../util/rune"

@[default_target]
lean_lib AgentMail where
  roots := #[`AgentMail]

lean_lib Tests where
  roots := #[`Tests]

lean_exe «agent-mail» where
  root := `AgentMail.Main

@[test_driver]
lean_exe «agent-mail_tests» where
  root := `Tests.Main
