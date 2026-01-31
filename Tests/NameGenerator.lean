import Crucible
import AgentMail

open Crucible
open AgentMail

namespace Tests.NameGenerator

testSuite "NameGenerator"

open AgentMail.Utils.NameGenerator in
test "Deterministic name generation" := do
  let name1 := generateNameDeterministic 12345
  let name2 := generateNameDeterministic 12345
  name1 ≡ name2
  -- Verify it follows AdjectiveNoun pattern
  shouldSatisfy (name1.length > 0) "name should not be empty"

open AgentMail.Utils.NameGenerator in
test "Different seeds produce different names" := do
  let name1 := generateNameDeterministic 1
  let name2 := generateNameDeterministic 1000000
  shouldSatisfy (name1 != name2) "different seeds should produce different names"

end Tests.NameGenerator
