import Crucible
import AgentMail

open Crucible
open AgentMail

namespace Tests.FileReservation

testSuite "FileReservation"

test "JSON roundtrip" := do
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let later := Chronos.Timestamp.fromSeconds 1700003600
  let res : FileReservation := {
    id := 1
    projectId := 1
    agentId := 1
    pathPattern := "src/**/*.lean"
    exclusive := true
    reason := "Refactoring module"
    createdTs := now
    expiresTs := later
    releasedTs := none
  }
  let json := Lean.toJson res
  let parsed : Except String FileReservation := Lean.FromJson.fromJson? json
  match parsed with
  | Except.ok r =>
    r.id ≡ res.id
    r.pathPattern ≡ res.pathPattern
    r.exclusive ≡ res.exclusive
  | Except.error e => throw (IO.userError s!"Failed to parse: {e}")

test "isActive check" := do
  let now := Chronos.Timestamp.fromSeconds 1700001000
  let res : FileReservation := {
    id := 1
    projectId := 1
    agentId := 1
    pathPattern := "*.lean"
    exclusive := true
    reason := ""
    createdTs := Chronos.Timestamp.fromSeconds 1700000000
    expiresTs := Chronos.Timestamp.fromSeconds 1700002000
    releasedTs := none
  }
  shouldSatisfy (res.isActive now) "should be active"
  -- After expiry
  let later := Chronos.Timestamp.fromSeconds 1700003000
  shouldSatisfy (not (res.isActive later)) "should be expired"
  -- If released
  let released := { res with releasedTs := some now }
  shouldSatisfy (not (released.isActive now)) "should be released"

end Tests.FileReservation
