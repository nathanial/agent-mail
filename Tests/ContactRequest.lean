import Crucible
import AgentMail

open Crucible
open AgentMail

namespace Tests.ContactRequest

testSuite "ContactRequest"

test "ContactRequestStatus roundtrip" := do
  let statuses := #[ContactRequestStatus.pending, ContactRequestStatus.accepted, ContactRequestStatus.rejected]
  for s in statuses do
    let str := s.toString
    let parsed := ContactRequestStatus.fromString? str
    parsed ≡ some s

test "ContactRequest JSON roundtrip" := do
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let req : ContactRequest := {
    id := 1
    projectId := 1
    fromAgentId := 1
    toAgentId := 2
    message := "Hello, let's connect"
    status := ContactRequestStatus.pending
    createdTs := now
    respondedAt := none
  }
  let json := Lean.toJson req
  let parsed : Except String ContactRequest := Lean.FromJson.fromJson? json
  match parsed with
  | Except.ok r =>
    r.id ≡ req.id
    r.fromAgentId ≡ req.fromAgentId
    r.toAgentId ≡ req.toAgentId
    r.status ≡ req.status
  | Except.error e => throw (IO.userError s!"Failed to parse: {e}")

test "ContactRequest with respondedAt" := do
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let later := Chronos.Timestamp.fromSeconds 1700001000
  let req : ContactRequest := {
    id := 1
    projectId := 1
    fromAgentId := 1
    toAgentId := 2
    message := ""
    status := ContactRequestStatus.accepted
    createdTs := now
    respondedAt := some later
  }
  let json := Lean.toJson req
  let parsed : Except String ContactRequest := Lean.FromJson.fromJson? json
  match parsed with
  | Except.ok r =>
    r.status ≡ ContactRequestStatus.accepted
    match r.respondedAt with
    | some ts => ts.seconds ≡ later.seconds
    | none => throw (IO.userError "respondedAt should be set")
  | Except.error e => throw (IO.userError s!"Failed to parse: {e}")

end Tests.ContactRequest
