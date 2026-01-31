import Crucible
import AgentMail

open Crucible
open AgentMail

namespace Tests.Contact

testSuite "Contact"

test "Contact JSON roundtrip" := do
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let contact : Contact := {
    id := 1
    projectId := 1
    agentId1 := 1
    agentId2 := 2
    createdTs := now
  }
  let json := Lean.toJson contact
  let parsed : Except String Contact := Lean.FromJson.fromJson? json
  match parsed with
  | Except.ok c =>
    c.id ≡ contact.id
    c.agentId1 ≡ contact.agentId1
    c.agentId2 ≡ contact.agentId2
  | Except.error e => throw (IO.userError s!"Failed to parse: {e}")

test "Contact.involvesAgent" := do
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let contact : Contact := {
    id := 1, projectId := 1, agentId1 := 1, agentId2 := 2, createdTs := now
  }
  shouldSatisfy (contact.involvesAgent 1) "should involve agent 1"
  shouldSatisfy (contact.involvesAgent 2) "should involve agent 2"
  shouldSatisfy (not (contact.involvesAgent 3)) "should not involve agent 3"

test "Contact.otherAgent" := do
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let contact : Contact := {
    id := 1, projectId := 1, agentId1 := 1, agentId2 := 2, createdTs := now
  }
  contact.otherAgent 1 ≡ some 2
  contact.otherAgent 2 ≡ some 1
  contact.otherAgent 3 ≡ none

end Tests.Contact
