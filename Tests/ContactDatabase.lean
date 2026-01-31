import Crucible
import AgentMail

open Crucible
open AgentMail

namespace Tests.ContactDatabase

testSuite "ContactDatabase"

test "Insert and query contact request" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  -- Create project
  let projectId ← db.insertProject "test" "/test" now
  -- Create agents
  let agent1 : Agent := {
    id := 0, projectId := projectId, name := "Agent1"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let agent1Id ← db.insertAgent agent1
  let agent2 : Agent := {
    id := 0, projectId := projectId, name := "Agent2"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let agent2Id ← db.insertAgent agent2
  -- Insert contact request
  let requestId ← db.insertContactRequest projectId agent1Id agent2Id "Hello!" now
  requestId ≡ (1 : Nat)
  -- Query by ID
  let found ← db.queryContactRequestById requestId
  match found with
  | some r =>
    r.fromAgentId ≡ agent1Id
    r.toAgentId ≡ agent2Id
    r.message ≡ "Hello!"
    r.status ≡ ContactRequestStatus.pending
  | none => throw (IO.userError "Contact request not found")
  db.close

test "Query contact request between agents" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "test" "/test" now
  let agent1 : Agent := {
    id := 0, projectId := projectId, name := "Agent1"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let agent1Id ← db.insertAgent agent1
  let agent2 : Agent := {
    id := 0, projectId := projectId, name := "Agent2"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let agent2Id ← db.insertAgent agent2
  -- No request yet
  let notFound ← db.queryContactRequestBetween projectId agent1Id agent2Id
  shouldSatisfy notFound.isNone "should not find request before insert"
  -- Insert request
  let _ ← db.insertContactRequest projectId agent1Id agent2Id "" now
  -- Now found
  let found ← db.queryContactRequestBetween projectId agent1Id agent2Id
  shouldSatisfy found.isSome "should find request after insert"
  db.close

test "Query pending contact requests" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "test" "/test" now
  let agent1 : Agent := {
    id := 0, projectId := projectId, name := "Agent1"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let agent1Id ← db.insertAgent agent1
  let agent2 : Agent := {
    id := 0, projectId := projectId, name := "Agent2"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let agent2Id ← db.insertAgent agent2
  let agent3 : Agent := {
    id := 0, projectId := projectId, name := "Agent3"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let agent3Id ← db.insertAgent agent3
  -- Insert requests to agent2
  let _ ← db.insertContactRequest projectId agent1Id agent2Id "From 1" now
  let _ ← db.insertContactRequest projectId agent3Id agent2Id "From 3" now
  -- Query pending for agent2
  let pending ← db.queryPendingContactRequests projectId agent2Id
  pending.size ≡ (2 : Nat)
  db.close

test "Update contact request status" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let later := Chronos.Timestamp.fromSeconds 1700001000
  let projectId ← db.insertProject "test" "/test" now
  let agent1 : Agent := {
    id := 0, projectId := projectId, name := "Agent1"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let agent1Id ← db.insertAgent agent1
  let agent2 : Agent := {
    id := 0, projectId := projectId, name := "Agent2"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let agent2Id ← db.insertAgent agent2
  let requestId ← db.insertContactRequest projectId agent1Id agent2Id "" now
  -- Update to accepted
  db.updateContactRequestStatus requestId ContactRequestStatus.accepted later
  let found ← db.queryContactRequestById requestId
  match found with
  | some r =>
    r.status ≡ ContactRequestStatus.accepted
    match r.respondedAt with
    | some ts => ts.seconds ≡ later.seconds
    | none => throw (IO.userError "respondedAt should be set")
  | none => throw (IO.userError "Contact request not found")
  db.close

test "Insert and query contact" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "test" "/test" now
  let agent1 : Agent := {
    id := 0, projectId := projectId, name := "Agent1"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let agent1Id ← db.insertAgent agent1
  let agent2 : Agent := {
    id := 0, projectId := projectId, name := "Agent2"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let agent2Id ← db.insertAgent agent2
  -- Insert contact (order shouldn't matter)
  let contactId ← db.insertContact projectId agent2Id agent1Id now
  contactId ≡ (1 : Nat)
  -- Query both directions
  let found1 ← db.queryContactBetween projectId agent1Id agent2Id
  shouldSatisfy found1.isSome "should find contact (1,2)"
  let found2 ← db.queryContactBetween projectId agent2Id agent1Id
  shouldSatisfy found2.isSome "should find contact (2,1)"
  db.close

test "Query contacts for agent" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "test" "/test" now
  let agent1 : Agent := {
    id := 0, projectId := projectId, name := "Agent1"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let agent1Id ← db.insertAgent agent1
  let agent2 : Agent := {
    id := 0, projectId := projectId, name := "Agent2"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let agent2Id ← db.insertAgent agent2
  let agent3 : Agent := {
    id := 0, projectId := projectId, name := "Agent3"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let agent3Id ← db.insertAgent agent3
  -- Agent1 has contacts with Agent2 and Agent3
  let _ ← db.insertContact projectId agent1Id agent2Id now
  let _ ← db.insertContact projectId agent1Id agent3Id now
  -- Query contacts for Agent1
  let contacts ← db.queryContacts projectId agent1Id
  contacts.size ≡ (2 : Nat)
  -- Verify names
  let names := contacts.map (·.agentName)
  shouldSatisfy (names.contains "Agent2") "should include Agent2"
  shouldSatisfy (names.contains "Agent3") "should include Agent3"
  db.close

test "Update agent contact policy" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "test" "/test" now
  let agent : Agent := {
    id := 0, projectId := projectId, name := "Agent1"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let agentId ← db.insertAgent agent
  -- Update to blockAll
  db.updateAgentContactPolicy agentId ContactPolicy.blockAll
  let found ← db.queryAgentById agentId
  match found with
  | some a => a.contactPolicy ≡ ContactPolicy.blockAll
  | none => throw (IO.userError "Agent not found")
  db.close

end Tests.ContactDatabase
