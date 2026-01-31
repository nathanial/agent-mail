import Crucible
import AgentMail

open Crucible
open AgentMail

namespace Tests.ContactTools

testSuite "ContactTools"

open Citadel

def mkAgent (projectId : Nat) (name : String) (now : Chronos.Timestamp) : Agent := {
  id := 0
  projectId := projectId
  name := name
  program := "test"
  model := "test"
  taskDescription := ""
  contactPolicy := ContactPolicy.auto
  attachmentsPolicy := AttachmentsPolicy.auto
  inceptionTs := now
  lastActiveTs := now
}

def mkAgentWithPolicy (projectId : Nat) (name : String) (policy : ContactPolicy) (now : Chronos.Timestamp) : Agent := {
  id := 0
  projectId := projectId
  name := name
  program := "test"
  model := "test"
  taskDescription := ""
  contactPolicy := policy
  attachmentsPolicy := AttachmentsPolicy.auto
  inceptionTs := now
  lastActiveTs := now
}

def parseJsonRpcResponse (resp : Response) : IO JsonRpc.Response := do
  let body := String.fromUTF8! resp.body
  let json := Lean.Json.parse body
  match json with
  | Except.ok j =>
      match (Lean.FromJson.fromJson? j : Except String JsonRpc.Response) with
      | Except.ok r => pure r
      | Except.error e => throw (IO.userError s!"Failed to decode JSON-RPC response: {e}")
  | Except.error e => throw (IO.userError s!"Failed to parse JSON: {e}")

test "list_contacts returns array payload" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "p1" "/p1" now
  let agent1Id ← db.insertAgent (mkAgent projectId "Agent1" now)
  let agent2Id ← db.insertAgent (mkAgent projectId "Agent2" now)
  let _ ← db.insertContact projectId agent1Id agent2Id now
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("agent_name", Lean.Json.str "Agent1")
  ]
  let req : JsonRpc.Request := {
    method := "list_contacts"
    params := some params
    id := some (JsonRpc.RequestId.num 1)
  }
  let resp ← Tools.Contacts.handleListContacts db req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some (Lean.Json.arr items) =>
      items.size ≡ (1 : Nat)
      match items.toList with
      | entry :: _ =>
          match entry.getObjValAs? String "agent_name" with
          | Except.ok name => name ≡ "Agent2"
          | Except.error e => throw (IO.userError s!"Failed to read agent_name: {e}")
      | [] => throw (IO.userError "Expected contact entry")
  | _ => throw (IO.userError "Expected list_contacts result to be a JSON array")
  db.close

test "request_contact refreshes non-pending requests" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let later := Chronos.Timestamp.fromSeconds 1700001000
  let projectId ← db.insertProject "p1" "/p1" now
  let agent1Id ← db.insertAgent (mkAgent projectId "Agent1" now)
  let agent2Id ← db.insertAgent (mkAgent projectId "Agent2" now)
  let requestId ← db.insertContactRequest projectId agent1Id agent2Id "old message" now
  db.updateContactRequestStatus requestId ContactRequestStatus.accepted later
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("from_agent", Lean.Json.str "Agent1"),
    ("to_agent", Lean.Json.str "Agent2"),
    ("message", Lean.Json.str "new message")
  ]
  let req : JsonRpc.Request := {
    method := "request_contact"
    params := some params
    id := some (JsonRpc.RequestId.num 2)
  }
  let resp ← Tools.Contacts.handleRequestContact db req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some result =>
      match result.getObjValAs? String "status" with
      | Except.ok status => status ≡ "pending"
      | Except.error e => throw (IO.userError s!"Failed to read status: {e}")
  | none => throw (IO.userError "Expected request_contact result")
  let refreshed ← db.queryContactRequestById requestId
  match refreshed with
  | some r =>
      r.status ≡ ContactRequestStatus.pending
      r.message ≡ "new message"
      shouldSatisfy r.respondedAt.isNone "responded_at should be cleared"
  | none => throw (IO.userError "Contact request not found after refresh")
  db.close

test "respond_contact rejects requests from other projects" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let project1Id ← db.insertProject "p1" "/p1" now
  let project2Id ← db.insertProject "p2" "/p2" now
  let agentA1Id ← db.insertAgent (mkAgent project1Id "AgentA" now)
  let agentB1Id ← db.insertAgent (mkAgent project1Id "AgentB" now)
  let _agentB2Id ← db.insertAgent (mkAgent project2Id "AgentB" now)
  let requestId ← db.insertContactRequest project1Id agentA1Id agentB1Id "hello" now
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p2"),
    ("agent_name", Lean.Json.str "AgentB"),
    ("request_id", Lean.Json.num requestId),
    ("accept", Lean.Json.bool true)
  ]
  let req : JsonRpc.Request := {
    method := "respond_contact"
    params := some params
    id := some (JsonRpc.RequestId.num 3)
  }
  let resp ← Tools.Contacts.handleRespondContact db req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.error with
  | some err =>
      err.message ≡ "Invalid params"
      match err.data with
      | some (Lean.Json.str details) =>
          details ≡ "contact request does not belong to this project"
      | _ => throw (IO.userError "Expected error details string")
  | none => throw (IO.userError "Expected error response for project mismatch")
  db.close

test "respond_contact reuses existing contact" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "p1" "/p1" now
  let agent1Id ← db.insertAgent (mkAgent projectId "Agent1" now)
  let agent2Id ← db.insertAgent (mkAgent projectId "Agent2" now)
  let requestId ← db.insertContactRequest projectId agent1Id agent2Id "hello" now
  let existingContactId ← db.insertContact projectId agent1Id agent2Id now
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("agent_name", Lean.Json.str "Agent2"),
    ("request_id", Lean.Json.num requestId),
    ("accept", Lean.Json.bool true)
  ]
  let req : JsonRpc.Request := {
    method := "respond_contact"
    params := some params
    id := some (JsonRpc.RequestId.num 4)
  }
  let resp ← Tools.Contacts.handleRespondContact db req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some result =>
      match result.getObjValAs? Int "contact_id" with
      | Except.ok contactId => contactId ≡ Int.ofNat existingContactId
      | Except.error e => throw (IO.userError s!"Failed to read contact_id: {e}")
  | none => throw (IO.userError "Expected respond_contact result")
  let contacts ← db.queryContacts projectId agent1Id
  contacts.size ≡ (1 : Nat)
  db.close

test "request_contact respects block_all policy" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "p1" "/p1" now
  let _agent1Id ← db.insertAgent (mkAgent projectId "Agent1" now)
  let _agent2Id ← db.insertAgent (mkAgentWithPolicy projectId "Agent2" ContactPolicy.blockAll now)
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("from_agent", Lean.Json.str "Agent1"),
    ("to_agent", Lean.Json.str "Agent2"),
    ("message", Lean.Json.str "hello")
  ]
  let req : JsonRpc.Request := {
    method := "request_contact"
    params := some params
    id := some (JsonRpc.RequestId.num 5)
  }
  let resp ← Tools.Contacts.handleRequestContact db req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.error with
  | some err =>
      err.message ≡ "Invalid params"
      match err.data with
      | some (Lean.Json.str details) =>
          details ≡ "Agent2 is not accepting contact requests"
      | _ => throw (IO.userError "Expected error details string")
  | none => throw (IO.userError "Expected error response for block_all policy")
  db.close

test "respond_contact only allows recipient agent" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "p1" "/p1" now
  let agent1Id ← db.insertAgent (mkAgent projectId "Agent1" now)
  let agent2Id ← db.insertAgent (mkAgent projectId "Agent2" now)
  let _agent3Id ← db.insertAgent (mkAgent projectId "Agent3" now)
  let requestId ← db.insertContactRequest projectId agent1Id agent2Id "hello" now
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("agent_name", Lean.Json.str "Agent3"),
    ("request_id", Lean.Json.num requestId),
    ("accept", Lean.Json.bool true)
  ]
  let req : JsonRpc.Request := {
    method := "respond_contact"
    params := some params
    id := some (JsonRpc.RequestId.num 6)
  }
  let resp ← Tools.Contacts.handleRespondContact db req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.error with
  | some err =>
      err.message ≡ "Invalid params"
      match err.data with
      | some (Lean.Json.str details) =>
          details ≡ "you can only respond to requests sent to you"
      | _ => throw (IO.userError "Expected error details string")
  | none => throw (IO.userError "Expected error response for wrong agent")
  db.close

end Tests.ContactTools
