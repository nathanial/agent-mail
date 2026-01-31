import Crucible
import AgentMail

open Crucible
open AgentMail

namespace Tests.Macros

testSuite "Macros"

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

def testConfig : Config := {
  environment := "test"
  port := 8765
  host := "127.0.0.1"
  databasePath := ":memory:"
  storageRoot := "/tmp/test-agent-mail-archive"
  gitAuthorName := "test-agent"
  gitAuthorEmail := "test@example.com"
  worktreesEnabled := false
  authToken := none
}

-- =============================================================================
-- macro_start_session tests
-- =============================================================================

test "macro_start_session creates project and agent" := do
  let db ← Storage.Database.openMemory
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/test/project"),
    ("program", Lean.Json.str "test-program"),
    ("model", Lean.Json.str "claude-4")
  ]
  let req : JsonRpc.Request := {
    method := "macro_start_session"
    params := some params
    id := some (JsonRpc.RequestId.num 1)
  }
  let resp ← Tools.Macros.handleMacroStartSession db testConfig req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some result =>
      match result.getObjValAs? String "agent_name" with
      | Except.ok name => shouldSatisfy (name.length > 0) "agent_name should not be empty"
      | Except.error e => throw (IO.userError s!"Failed to read agent_name: {e}")
      match result.getObjValAs? String "project" with
      | Except.ok proj => proj ≡ "/test/project"
      | Except.error e => throw (IO.userError s!"Failed to read project: {e}")
      match result.getObjValAs? String "project_slug" with
      | Except.ok slug => shouldSatisfy (slug.length > 0) "project_slug should not be empty"
      | Except.error e => throw (IO.userError s!"Failed to read project_slug: {e}")
      match result.getObjValAs? String "session_id" with
      | Except.ok sid => shouldSatisfy (sid.startsWith "session-") "session_id should start with session-"
      | Except.error e => throw (IO.userError s!"Failed to read session_id: {e}")
  | none => throw (IO.userError "Expected macro_start_session result")
  db.close

test "macro_start_session reuses existing project" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let _ ← db.insertProject "existing-project" "/existing/project" now
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/existing/project"),
    ("program", Lean.Json.str "test-program"),
    ("model", Lean.Json.str "claude-4")
  ]
  let req : JsonRpc.Request := {
    method := "macro_start_session"
    params := some params
    id := some (JsonRpc.RequestId.num 2)
  }
  let resp ← Tools.Macros.handleMacroStartSession db testConfig req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some result =>
      match result.getObjValAs? String "project_slug" with
      | Except.ok slug => slug ≡ "existing-project"
      | Except.error e => throw (IO.userError s!"Failed to read project_slug: {e}")
  | none => throw (IO.userError "Expected macro_start_session result")
  db.close

-- =============================================================================
-- macro_prepare_thread tests
-- =============================================================================

test "macro_prepare_thread fetches messages and marks read" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "p1" "/p1" now
  let agent1Id ← db.insertAgent (mkAgent projectId "Agent1" now)
  let agent2Id ← db.insertAgent (mkAgent projectId "Agent2" now)
  -- Create a message in thread
  let msg : Message := {
    id := 0
    projectId := projectId
    senderId := agent1Id
    subject := "Test Subject"
    bodyMd := "Test body content"
    attachments := #[]
    importance := .normal
    ackRequired := false
    threadId := some "test-thread-123"
    createdTs := now
  }
  let msgId ← db.insertMessage msg
  let recipient : MessageRecipient := {
    messageId := msgId
    agentId := agent2Id
    recipientType := .toRecipient
    readAt := none
    ackedAt := none
  }
  db.insertMessageRecipient recipient
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("agent_name", Lean.Json.str "Agent2"),
    ("thread_id", Lean.Json.str "test-thread-123")
  ]
  let req : JsonRpc.Request := {
    method := "macro_prepare_thread"
    params := some params
    id := some (JsonRpc.RequestId.num 3)
  }
  let resp ← Tools.Macros.handleMacroPrepareThread db testConfig req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some result =>
      match result.getObjValAs? String "thread_id" with
      | Except.ok tid => tid ≡ "test-thread-123"
      | Except.error e => throw (IO.userError s!"Failed to read thread_id: {e}")
      match result.getObjValAs? Nat "total_messages" with
      | Except.ok count => count ≡ (1 : Nat)
      | Except.error e => throw (IO.userError s!"Failed to read total_messages: {e}")
      match result.getObjValAs? Nat "messages_marked_read" with
      | Except.ok count => count ≡ (1 : Nat)
      | Except.error e => throw (IO.userError s!"Failed to read messages_marked_read: {e}")
  | none => throw (IO.userError "Expected macro_prepare_thread result")
  -- Verify message was marked read in database
  let status ← db.queryRecipientStatus msgId agent2Id
  match status with
  | some s => shouldSatisfy s.readAt.isSome "Message should be marked read"
  | none => throw (IO.userError "Recipient status not found")
  db.close

-- =============================================================================
-- macro_file_reservation_cycle tests
-- =============================================================================

test "macro_file_reservation_cycle grants reservations" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "p1" "/p1" now
  let _ ← db.insertAgent (mkAgent projectId "Agent1" now)
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("agent_name", Lean.Json.str "Agent1"),
    ("paths", Lean.Json.arr #[Lean.Json.str "src/**/*.lean", Lean.Json.str "tests/*.lean"])
  ]
  let req : JsonRpc.Request := {
    method := "macro_file_reservation_cycle"
    params := some params
    id := some (JsonRpc.RequestId.num 4)
  }
  let resp ← Tools.Macros.handleMacroFileReservationCycle db testConfig req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some result =>
      match result.getObjValAs? Nat "granted_count" with
      | Except.ok count => count ≡ (2 : Nat)
      | Except.error e => throw (IO.userError s!"Failed to read granted_count: {e}")
      match result.getObjValAs? Nat "conflict_count" with
      | Except.ok count => count ≡ (0 : Nat)
      | Except.error e => throw (IO.userError s!"Failed to read conflict_count: {e}")
  | none => throw (IO.userError "Expected macro_file_reservation_cycle result")
  db.close

test "macro_file_reservation_cycle reports conflicts" := do
  let db ← Storage.Database.openMemory
  -- Use current time to ensure reservation is active when handler runs
  let now ← Chronos.Timestamp.now
  let projectId ← db.insertProject "p1" "/p1" now
  let agent1Id ← db.insertAgent (mkAgent projectId "Agent1" now)
  let _agent2Id ← db.insertAgent (mkAgent projectId "Agent2" now)
  -- Create an existing reservation by Agent1 that expires far in the future
  let expiresTs := Chronos.Timestamp.fromSeconds (now.seconds + 86400)  -- 24 hours
  let _ ← db.insertFileReservation projectId agent1Id "src/**/*.lean" true "working" now expiresTs
  -- Agent2 tries to reserve overlapping pattern
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("agent_name", Lean.Json.str "Agent2"),
    ("paths", Lean.Json.arr #[Lean.Json.str "src/**/*.lean"])
  ]
  let req : JsonRpc.Request := {
    method := "macro_file_reservation_cycle"
    params := some params
    id := some (JsonRpc.RequestId.num 5)
  }
  let resp ← Tools.Macros.handleMacroFileReservationCycle db testConfig req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some result =>
      match result.getObjValAs? Nat "granted_count" with
      | Except.ok count => count ≡ (1 : Nat)
      | Except.error e => throw (IO.userError s!"Failed to read granted_count: {e}")
      match result.getObjValAs? Nat "conflict_count" with
      | Except.ok count => count ≡ (1 : Nat)
      | Except.error e => throw (IO.userError s!"Failed to read conflict_count: {e}")
      match result.getObjVal? "conflicts" with
      | Except.ok (Lean.Json.arr conflicts) =>
          conflicts.size ≡ (1 : Nat)
          match conflicts.toList with
          | c :: _ =>
              match c.getObjValAs? String "held_by" with
              | Except.ok holder => holder ≡ "Agent1"
              | Except.error e => throw (IO.userError s!"Failed to read held_by: {e}")
          | [] => throw (IO.userError "Expected conflict entry")
      | _ => throw (IO.userError "Expected conflicts array")
  | none => throw (IO.userError "Expected macro_file_reservation_cycle result")
  db.close

-- =============================================================================
-- macro_contact_handshake tests
-- =============================================================================

test "macro_contact_handshake establishes contact" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "p1" "/p1" now
  let _agent1Id ← db.insertAgent (mkAgent projectId "Agent1" now)
  let _agent2Id ← db.insertAgent (mkAgent projectId "Agent2" now)
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("from_agent", Lean.Json.str "Agent1"),
    ("to_agent", Lean.Json.str "Agent2")
  ]
  let req : JsonRpc.Request := {
    method := "macro_contact_handshake"
    params := some params
    id := some (JsonRpc.RequestId.num 6)
  }
  let resp ← Tools.Macros.handleMacroContactHandshake db testConfig req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some result =>
      match result.getObjValAs? Bool "contact_established" with
      | Except.ok established => established ≡ true
      | Except.error e => throw (IO.userError s!"Failed to read contact_established: {e}")
      match result.getObjValAs? Bool "was_existing" with
      | Except.ok wasExisting => wasExisting ≡ false
      | Except.error e => throw (IO.userError s!"Failed to read was_existing: {e}")
      match result.getObjValAs? String "from_agent" with
      | Except.ok name => name ≡ "Agent1"
      | Except.error e => throw (IO.userError s!"Failed to read from_agent: {e}")
      match result.getObjValAs? String "to_agent" with
      | Except.ok name => name ≡ "Agent2"
      | Except.error e => throw (IO.userError s!"Failed to read to_agent: {e}")
  | none => throw (IO.userError "Expected macro_contact_handshake result")
  db.close

test "macro_contact_handshake returns existing contact" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "p1" "/p1" now
  let agent1Id ← db.insertAgent (mkAgent projectId "Agent1" now)
  let agent2Id ← db.insertAgent (mkAgent projectId "Agent2" now)
  -- Create existing contact
  let _ ← db.insertContact projectId agent1Id agent2Id now
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("from_agent", Lean.Json.str "Agent1"),
    ("to_agent", Lean.Json.str "Agent2")
  ]
  let req : JsonRpc.Request := {
    method := "macro_contact_handshake"
    params := some params
    id := some (JsonRpc.RequestId.num 7)
  }
  let resp ← Tools.Macros.handleMacroContactHandshake db testConfig req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some result =>
      match result.getObjValAs? Bool "contact_established" with
      | Except.ok established => established ≡ true
      | Except.error e => throw (IO.userError s!"Failed to read contact_established: {e}")
      match result.getObjValAs? Bool "was_existing" with
      | Except.ok wasExisting => wasExisting ≡ true
      | Except.error e => throw (IO.userError s!"Failed to read was_existing: {e}")
  | none => throw (IO.userError "Expected macro_contact_handshake result")
  db.close

test "macro_contact_handshake rejects self-contact" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "p1" "/p1" now
  let _agent1Id ← db.insertAgent (mkAgent projectId "Agent1" now)
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("from_agent", Lean.Json.str "Agent1"),
    ("to_agent", Lean.Json.str "Agent1")
  ]
  let req : JsonRpc.Request := {
    method := "macro_contact_handshake"
    params := some params
    id := some (JsonRpc.RequestId.num 8)
  }
  let resp ← Tools.Macros.handleMacroContactHandshake db testConfig req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.error with
  | some err =>
      err.message ≡ "Invalid params"
      match err.data with
      | some (Lean.Json.str details) =>
          details ≡ "cannot establish contact with yourself"
      | _ => throw (IO.userError "Expected error details string")
  | none => throw (IO.userError "Expected error response for self-contact")
  db.close

test "macro_contact_handshake respects block_all policy" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "p1" "/p1" now
  let _agent1Id ← db.insertAgent (mkAgent projectId "Agent1" now)
  let _agent2Id ← db.insertAgent (mkAgentWithPolicy projectId "Agent2" .blockAll now)
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("from_agent", Lean.Json.str "Agent1"),
    ("to_agent", Lean.Json.str "Agent2")
  ]
  let req : JsonRpc.Request := {
    method := "macro_contact_handshake"
    params := some params
    id := some (JsonRpc.RequestId.num 9)
  }
  let resp ← Tools.Macros.handleMacroContactHandshake db testConfig req
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

test "macro_contact_handshake accepts reverse pending request" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "p1" "/p1" now
  let agent1Id ← db.insertAgent (mkAgent projectId "Agent1" now)
  let agent2Id ← db.insertAgent (mkAgent projectId "Agent2" now)
  let requestId ← db.insertContactRequest projectId agent2Id agent1Id "hello" now
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("from_agent", Lean.Json.str "Agent1"),
    ("to_agent", Lean.Json.str "Agent2")
  ]
  let req : JsonRpc.Request := {
    method := "macro_contact_handshake"
    params := some params
    id := some (JsonRpc.RequestId.num 10)
  }
  let resp ← Tools.Macros.handleMacroContactHandshake db testConfig req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some result =>
      match result.getObjValAs? Bool "contact_established" with
      | Except.ok established => established ≡ true
      | Except.error e => throw (IO.userError s!"Failed to read contact_established: {e}")
  | none => throw (IO.userError "Expected macro_contact_handshake result")
  let contact ← db.queryContactBetween projectId agent1Id agent2Id
  match contact with
  | some _ => pure ()
  | none => throw (IO.userError "Expected contact to be created")
  let updated ← db.queryContactRequestById requestId
  match updated with
  | some r => r.status ≡ ContactRequestStatus.accepted
  | none => throw (IO.userError "Expected contact request to exist")
  db.close

end Tests.Macros
