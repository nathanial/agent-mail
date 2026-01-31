import Crucible
import AgentMail

open Crucible
open AgentMail

namespace Tests.BuildSlots

testSuite "BuildSlots"

open Citadel
open AgentMail.Tools.BuildSlots

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

def testConfig : Config := {
  Config.default with
  storageRoot := "/tmp/agent-mail-test-archive"
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

test "acquire_build_slot grants when available" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "p1" "/p1" now
  let _agentId ← db.insertAgent (mkAgent projectId "Agent1" now)
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("agent_name", Lean.Json.str "Agent1"),
    ("slot_name", Lean.Json.str "build"),
    ("ttl_seconds", Lean.Json.num 7200)
  ]
  let req : JsonRpc.Request := {
    method := "acquire_build_slot"
    params := some params
    id := some (JsonRpc.RequestId.num 1)
  }
  let resp ← handleAcquireBuildSlot db testConfig req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some result =>
      match result.getObjValAs? Bool "granted" with
      | Except.ok granted => shouldSatisfy granted "should be granted"
      | Except.error e => throw (IO.userError s!"Expected granted: {e}")
      match result.getObjValAs? String "slot_name" with
      | Except.ok slotName => slotName ≡ "build"
      | Except.error e => throw (IO.userError s!"Expected slot_name: {e}")
      match result.getObjValAs? Nat "slot_id" with
      | Except.ok slotId => shouldSatisfy (slotId > 0) "should have slot_id"
      | Except.error e => throw (IO.userError s!"Expected slot_id: {e}")
  | none => throw (IO.userError "Expected result")
  db.close

test "acquire_build_slot rejects when held by another agent" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let expires := Chronos.Timestamp.fromSeconds 2000000000  -- Far future
  let projectId ← db.insertProject "p1" "/p1" now
  let agent1Id ← db.insertAgent (mkAgent projectId "Agent1" now)
  let _agent2Id ← db.insertAgent (mkAgent projectId "Agent2" now)
  -- Agent1 acquires build slot
  let _ ← db.insertBuildSlot projectId agent1Id "build" now expires
  -- Agent2 tries to acquire same slot
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("agent_name", Lean.Json.str "Agent2"),
    ("slot_name", Lean.Json.str "build")
  ]
  let req : JsonRpc.Request := {
    method := "acquire_build_slot"
    params := some params
    id := some (JsonRpc.RequestId.num 2)
  }
  let resp ← handleAcquireBuildSlot db testConfig req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some result =>
      match result.getObjValAs? Bool "granted" with
      | Except.ok granted => shouldSatisfy (!granted) "should not be granted"
      | Except.error e => throw (IO.userError s!"Expected granted: {e}")
      match result.getObjValAs? String "holder_agent" with
      | Except.ok holder => holder ≡ "Agent1"
      | Except.error e => throw (IO.userError s!"Expected holder_agent: {e}")
      match result.getObjValAs? Int "retry_after_seconds" with
      | Except.ok retryAfter => shouldSatisfy (retryAfter > 0) "should have retry_after_seconds"
      | Except.error e => throw (IO.userError s!"Expected retry_after_seconds: {e}")
  | none => throw (IO.userError "Expected result")
  db.close

test "acquire_build_slot succeeds when same agent already holds" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let expires := Chronos.Timestamp.fromSeconds 2000000000
  let projectId ← db.insertProject "p1" "/p1" now
  let agent1Id ← db.insertAgent (mkAgent projectId "Agent1" now)
  -- Agent1 acquires build slot
  let existingId ← db.insertBuildSlot projectId agent1Id "build" now expires
  -- Agent1 tries to acquire same slot again (should succeed with existing slot info)
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("agent_name", Lean.Json.str "Agent1"),
    ("slot_name", Lean.Json.str "build")
  ]
  let req : JsonRpc.Request := {
    method := "acquire_build_slot"
    params := some params
    id := some (JsonRpc.RequestId.num 3)
  }
  let resp ← handleAcquireBuildSlot db testConfig req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some result =>
      match result.getObjValAs? Bool "granted" with
      | Except.ok granted => shouldSatisfy granted "should be granted"
      | Except.error e => throw (IO.userError s!"Expected granted: {e}")
      match result.getObjValAs? Nat "slot_id" with
      | Except.ok slotId => slotId ≡ existingId
      | Except.error e => throw (IO.userError s!"Expected slot_id: {e}")
  | none => throw (IO.userError "Expected result")
  db.close

test "acquire_build_slot allows different slot names" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let expires := Chronos.Timestamp.fromSeconds 2000000000
  let projectId ← db.insertProject "p1" "/p1" now
  let agent1Id ← db.insertAgent (mkAgent projectId "Agent1" now)
  let _agent2Id ← db.insertAgent (mkAgent projectId "Agent2" now)
  -- Agent1 acquires "build" slot
  let _ ← db.insertBuildSlot projectId agent1Id "build" now expires
  -- Agent2 tries to acquire "deploy" slot (should succeed)
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("agent_name", Lean.Json.str "Agent2"),
    ("slot_name", Lean.Json.str "deploy")
  ]
  let req : JsonRpc.Request := {
    method := "acquire_build_slot"
    params := some params
    id := some (JsonRpc.RequestId.num 4)
  }
  let resp ← handleAcquireBuildSlot db testConfig req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some result =>
      match result.getObjValAs? Bool "granted" with
      | Except.ok granted => shouldSatisfy granted "should be granted for different slot name"
      | Except.error e => throw (IO.userError s!"Expected granted: {e}")
      match result.getObjValAs? String "slot_name" with
      | Except.ok slotName => slotName ≡ "deploy"
      | Except.error e => throw (IO.userError s!"Expected slot_name: {e}")
  | none => throw (IO.userError "Expected result")
  db.close

test "renew_build_slot extends TTL" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let expires := Chronos.Timestamp.fromSeconds 2000000000
  let projectId ← db.insertProject "p1" "/p1" now
  let agentId ← db.insertAgent (mkAgent projectId "Agent1" now)
  let slotId ← db.insertBuildSlot projectId agentId "build" now expires
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("agent_name", Lean.Json.str "Agent1"),
    ("slot_name", Lean.Json.str "build"),
    ("additional_seconds", Lean.Json.num 3600)
  ]
  let req : JsonRpc.Request := {
    method := "renew_build_slot"
    params := some params
    id := some (JsonRpc.RequestId.num 5)
  }
  let resp ← handleRenewBuildSlot db testConfig req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some result =>
      match result.getObjValAs? Bool "renewed" with
      | Except.ok renewed => shouldSatisfy renewed "should be renewed"
      | Except.error e => throw (IO.userError s!"Expected renewed: {e}")
      match result.getObjValAs? Int "old_expires_ts" with
      | Except.ok oldExpires => oldExpires ≡ (2000000000 : Int)
      | Except.error e => throw (IO.userError s!"Expected old_expires_ts: {e}")
      match result.getObjValAs? Int "new_expires_ts" with
      | Except.ok newExpires => newExpires ≡ (2000000000 + 3600 : Int)
      | Except.error e => throw (IO.userError s!"Expected new_expires_ts: {e}")
  | none => throw (IO.userError "Expected result")
  -- Verify extended
  let found ← db.queryBuildSlotById slotId
  match found with
  | some s => s.expiresTs.seconds ≡ (2000000000 + 3600 : Int)
  | none => throw (IO.userError "Slot not found")
  db.close

test "renew_build_slot fails for non-holder" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let expires := Chronos.Timestamp.fromSeconds 2000000000
  let projectId ← db.insertProject "p1" "/p1" now
  let agent1Id ← db.insertAgent (mkAgent projectId "Agent1" now)
  let _agent2Id ← db.insertAgent (mkAgent projectId "Agent2" now)
  let _ ← db.insertBuildSlot projectId agent1Id "build" now expires
  -- Agent2 tries to renew Agent1's slot
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("agent_name", Lean.Json.str "Agent2"),
    ("slot_name", Lean.Json.str "build")
  ]
  let req : JsonRpc.Request := {
    method := "renew_build_slot"
    params := some params
    id := some (JsonRpc.RequestId.num 6)
  }
  let resp ← handleRenewBuildSlot db testConfig req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.error with
  | some err =>
      err.message ≡ "Invalid params"
      match err.data with
      | some (Lean.Json.str details) =>
          details ≡ "only slot holder can renew"
      | _ => throw (IO.userError "Expected error details string")
  | none => throw (IO.userError "Expected error response")
  db.close

test "release_build_slot releases slot" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let expires := Chronos.Timestamp.fromSeconds 2000000000
  let projectId ← db.insertProject "p1" "/p1" now
  let agentId ← db.insertAgent (mkAgent projectId "Agent1" now)
  let slotId ← db.insertBuildSlot projectId agentId "build" now expires
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("agent_name", Lean.Json.str "Agent1"),
    ("slot_name", Lean.Json.str "build")
  ]
  let req : JsonRpc.Request := {
    method := "release_build_slot"
    params := some params
    id := some (JsonRpc.RequestId.num 7)
  }
  let resp ← handleReleaseBuildSlot db testConfig req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some result =>
      match result.getObjValAs? Bool "released" with
      | Except.ok released => shouldSatisfy released "should be released"
      | Except.error e => throw (IO.userError s!"Expected released: {e}")
      match result.getObjValAs? String "slot_name" with
      | Except.ok slotName => slotName ≡ "build"
      | Except.error e => throw (IO.userError s!"Expected slot_name: {e}")
  | none => throw (IO.userError "Expected result")
  -- Verify released
  let found ← db.queryBuildSlotById slotId
  match found with
  | some s => shouldSatisfy s.releasedTs.isSome "should be released"
  | none => throw (IO.userError "Slot not found")
  db.close

test "release_build_slot fails for non-holder" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let expires := Chronos.Timestamp.fromSeconds 2000000000
  let projectId ← db.insertProject "p1" "/p1" now
  let agent1Id ← db.insertAgent (mkAgent projectId "Agent1" now)
  let _agent2Id ← db.insertAgent (mkAgent projectId "Agent2" now)
  let _ ← db.insertBuildSlot projectId agent1Id "build" now expires
  -- Agent2 tries to release Agent1's slot
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("agent_name", Lean.Json.str "Agent2"),
    ("slot_name", Lean.Json.str "build")
  ]
  let req : JsonRpc.Request := {
    method := "release_build_slot"
    params := some params
    id := some (JsonRpc.RequestId.num 8)
  }
  let resp ← handleReleaseBuildSlot db testConfig req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.error with
  | some err =>
      err.message ≡ "Invalid params"
      match err.data with
      | some (Lean.Json.str details) =>
          details ≡ "only slot holder can release"
      | _ => throw (IO.userError "Expected error details string")
  | none => throw (IO.userError "Expected error response")
  db.close

test "expired slot can be acquired by new agent" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let expired := Chronos.Timestamp.fromSeconds 1600000000  -- In the past
  let projectId ← db.insertProject "p1" "/p1" now
  let agent1Id ← db.insertAgent (mkAgent projectId "Agent1" now)
  let _agent2Id ← db.insertAgent (mkAgent projectId "Agent2" now)
  -- Agent1 had the build slot but it expired
  let _ ← db.insertBuildSlot projectId agent1Id "build" now expired
  -- Agent2 should be able to acquire it
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("agent_name", Lean.Json.str "Agent2"),
    ("slot_name", Lean.Json.str "build")
  ]
  let req : JsonRpc.Request := {
    method := "acquire_build_slot"
    params := some params
    id := some (JsonRpc.RequestId.num 9)
  }
  let resp ← handleAcquireBuildSlot db testConfig req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some result =>
      match result.getObjValAs? Bool "granted" with
      | Except.ok granted => shouldSatisfy granted "should be granted after expiration"
      | Except.error e => throw (IO.userError s!"Expected granted: {e}")
  | none => throw (IO.userError "Expected result")
  db.close

test "release_build_slot is idempotent for non-existent slot" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "p1" "/p1" now
  let _agentId ← db.insertAgent (mkAgent projectId "Agent1" now)
  -- Try to release a slot that was never acquired
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("agent_name", Lean.Json.str "Agent1"),
    ("slot_name", Lean.Json.str "build")
  ]
  let req : JsonRpc.Request := {
    method := "release_build_slot"
    params := some params
    id := some (JsonRpc.RequestId.num 10)
  }
  let resp ← handleReleaseBuildSlot db testConfig req
  let rpcResp ← parseJsonRpcResponse resp
  -- Should succeed (idempotent)
  match rpcResp.result with
  | some result =>
      match result.getObjValAs? Bool "released" with
      | Except.ok released => shouldSatisfy released "should return released=true for idempotency"
      | Except.error e => throw (IO.userError s!"Expected released: {e}")
  | none => throw (IO.userError "Expected result")
  db.close

test "build slot uniqueness prevents duplicate active slots" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let expires := Chronos.Timestamp.fromSeconds 2000000000
  let projectId ← db.insertProject "p1" "/p1" now
  let agent1Id ← db.insertAgent (mkAgent projectId "Agent1" now)
  let agent2Id ← db.insertAgent (mkAgent projectId "Agent2" now)
  let _ ← db.insertBuildSlot projectId agent1Id "build" now expires
  let threw ← try
    let _ ← db.insertBuildSlot projectId agent2Id "build" now expires
    pure false
  catch _ =>
    pure true
  shouldSatisfy threw "should reject duplicate active slot"
  db.close

end Tests.BuildSlots
