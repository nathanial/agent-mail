import Crucible
import AgentMail

open Crucible
open AgentMail

namespace Tests.FileReservationTools

testSuite "FileReservationTools"

open Citadel
open AgentMail.Tools.FileReservations

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

test "patternsOverlap detects identical patterns" := do
  shouldSatisfy (patternsOverlap "src/*.lean" "src/*.lean") "identical patterns should overlap"

test "patternsOverlap detects prefix patterns" := do
  shouldSatisfy (patternsOverlap "src/" "src/foo.lean") "prefix should overlap"
  shouldSatisfy (patternsOverlap "src/foo.lean" "src/") "prefix should overlap (reverse)"

test "patternsOverlap with wildcards" := do
  shouldSatisfy (patternsOverlap "*.lean" "foo.lean") "wildcard at start overlaps"
  shouldSatisfy (patternsOverlap "src/*" "src/foo") "directory wildcard overlaps"

test "patternsOverlap non-overlapping" := do
  shouldSatisfy (not (patternsOverlap "src/foo.lean" "docs/bar.md")) "different dirs don't overlap"

test "file_reservation_paths grants reservations" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "p1" "/p1" now
  let _agent1Id ← db.insertAgent (mkAgent projectId "Agent1" now)
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("agent_name", Lean.Json.str "Agent1"),
    ("paths", Lean.toJson #["src/*.lean", "docs/*.md"]),
    ("ttl_seconds", Lean.Json.num 7200),
    ("exclusive", Lean.Json.bool true)
  ]
  let req : JsonRpc.Request := {
    method := "file_reservation_paths"
    params := some params
    id := some (JsonRpc.RequestId.num 1)
  }
  let resp ← handleFileReservationPaths db testConfig req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some result =>
      match result.getObjVal? "granted" with
      | Except.ok (Lean.Json.arr granted) =>
          granted.size ≡ (2 : Nat)
          match granted.toList with
          | entry :: _ =>
              match entry.getObjValAs? Bool "exclusive" with
              | Except.ok isExclusive => shouldSatisfy isExclusive "exclusive should be true"
              | Except.error e => throw (IO.userError s!"Expected exclusive: {e}")
          | [] => throw (IO.userError "Expected granted entry")
      | _ => throw (IO.userError "Expected granted array")
      match result.getObjVal? "conflicts" with
      | Except.ok (Lean.Json.arr conflicts) =>
          conflicts.size ≡ (0 : Nat)
      | _ => throw (IO.userError "Expected conflicts array")
  | none => throw (IO.userError "Expected result")
  db.close

test "file_reservation_paths detects conflicts" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  -- Use a far-future expiration to ensure reservation is still active
  let expires := Chronos.Timestamp.fromSeconds 2000000000  -- Year 2033
  let projectId ← db.insertProject "p1" "/p1" now
  let agent1Id ← db.insertAgent (mkAgent projectId "Agent1" now)
  let _agent2Id ← db.insertAgent (mkAgent projectId "Agent2" now)
  -- Agent1 reserves src/*.lean
  let _ ← db.insertFileReservation projectId agent1Id "src/*.lean" true "" now expires
  -- Agent2 tries to reserve overlapping pattern
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("agent_name", Lean.Json.str "Agent2"),
    ("paths", Lean.toJson #["src/*.lean", "docs/*.md"]),
    ("exclusive", Lean.Json.bool true)
  ]
  let req : JsonRpc.Request := {
    method := "file_reservation_paths"
    params := some params
    id := some (JsonRpc.RequestId.num 2)
  }
  let resp ← handleFileReservationPaths db testConfig req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some result =>
      match result.getObjVal? "granted" with
      | Except.ok (Lean.Json.arr granted) =>
          granted.size ≡ (2 : Nat)
      | _ => throw (IO.userError "Expected granted array")
      match result.getObjVal? "conflicts" with
      | Except.ok (Lean.Json.arr conflicts) =>
          conflicts.size ≡ (1 : Nat)
          match conflicts.toList with
          | conflictEntry :: _ =>
              match conflictEntry.getObjValAs? String "path" with
              | Except.ok path => path ≡ "src/*.lean"
              | Except.error e => throw (IO.userError s!"Expected conflict path: {e}")
              match conflictEntry.getObjVal? "holders" with
              | Except.ok (Lean.Json.arr holders) =>
                  holders.size ≡ (1 : Nat)
              | _ => throw (IO.userError "Expected holders array")
          | [] => throw (IO.userError "Expected conflict entry")
      | _ => throw (IO.userError "Expected conflicts array")
  | none => throw (IO.userError "Expected result")
  db.close

test "file_reservation_paths allows shared reservations" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let expires := Chronos.Timestamp.fromSeconds 2000000000
  let projectId ← db.insertProject "p1" "/p1" now
  let agent1Id ← db.insertAgent (mkAgent projectId "Agent1" now)
  let _agent2Id ← db.insertAgent (mkAgent projectId "Agent2" now)
  -- Agent1 reserves src/*.lean as NON-exclusive
  let _ ← db.insertFileReservation projectId agent1Id "src/*.lean" false "" now expires
  -- Agent2 tries to reserve same pattern as NON-exclusive
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("agent_name", Lean.Json.str "Agent2"),
    ("paths", Lean.toJson #["src/*.lean"]),
    ("exclusive", Lean.Json.bool false)
  ]
  let req : JsonRpc.Request := {
    method := "file_reservation_paths"
    params := some params
    id := some (JsonRpc.RequestId.num 3)
  }
  let resp ← handleFileReservationPaths db testConfig req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some result =>
      match result.getObjVal? "granted" with
      | Except.ok (Lean.Json.arr granted) =>
          -- Should be granted since both are non-exclusive
          granted.size ≡ (1 : Nat)
      | _ => throw (IO.userError "Expected granted array")
      match result.getObjVal? "conflicts" with
      | Except.ok (Lean.Json.arr conflicts) =>
          conflicts.size ≡ (0 : Nat)
      | _ => throw (IO.userError "Expected conflicts array")
  | none => throw (IO.userError "Expected result")
  db.close

test "release_file_reservations by IDs" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let expires := Chronos.Timestamp.fromSeconds 2000000000
  let projectId ← db.insertProject "p1" "/p1" now
  let agentId ← db.insertAgent (mkAgent projectId "Agent1" now)
  let resId ← db.insertFileReservation projectId agentId "src/*.lean" true "" now expires
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("agent_name", Lean.Json.str "Agent1"),
    ("file_reservation_ids", Lean.toJson #[resId])
  ]
  let req : JsonRpc.Request := {
    method := "release_file_reservations"
    params := some params
    id := some (JsonRpc.RequestId.num 4)
  }
  let resp ← handleReleaseFileReservations db testConfig req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some result =>
      match result.getObjValAs? Nat "released" with
      | Except.ok released => released ≡ (1 : Nat)
      | Except.error e => throw (IO.userError s!"Expected released count: {e}")
  | none => throw (IO.userError "Expected result")
  -- Verify released
  let found ← db.queryFileReservationById resId
  match found with
  | some r => shouldSatisfy r.releasedTs.isSome "should be released"
  | none => throw (IO.userError "Reservation not found")
  db.close

test "release_file_reservations by paths" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let expires := Chronos.Timestamp.fromSeconds 2000000000
  let projectId ← db.insertProject "p1" "/p1" now
  let agentId ← db.insertAgent (mkAgent projectId "Agent1" now)
  let _ ← db.insertFileReservation projectId agentId "src/*.lean" true "" now expires
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("agent_name", Lean.Json.str "Agent1"),
    ("paths", Lean.toJson #["src/*.lean"])
  ]
  let req : JsonRpc.Request := {
    method := "release_file_reservations"
    params := some params
    id := some (JsonRpc.RequestId.num 5)
  }
  let resp ← handleReleaseFileReservations db testConfig req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some result =>
      match result.getObjValAs? Nat "released" with
      | Except.ok released => released ≡ (1 : Nat)
      | Except.error e => throw (IO.userError s!"Expected released count: {e}")
  | none => throw (IO.userError "Expected result")
  db.close

test "renew_file_reservations extends TTL" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let expires := Chronos.Timestamp.fromSeconds 2000000000
  let projectId ← db.insertProject "p1" "/p1" now
  let agentId ← db.insertAgent (mkAgent projectId "Agent1" now)
  let resId ← db.insertFileReservation projectId agentId "src/*.lean" true "" now expires
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("agent_name", Lean.Json.str "Agent1"),
    ("file_reservation_ids", Lean.toJson #[resId]),
    ("extend_seconds", Lean.Json.num 3600)
  ]
  let req : JsonRpc.Request := {
    method := "renew_file_reservations"
    params := some params
    id := some (JsonRpc.RequestId.num 6)
  }
  let resp ← handleRenewFileReservations db testConfig req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some result =>
      match result.getObjValAs? Nat "renewed" with
      | Except.ok renewed => renewed ≡ (1 : Nat)
      | Except.error e => throw (IO.userError s!"Expected renewed count: {e}")
      match result.getObjVal? "file_reservations" with
      | Except.ok (Lean.Json.arr updated) =>
          updated.size ≡ (1 : Nat)
      | _ => throw (IO.userError "Expected file_reservations array")
  | none => throw (IO.userError "Expected result")
  -- Verify extended
  let found ← db.queryFileReservationById resId
  match found with
  | some r => r.expiresTs.seconds ≡ (2000000000 + 3600 : Int)
  | none => throw (IO.userError "Reservation not found")
  db.close

test "renew_file_reservations fails for other agent's reservations" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let expires := Chronos.Timestamp.fromSeconds 2000000000
  let projectId ← db.insertProject "p1" "/p1" now
  let agent1Id ← db.insertAgent (mkAgent projectId "Agent1" now)
  let _agent2Id ← db.insertAgent (mkAgent projectId "Agent2" now)
  let resId ← db.insertFileReservation projectId agent1Id "src/*.lean" true "" now expires
  -- Agent2 tries to renew Agent1's reservation
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("agent_name", Lean.Json.str "Agent2"),
    ("file_reservation_ids", Lean.toJson #[resId])
  ]
  let req : JsonRpc.Request := {
    method := "renew_file_reservations"
    params := some params
    id := some (JsonRpc.RequestId.num 7)
  }
  let resp ← handleRenewFileReservations db testConfig req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some result =>
      match result.getObjValAs? Nat "renewed" with
      | Except.ok renewed => renewed ≡ (0 : Nat)
      | Except.error e => throw (IO.userError s!"Expected renewed count: {e}")
      match result.getObjVal? "file_reservations" with
      | Except.ok (Lean.Json.arr updated) =>
          updated.size ≡ (0 : Nat)
      | _ => throw (IO.userError "Expected file_reservations array")
  | none => throw (IO.userError "Expected result")
  db.close

test "force_release_file_reservation releases any reservation" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let expires := Chronos.Timestamp.fromSeconds 1700003600
  let projectId ← db.insertProject "p1" "/p1" now
  let agentId ← db.insertAgent (mkAgent projectId "Agent1" now)
  let resId ← db.insertFileReservation projectId agentId "src/*.lean" true "" now expires
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("agent_name", Lean.Json.str "Agent1"),
    ("file_reservation_id", Lean.Json.num resId),
    ("note", Lean.Json.str "Admin override")
  ]
  let req : JsonRpc.Request := {
    method := "force_release_file_reservation"
    params := some params
    id := some (JsonRpc.RequestId.num 8)
  }
  let resp ← handleForceReleaseFileReservation db testConfig req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some result =>
      match result.getObjValAs? Nat "released" with
      | Except.ok released => released ≡ (1 : Nat)
      | Except.error e => throw (IO.userError s!"Expected released count: {e}")
      match result.getObjVal? "reservation" with
      | Except.ok reservation =>
          match reservation.getObjValAs? Nat "id" with
          | Except.ok rid => rid ≡ resId
          | Except.error e => throw (IO.userError s!"Expected reservation id: {e}")
      | _ => throw (IO.userError "Expected reservation summary")
  | none => throw (IO.userError "Expected result")
  -- Verify released
  let found ← db.queryFileReservationById resId
  match found with
  | some r => shouldSatisfy r.releasedTs.isSome "should be released"
  | none => throw (IO.userError "Reservation not found")
  db.close

test "force_release_file_reservation rejects wrong project" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let expires := Chronos.Timestamp.fromSeconds 1700003600
  let project1Id ← db.insertProject "p1" "/p1" now
  let project2Id ← db.insertProject "p2" "/p2" now
  let agent1Id ← db.insertAgent (mkAgent project1Id "Agent1" now)
  let _agent2Id ← db.insertAgent (mkAgent project2Id "Agent2" now)
  let resId ← db.insertFileReservation project1Id agent1Id "src/*.lean" true "" now expires
  -- Try to force release from wrong project
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p2"),
    ("agent_name", Lean.Json.str "Agent2"),
    ("file_reservation_id", Lean.Json.num resId),
    ("note", Lean.Json.str "Wrong project test")
  ]
  let req : JsonRpc.Request := {
    method := "force_release_file_reservation"
    params := some params
    id := some (JsonRpc.RequestId.num 9)
  }
  let resp ← handleForceReleaseFileReservation db testConfig req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.error with
  | some err =>
      err.message ≡ "Invalid params"
      match err.data with
      | some (Lean.Json.str details) =>
          details ≡ "reservation does not belong to this project"
      | _ => throw (IO.userError "Expected error details string")
  | none => throw (IO.userError "Expected error response")
  db.close

test "same agent can re-reserve own patterns" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let expires := Chronos.Timestamp.fromSeconds 2000000000
  let projectId ← db.insertProject "p1" "/p1" now
  let agentId ← db.insertAgent (mkAgent projectId "Agent1" now)
  -- Agent1 reserves src/*.lean
  let _ ← db.insertFileReservation projectId agentId "src/*.lean" true "" now expires
  -- Agent1 tries to reserve overlapping pattern (should succeed - same agent)
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("agent_name", Lean.Json.str "Agent1"),
    ("paths", Lean.toJson #["src/*.lean"]),
    ("exclusive", Lean.Json.bool true)
  ]
  let req : JsonRpc.Request := {
    method := "file_reservation_paths"
    params := some params
    id := some (JsonRpc.RequestId.num 10)
  }
  let resp ← handleFileReservationPaths db testConfig req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some result =>
      match result.getObjVal? "granted" with
      | Except.ok (Lean.Json.arr granted) =>
          -- Should be granted since it's the same agent
          granted.size ≡ (1 : Nat)
      | _ => throw (IO.userError "Expected granted array")
      match result.getObjVal? "conflicts" with
      | Except.ok (Lean.Json.arr conflicts) =>
          conflicts.size ≡ (0 : Nat)
      | _ => throw (IO.userError "Expected conflicts array")
  | none => throw (IO.userError "Expected result")
  db.close

end Tests.FileReservationTools
