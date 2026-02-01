import Crucible
import AgentMail

open Crucible
open AgentMail
open Citadel

namespace Tests.Resources.FileReservations

testSuite "Resources.FileReservations"

def mkAgent (projectId : Nat) (name : String) (now : Chronos.Timestamp) : Agent := {
  id := 0
  projectId := projectId
  name := name
  program := "test"
  model := "test"
  taskDescription := "test task"
  contactPolicy := ContactPolicy.auto
  attachmentsPolicy := AttachmentsPolicy.auto
  inceptionTs := now
  lastActiveTs := now
}

def parseJsonResponse (resp : Response) : IO Lean.Json := do
  let body := String.fromUTF8! resp.body
  match Lean.Json.parse body with
  | Except.ok j => pure j
  | Except.error e => throw (IO.userError s!"Failed to parse JSON: {e}")

def mkRequest (pathParams : List (String × String)) : ServerRequest := {
  request := {
    method := .GET
    path := "/"
    version := .http11
    headers := Herald.Core.Headers.empty
    body := ByteArray.empty
  }
  params := pathParams
}

test "handleFileReservations returns 404 for missing project" := do
  let db ← Storage.Database.openMemory
  let cfg := Config.default
  let req := mkRequest [("slug", "nonexistent")]
  let resp ← Resources.FileReservations.handleFileReservations db cfg req
  resp.status.code ≡ (404 : UInt16)
  db.close

test "handleFileReservations returns empty list" := do
  let db ← Storage.Database.openMemory
  let cfg := Config.default
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let _ ← db.insertProject "p1" "/my/project" now
  let req := mkRequest [("slug", "p1")]
  let resp ← Resources.FileReservations.handleFileReservations db cfg req
  resp.status.code ≡ (200 : UInt16)
  let json ← parseJsonResponse resp
  match json.getObjVal? "count" with
  | Except.ok (Lean.Json.num n) => n ≡ (0 : Nat)
  | _ => throw (IO.userError "Expected count field")
  db.close

test "handleFileReservations returns active reservations" := do
  let db ← Storage.Database.openMemory
  let cfg := Config.default
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let expires := Chronos.Timestamp.fromSeconds 2000000000  -- Far future
  let projectId ← db.insertProject "p1" "/my/project" now
  let agentId ← db.insertAgent (mkAgent projectId "Agent1" now)
  let _ ← db.insertFileReservation projectId agentId "src/*.lean" true "Testing" now expires
  let req := mkRequest [("slug", "p1")]
  let resp ← Resources.FileReservations.handleFileReservations db cfg req
  resp.status.code ≡ (200 : UInt16)
  let json ← parseJsonResponse resp
  match json.getObjVal? "count" with
  | Except.ok (Lean.Json.num n) => n ≡ (1 : Nat)
  | _ => throw (IO.userError "Expected count field")
  match json.getObjVal? "reservations" with
  | Except.ok (Lean.Json.arr reservations) =>
    match reservations.toList with
    | entry :: _ =>
      match entry.getObjValAs? String "path_pattern" with
      | Except.ok pattern => pattern ≡ "src/*.lean"
      | Except.error e => throw (IO.userError s!"Expected path_pattern: {e}")
      match entry.getObjValAs? String "agent_name" with
      | Except.ok name => name ≡ "Agent1"
      | Except.error e => throw (IO.userError s!"Expected agent_name: {e}")
    | [] => throw (IO.userError "Expected reservation entry")
  | _ => throw (IO.userError "Expected reservations array")
  db.close

test "handleFileReservations excludes expired reservations" := do
  let db ← Storage.Database.openMemory
  let cfg := Config.default
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let expired := Chronos.Timestamp.fromSeconds 1600000000  -- Past
  let projectId ← db.insertProject "p1" "/my/project" now
  let agentId ← db.insertAgent (mkAgent projectId "Agent1" now)
  let _ ← db.insertFileReservation projectId agentId "src/*.lean" true "Testing" now expired
  let req := mkRequest [("slug", "p1")]
  let resp ← Resources.FileReservations.handleFileReservations db cfg req
  resp.status.code ≡ (200 : UInt16)
  let json ← parseJsonResponse resp
  match json.getObjVal? "count" with
  | Except.ok (Lean.Json.num n) => n ≡ (0 : Nat)
  | _ => throw (IO.userError "Expected count field")
  db.close

end Tests.Resources.FileReservations
