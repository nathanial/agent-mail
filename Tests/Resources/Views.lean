import Crucible
import AgentMail

open Crucible
open AgentMail
open Citadel

namespace Tests.Resources.Views

testSuite "Resources.Views"

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

def mkRequest (pathParams : List (String × String)) (queryParams : List (String × String) := []) : ServerRequest :=
  let queryStr := queryParams.map (fun (k, v) => s!"{k}={v}") |>.intersperse "&" |> String.join
  let path := if queryStr.isEmpty then "/" else s!"/?{queryStr}"
  {
    request := {
      method := .GET
      path := path
      version := .http11
      headers := Herald.Core.Headers.empty
      body := ByteArray.empty
    }
    params := pathParams
  }

test "handleUrgentUnread returns 404 for missing project" := do
  let db ← Storage.Database.openMemory
  let cfg := Config.default
  let req := mkRequest [("agent", "Agent1")] [("project", "/missing")]
  let resp ← Resources.Views.handleUrgentUnread db cfg req
  resp.status.code ≡ (404 : UInt16)
  db.close

test "handleUrgentUnread returns 404 for missing agent" := do
  let db ← Storage.Database.openMemory
  let cfg := Config.default
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let _ ← db.insertProject "p1" "/my/project" now
  let req := mkRequest [("agent", "MissingAgent")] [("project", "/my/project")]
  let resp ← Resources.Views.handleUrgentUnread db cfg req
  resp.status.code ≡ (404 : UInt16)
  db.close

test "handleUrgentUnread returns empty list" := do
  let db ← Storage.Database.openMemory
  let cfg := Config.default
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "p1" "/my/project" now
  let _ ← db.insertAgent (mkAgent projectId "Agent1" now)
  let req := mkRequest [("agent", "Agent1")] [("project", "/my/project")]
  let resp ← Resources.Views.handleUrgentUnread db cfg req
  resp.status.code ≡ (200 : UInt16)
  let json ← parseJsonResponse resp
  match json.getObjValAs? String "view" with
  | Except.ok viewName => viewName ≡ "urgent-unread"
  | _ => throw (IO.userError "Expected view field")
  match json.getObjVal? "count" with
  | Except.ok (Lean.Json.num n) => n ≡ (0 : Nat)
  | _ => throw (IO.userError "Expected count field")
  db.close

test "handleAckRequired returns empty list" := do
  let db ← Storage.Database.openMemory
  let cfg := Config.default
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "p1" "/my/project" now
  let _ ← db.insertAgent (mkAgent projectId "Agent1" now)
  let req := mkRequest [("agent", "Agent1")] [("project", "/my/project")]
  let resp ← Resources.Views.handleAckRequired db cfg req
  resp.status.code ≡ (200 : UInt16)
  let json ← parseJsonResponse resp
  match json.getObjValAs? String "view" with
  | Except.ok viewName => viewName ≡ "ack-required"
  | _ => throw (IO.userError "Expected view field")
  db.close

test "handleAcksStale returns empty list" := do
  let db ← Storage.Database.openMemory
  let cfg := Config.default
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "p1" "/my/project" now
  let _ ← db.insertAgent (mkAgent projectId "Agent1" now)
  let req := mkRequest [("agent", "Agent1")] [("project", "/my/project")]
  let resp ← Resources.Views.handleAcksStale db cfg req
  resp.status.code ≡ (200 : UInt16)
  let json ← parseJsonResponse resp
  match json.getObjValAs? String "view" with
  | Except.ok viewName => viewName ≡ "acks-stale"
  | _ => throw (IO.userError "Expected view field")
  db.close

test "handleAckOverdue returns empty list" := do
  let db ← Storage.Database.openMemory
  let cfg := Config.default
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "p1" "/my/project" now
  let _ ← db.insertAgent (mkAgent projectId "Agent1" now)
  let req := mkRequest [("agent", "Agent1")] [("project", "/my/project")]
  let resp ← Resources.Views.handleAckOverdue db cfg req
  resp.status.code ≡ (200 : UInt16)
  let json ← parseJsonResponse resp
  match json.getObjValAs? String "view" with
  | Except.ok viewName => viewName ≡ "ack-overdue"
  | _ => throw (IO.userError "Expected view field")
  db.close

end Tests.Resources.Views
