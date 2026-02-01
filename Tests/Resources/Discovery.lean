import Crucible
import AgentMail

open Crucible
open AgentMail
open Citadel

namespace Tests.Resources.Discovery

testSuite "Resources.Discovery"

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

def mkRequest (params : List (String × String)) : ServerRequest := {
  request := {
    method := .GET
    path := "/"
    version := .http11
    headers := Herald.Core.Headers.empty
    body := ByteArray.empty
  }
  params := params
}

test "handleProjects returns empty list when no projects" := do
  let db ← Storage.Database.openMemory
  let cfg := Config.default
  let resp ← Resources.Discovery.handleProjects db cfg (mkRequest [])
  resp.status.code ≡ (200 : UInt16)
  let json ← parseJsonResponse resp
  match json.getObjVal? "count" with
  | Except.ok (Lean.Json.num n) => n ≡ (0 : Nat)
  | _ => throw (IO.userError "Expected count field")
  db.close

test "handleProjects returns projects" := do
  let db ← Storage.Database.openMemory
  let cfg := Config.default
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let _ ← db.insertProject "p1" "/my/project" now
  let _ ← db.insertProject "p2" "/another/project" now
  let resp ← Resources.Discovery.handleProjects db cfg (mkRequest [])
  resp.status.code ≡ (200 : UInt16)
  let json ← parseJsonResponse resp
  match json.getObjVal? "count" with
  | Except.ok (Lean.Json.num n) => n ≡ (2 : Nat)
  | _ => throw (IO.userError "Expected count field")
  db.close

test "handleProject returns 404 for missing project" := do
  let db ← Storage.Database.openMemory
  let cfg := Config.default
  let req := mkRequest [("slug", "nonexistent")]
  let resp ← Resources.Discovery.handleProject db cfg req
  resp.status.code ≡ (404 : UInt16)
  db.close

test "handleProject returns project with agents" := do
  let db ← Storage.Database.openMemory
  let cfg := Config.default
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "test-project" "/test/project" now
  let _ ← db.insertAgent (mkAgent projectId "Agent1" now)
  let _ ← db.insertAgent (mkAgent projectId "Agent2" now)
  let req := mkRequest [("slug", "test-project")]
  let resp ← Resources.Discovery.handleProject db cfg req
  resp.status.code ≡ (200 : UInt16)
  let json ← parseJsonResponse resp
  match json.getObjVal? "agent_count" with
  | Except.ok (Lean.Json.num n) => n ≡ (2 : Nat)
  | _ => throw (IO.userError "Expected agent_count field")
  db.close

test "handleAgents returns agents for project" := do
  let db ← Storage.Database.openMemory
  let cfg := Config.default
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "p1" "/my/project" now
  let _ ← db.insertAgent (mkAgent projectId "Agent1" now)
  let req := mkRequest [("project_key", "/my/project")]
  let resp ← Resources.Discovery.handleAgents db cfg req
  resp.status.code ≡ (200 : UInt16)
  let json ← parseJsonResponse resp
  match json.getObjVal? "count" with
  | Except.ok (Lean.Json.num n) => n ≡ (1 : Nat)
  | _ => throw (IO.userError "Expected count field")
  db.close

test "handleIdentity resolves project and agents" := do
  let db ← Storage.Database.openMemory
  let cfg := Config.default
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "p1" "/my/project" now
  let _ ← db.insertAgent (mkAgent projectId "Alice" now)
  let _ ← db.insertAgent (mkAgent projectId "Bob" now)
  let req := mkRequest [("project", "/my/project")]
  let resp ← Resources.Discovery.handleIdentity db cfg req
  resp.status.code ≡ (200 : UInt16)
  let json ← parseJsonResponse resp
  match json.getObjVal? "agent_count" with
  | Except.ok (Lean.Json.num n) => n ≡ (2 : Nat)
  | _ => throw (IO.userError "Expected agent_count field")
  db.close

test "handleProduct returns product with linked projects" := do
  let db ← Storage.Database.openMemory
  let cfg := Config.default
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let productDbId ← db.insertProduct "my-product" now
  let projectId ← db.insertProject "p1" "/project1" now
  let _ ← db.insertProductProject productDbId projectId now
  let req := mkRequest [("key", "my-product")]
  let resp ← Resources.Discovery.handleProduct db cfg req
  resp.status.code ≡ (200 : UInt16)
  let json ← parseJsonResponse resp
  match json.getObjVal? "project_count" with
  | Except.ok (Lean.Json.num n) => n ≡ (1 : Nat)
  | _ => throw (IO.userError "Expected project_count field")
  db.close

end Tests.Resources.Discovery
