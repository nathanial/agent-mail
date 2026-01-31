import Crucible
import Chronos
import AgentMail

open Crucible
open AgentMail

namespace Tests.Types

testSuite "Types"

test "ContactPolicy roundtrip" := do
  let policies := #[ContactPolicy.openPolicy, ContactPolicy.auto, ContactPolicy.contactsOnly, ContactPolicy.blockAll]
  for p in policies do
    let str := p.toString
    let parsed := ContactPolicy.fromString? str
    parsed ≡ some p

test "ContactPolicy JSON roundtrip" := do
  let p := ContactPolicy.auto
  let json := Lean.toJson p
  let parsed : Except String ContactPolicy := Lean.FromJson.fromJson? json
  match parsed with
  | Except.ok q => q ≡ p
  | Except.error e => throw (IO.userError s!"Failed to parse: {e}")

test "AttachmentsPolicy roundtrip" := do
  let policies := #[AttachmentsPolicy.accept, AttachmentsPolicy.reject, AttachmentsPolicy.linksOnly]
  for p in policies do
    let str := p.toString
    let parsed := AttachmentsPolicy.fromString? str
    parsed ≡ some p

test "Importance roundtrip" := do
  let levels := #[Importance.low, Importance.normal, Importance.high, Importance.urgent]
  for i in levels do
    let str := i.toString
    let parsed := Importance.fromString? str
    parsed ≡ some i

test "RecipientType roundtrip" := do
  let types := #[RecipientType.toRecipient, RecipientType.cc, RecipientType.bcc]
  for t in types do
    let str := t.toString
    let parsed := RecipientType.fromString? str
    parsed ≡ some t

end Tests.Types

namespace Tests.Project

testSuite "Project"

test "JSON roundtrip" := do
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let project : Project := {
    id := 1
    slug := "test-project"
    humanKey := "Test Project"
    createdAt := now
  }
  let json := Lean.toJson project
  let parsed : Except String Project := Lean.FromJson.fromJson? json
  match parsed with
  | Except.ok p =>
    p.id ≡ project.id
    p.slug ≡ project.slug
    p.humanKey ≡ project.humanKey
    p.createdAt.seconds ≡ project.createdAt.seconds
  | Except.error e => throw (IO.userError s!"Failed to parse: {e}")

end Tests.Project

namespace Tests.Agent

testSuite "Agent"

test "JSON roundtrip" := do
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let agent : Agent := {
    id := 1
    projectId := 1
    name := "builder"
    program := "claude-code"
    model := "opus-4.5"
    taskDescription := "Building features"
    contactPolicy := ContactPolicy.openPolicy
    attachmentsPolicy := AttachmentsPolicy.accept
    inceptionTs := now
    lastActiveTs := now
  }
  let json := Lean.toJson agent
  let parsed : Except String Agent := Lean.FromJson.fromJson? json
  match parsed with
  | Except.ok a =>
    a.id ≡ agent.id
    a.name ≡ agent.name
    a.contactPolicy ≡ agent.contactPolicy
  | Except.error e => throw (IO.userError s!"Failed to parse: {e}")

end Tests.Agent

namespace Tests.Message

testSuite "Message"

test "JSON roundtrip" := do
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let msg : Message := {
    id := 1
    projectId := 1
    senderId := 1
    subject := "Test subject"
    bodyMd := "Test body"
    importance := Importance.normal
    ackRequired := false
    threadId := some "thread-123"
    createdTs := now
  }
  let json := Lean.toJson msg
  let parsed : Except String Message := Lean.FromJson.fromJson? json
  match parsed with
  | Except.ok m =>
    m.id ≡ msg.id
    m.subject ≡ msg.subject
    m.threadId ≡ msg.threadId
  | Except.error e => throw (IO.userError s!"Failed to parse: {e}")

test "Message without threadId" := do
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let msg : Message := {
    id := 1
    projectId := 1
    senderId := 1
    subject := "Test"
    bodyMd := "Body"
    importance := Importance.low
    ackRequired := true
    threadId := none
    createdTs := now
  }
  let json := Lean.toJson msg
  let parsed : Except String Message := Lean.FromJson.fromJson? json
  match parsed with
  | Except.ok m => shouldSatisfy m.threadId.isNone "threadId should be none"
  | Except.error e => throw (IO.userError s!"Failed to parse: {e}")

end Tests.Message

namespace Tests.FileReservation

testSuite "FileReservation"

test "JSON roundtrip" := do
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let later := Chronos.Timestamp.fromSeconds 1700003600
  let res : FileReservation := {
    id := 1
    projectId := 1
    agentId := 1
    pathPattern := "src/**/*.lean"
    exclusive := true
    reason := "Refactoring module"
    createdTs := now
    expiresTs := later
    releasedTs := none
  }
  let json := Lean.toJson res
  let parsed : Except String FileReservation := Lean.FromJson.fromJson? json
  match parsed with
  | Except.ok r =>
    r.id ≡ res.id
    r.pathPattern ≡ res.pathPattern
    r.exclusive ≡ res.exclusive
  | Except.error e => throw (IO.userError s!"Failed to parse: {e}")

test "isActive check" := do
  let now := Chronos.Timestamp.fromSeconds 1700001000
  let res : FileReservation := {
    id := 1
    projectId := 1
    agentId := 1
    pathPattern := "*.lean"
    exclusive := true
    reason := ""
    createdTs := Chronos.Timestamp.fromSeconds 1700000000
    expiresTs := Chronos.Timestamp.fromSeconds 1700002000
    releasedTs := none
  }
  shouldSatisfy (res.isActive now) "should be active"
  -- After expiry
  let later := Chronos.Timestamp.fromSeconds 1700003000
  shouldSatisfy (not (res.isActive later)) "should be expired"
  -- If released
  let released := { res with releasedTs := some now }
  shouldSatisfy (not (released.isActive now)) "should be released"

end Tests.FileReservation

namespace Tests.JsonRpc

testSuite "JsonRpc"

test "Request parsing" := do
  let json := Lean.Json.parse "{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"id\":1}"
  match json with
  | Except.ok j =>
    let req : Except String JsonRpc.Request := Lean.FromJson.fromJson? j
    match req with
    | Except.ok r =>
      r.method ≡ "test"
      r.id ≡ some (JsonRpc.RequestId.num 1)
    | Except.error e => throw (IO.userError s!"Failed to parse request: {e}")
  | Except.error e => throw (IO.userError s!"Failed to parse JSON: {e}")

test "Request with params" := do
  let json := Lean.Json.parse "{\"jsonrpc\":\"2.0\",\"method\":\"add\",\"params\":{\"a\":1,\"b\":2},\"id\":\"req-1\"}"
  match json with
  | Except.ok j =>
    let req : Except String JsonRpc.Request := Lean.FromJson.fromJson? j
    match req with
    | Except.ok r =>
      r.method ≡ "add"
      shouldSatisfy r.params.isSome "params should be present"
      r.id ≡ some (JsonRpc.RequestId.str "req-1")
    | Except.error e => throw (IO.userError s!"Failed to parse request: {e}")
  | Except.error e => throw (IO.userError s!"Failed to parse JSON: {e}")

test "Notification (no id)" := do
  let json := Lean.Json.parse "{\"jsonrpc\":\"2.0\",\"method\":\"notify\"}"
  match json with
  | Except.ok j =>
    let req : Except String JsonRpc.Request := Lean.FromJson.fromJson? j
    match req with
    | Except.ok r =>
      r.method ≡ "notify"
      shouldSatisfy r.isNotification "should be notification"
    | Except.error e => throw (IO.userError s!"Failed to parse request: {e}")
  | Except.error e => throw (IO.userError s!"Failed to parse JSON: {e}")

test "Error creation" := do
  let err := JsonRpc.Error.methodNotFound "unknown_method"
  err.code ≡ JsonRpc.errorMethodNotFound
  err.message ≡ "Method not found: unknown_method"

test "Response success" := do
  let resp := JsonRpc.Response.success (some (JsonRpc.RequestId.num 1)) (Lean.Json.str "ok")
  let json := Lean.toJson resp
  let str := Lean.Json.compress json
  shouldSatisfy (str.find? "result" |>.isSome) "should contain result"
  shouldSatisfy (str.find? "error" |>.isNone) "should not contain error"

test "Response failure" := do
  let err := JsonRpc.Error.invalidParams (some "missing field")
  let resp := JsonRpc.Response.failure (some (JsonRpc.RequestId.num 1)) err
  let json := Lean.toJson resp
  let str := Lean.Json.compress json
  shouldSatisfy (str.find? "error" |>.isSome) "should contain error"
  shouldSatisfy (str.find? "-32602" |>.isSome) "should contain error code"

end Tests.JsonRpc

namespace Tests.Config

testSuite "Config"

test "Default config" := do
  let cfg := Config.default
  cfg.port ≡ 8765
  cfg.host ≡ "127.0.0.1"
  cfg.databasePath ≡ "agent_mail.db"
  shouldSatisfy cfg.authToken.isNone "authToken should be none"

test "Display hides token" := do
  let cfg := { Config.default with authToken := some "secret" }
  let display := cfg.display
  shouldSatisfy (display.find? "(set)" |>.isSome) "should show (set)"
  shouldSatisfy (display.find? "secret" |>.isNone) "should hide secret"

end Tests.Config

namespace Tests.Database

testSuite "Database"

test "Schema initialization" := do
  let db ← Storage.Database.openMemory
  -- Verify tables exist by querying sqlite_master
  let rows ← db.query "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name"
  let tableNames := rows.map fun row =>
    match row.get? 0 with
    | some (Quarry.Value.text name) => name
    | _ => ""
  shouldSatisfy (tableNames.contains "projects") "should have projects table"
  shouldSatisfy (tableNames.contains "agents") "should have agents table"
  shouldSatisfy (tableNames.contains "messages") "should have messages table"
  shouldSatisfy (tableNames.contains "message_recipients") "should have message_recipients table"
  shouldSatisfy (tableNames.contains "file_reservations") "should have file_reservations table"
  db.close

test "Insert and query project" := do
  let db ← Storage.Database.openMemory
  let now := 1700000000
  let id ← db.insert s!"INSERT INTO projects (slug, human_key, created_at) VALUES ('test', 'Test', {now})"
  id ≡ (1 : Int)
  let row ← db.queryOne "SELECT slug, human_key FROM projects WHERE id = 1"
  match row with
  | some r =>
    match (r.get? 0, r.get? 1) with
    | (some (Quarry.Value.text slug), some (Quarry.Value.text humanKey)) =>
      slug ≡ "test"
      humanKey ≡ "Test"
    | _ => throw (IO.userError "Unexpected column types")
  | none => throw (IO.userError "Project not found")
  db.close

end Tests.Database

def main : IO UInt32 := runAllSuites
