import Crucible
import AgentMail

open Crucible
open AgentMail
open Citadel

namespace Tests.Resources.Mail

testSuite "Resources.Mail"

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

def mkMessage (projectId senderId : Nat) (subject : String) (now : Chronos.Timestamp) : Message := {
  id := 0
  projectId := projectId
  senderId := senderId
  subject := subject
  bodyMd := "Test body"
  attachments := #[]
  importance := Importance.normal
  ackRequired := false
  threadId := none
  createdTs := now
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

test "handleMessage returns 404 for missing message" := do
  let db ← Storage.Database.openMemory
  let cfg := Config.default
  let req := mkRequest [("id", "999")]
  let resp ← Resources.Mail.handleMessage db cfg req
  resp.status.code ≡ (404 : UInt16)
  db.close

test "handleMessage returns message details" := do
  let db ← Storage.Database.openMemory
  let cfg := Config.default
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "p1" "/my/project" now
  let agentId ← db.insertAgent (mkAgent projectId "Sender" now)
  let messageId ← db.insertMessage (mkMessage projectId agentId "Test Subject" now)
  let req := mkRequest [("id", s!"{messageId}")]
  let resp ← Resources.Mail.handleMessage db cfg req
  resp.status.code ≡ (200 : UInt16)
  let json ← parseJsonResponse resp
  match json.getObjValAs? String "subject" with
  | Except.ok subject => subject ≡ "Test Subject"
  | _ => throw (IO.userError "Expected subject field")
  db.close

test "handleInbox returns empty inbox" := do
  let db ← Storage.Database.openMemory
  let cfg := Config.default
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "p1" "/my/project" now
  let _ ← db.insertAgent (mkAgent projectId "Agent1" now)
  let req := mkRequest [("agent", "Agent1")] [("project", "/my/project")]
  let resp ← Resources.Mail.handleInbox db cfg req
  resp.status.code ≡ (200 : UInt16)
  let json ← parseJsonResponse resp
  match json.getObjVal? "count" with
  | Except.ok (Lean.Json.num n) => n ≡ (0 : Nat)
  | _ => throw (IO.userError "Expected count field")
  db.close

test "handleInbox returns messages" := do
  let db ← Storage.Database.openMemory
  let cfg := Config.default
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "p1" "/my/project" now
  let senderId ← db.insertAgent (mkAgent projectId "Sender" now)
  let recipientId ← db.insertAgent (mkAgent projectId "Recipient" now)
  let messageId ← db.insertMessage (mkMessage projectId senderId "Hello" now)
  db.insertMessageRecipient { messageId, agentId := recipientId, recipientType := RecipientType.toRecipient, readAt := none, ackedAt := none }
  let req := mkRequest [("agent", "Recipient")] [("project", "/my/project")]
  let resp ← Resources.Mail.handleInbox db cfg req
  resp.status.code ≡ (200 : UInt16)
  let json ← parseJsonResponse resp
  match json.getObjVal? "count" with
  | Except.ok (Lean.Json.num n) => n ≡ (1 : Nat)
  | _ => throw (IO.userError "Expected count field")
  db.close

test "handleOutbox returns sent messages" := do
  let db ← Storage.Database.openMemory
  let cfg := Config.default
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "p1" "/my/project" now
  let senderId ← db.insertAgent (mkAgent projectId "Sender" now)
  let recipientId ← db.insertAgent (mkAgent projectId "Recipient" now)
  let messageId ← db.insertMessage (mkMessage projectId senderId "Hello" now)
  db.insertMessageRecipient { messageId, agentId := recipientId, recipientType := RecipientType.toRecipient, readAt := none, ackedAt := none }
  let req := mkRequest [("agent", "Sender")] [("project", "/my/project")]
  let resp ← Resources.Mail.handleOutbox db cfg req
  resp.status.code ≡ (200 : UInt16)
  let json ← parseJsonResponse resp
  match json.getObjVal? "count" with
  | Except.ok (Lean.Json.num n) => n ≡ (1 : Nat)
  | _ => throw (IO.userError "Expected count field")
  db.close

test "handleMailbox returns both inbox and outbox" := do
  let db ← Storage.Database.openMemory
  let cfg := Config.default
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "p1" "/my/project" now
  let agentId ← db.insertAgent (mkAgent projectId "Agent1" now)
  let _ ← db.insertMessage (mkMessage projectId agentId "Sent message" now)
  let req := mkRequest [("agent", "Agent1")] [("project", "/my/project")]
  let resp ← Resources.Mail.handleMailbox db cfg req
  resp.status.code ≡ (200 : UInt16)
  let json ← parseJsonResponse resp
  match json.getObjVal? "inbox" with
  | Except.ok inbox =>
    match inbox.getObjVal? "count" with
    | Except.ok (Lean.Json.num _) => pure ()
    | _ => throw (IO.userError "Expected inbox.count field")
  | _ => throw (IO.userError "Expected inbox field")
  match json.getObjVal? "outbox" with
  | Except.ok outbox =>
    match outbox.getObjVal? "count" with
    | Except.ok (Lean.Json.num n) => n ≡ (1 : Nat)
    | _ => throw (IO.userError "Expected outbox.count field")
  | _ => throw (IO.userError "Expected outbox field")
  db.close

test "handleThread returns messages in thread" := do
  let db ← Storage.Database.openMemory
  let cfg := Config.default
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "p1" "/my/project" now
  let senderId ← db.insertAgent (mkAgent projectId "Sender" now)
  let msg := { mkMessage projectId senderId "In thread" now with threadId := some "thread-123" }
  let _ ← db.insertMessage msg
  let req := mkRequest [("id", "thread-123")] [("project", "/my/project")]
  let resp ← Resources.Mail.handleThread db cfg req
  resp.status.code ≡ (200 : UInt16)
  let json ← parseJsonResponse resp
  match json.getObjVal? "count" with
  | Except.ok (Lean.Json.num n) => n ≡ (1 : Nat)
  | _ => throw (IO.userError "Expected count field")
  db.close

end Tests.Resources.Mail
