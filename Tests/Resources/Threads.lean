import Crucible
import AgentMail

open Crucible
open AgentMail
open Citadel

namespace Tests.Resources.Threads

testSuite "Resources.Threads"

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

def mkMessage (projectId senderId : Nat) (subject : String) (threadId : String) (now : Chronos.Timestamp) : Message := {
  id := 0
  projectId := projectId
  senderId := senderId
  subject := subject
  bodyMd := s!"Body for {subject}"
  attachments := #[]
  importance := Importance.normal
  ackRequired := false
  threadId := some threadId
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

test "handleThreads returns empty list" := do
  let db ← Storage.Database.openMemory
  let cfg := Config.default
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let _ ← db.insertProject "p1" "/my/project" now
  let req := mkRequest [("project_key", "/my/project")]
  let resp ← Resources.Threads.handleThreads db cfg req
  resp.status.code ≡ (200 : UInt16)
  let json ← parseJsonResponse resp
  match json.getObjVal? "count" with
  | Except.ok (Lean.Json.num n) => n ≡ (0 : Nat)
  | _ => throw (IO.userError "Expected count field")
  db.close

test "handleThreads returns summaries and unread count for agent" := do
  let db ← Storage.Database.openMemory
  let cfg := Config.default
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "p1" "/my/project" now
  let senderId ← db.insertAgent (mkAgent projectId "Sender" now)
  let recipientId ← db.insertAgent (mkAgent projectId "Recipient" now)
  let threadId := "thread-1"
  let messageId1 ← db.insertMessage (mkMessage projectId senderId "Hello" threadId now)
  db.insertMessageRecipient { messageId := messageId1, agentId := recipientId, recipientType := RecipientType.toRecipient, readAt := none, ackedAt := none }
  let later := Chronos.Timestamp.fromSeconds 1700000100
  let messageId2 ← db.insertMessage (mkMessage projectId senderId "Follow up" threadId later)
  db.insertMessageRecipient { messageId := messageId2, agentId := recipientId, recipientType := RecipientType.toRecipient, readAt := none, ackedAt := none }
  let req := mkRequest [("project_key", "/my/project")] [("agent", "Recipient"), ("include_bodies", "true")]
  let resp ← Resources.Threads.handleThreads db cfg req
  resp.status.code ≡ (200 : UInt16)
  let json ← parseJsonResponse resp
  match json.getObjVal? "threads" with
  | Except.ok (Lean.Json.arr arr) =>
    let first := arr[0]!
    match first.getObjVal? "message_count", first.getObjVal? "unread_count", first.getObjVal? "last_subject" with
    | Except.ok (Lean.Json.num count), Except.ok (Lean.Json.num unread), Except.ok (Lean.Json.str subject) =>
      count ≡ (2 : Nat)
      unread ≡ (2 : Nat)
      subject ≡ "Follow up"
    | _, _, _ => throw (IO.userError "Expected thread summary fields")
  | _ => throw (IO.userError "Expected threads array")
  db.close

end Tests.Resources.Threads
