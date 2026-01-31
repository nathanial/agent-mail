import Crucible
import AgentMail

open Crucible
open AgentMail

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
    attachments := #[]
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
    attachments := #[]
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
