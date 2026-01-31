import Crucible
import AgentMail

open Crucible
open AgentMail

namespace Tests.Messaging

testSuite "Messaging"

test "Insert and query message" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  -- Create project
  let projectId ← db.insertProject "test" "/test" now
  -- Create sender agent
  let senderAgent : Agent := {
    id := 0, projectId := projectId, name := "Sender"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let senderId ← db.insertAgent senderAgent
  -- Create recipient agent
  let recipientAgent : Agent := {
    id := 0, projectId := projectId, name := "Recipient"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let _recipientId ← db.insertAgent recipientAgent
  -- Create message
  let msg : Message := {
    id := 0, projectId := projectId, senderId := senderId
    subject := "Test Subject", bodyMd := "Test body"
    attachments := #[]
    importance := Importance.normal, ackRequired := false
    threadId := some "thread-123", createdTs := now
  }
  let messageId ← db.insertMessage msg
  messageId ≡ (1 : Nat)
  -- Query message
  let found ← db.queryMessageById messageId
  match found with
  | some m =>
    m.subject ≡ "Test Subject"
    m.bodyMd ≡ "Test body"
    m.threadId ≡ some "thread-123"
  | none => throw (IO.userError "Message not found")
  db.close

test "Insert and query recipients" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "test" "/test" now
  -- Create agents
  let senderAgent : Agent := {
    id := 0, projectId := projectId, name := "Sender"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let senderId ← db.insertAgent senderAgent
  let recipientAgent : Agent := {
    id := 0, projectId := projectId, name := "Recipient"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let recipientId ← db.insertAgent recipientAgent
  -- Create message
  let msg : Message := {
    id := 0, projectId := projectId, senderId := senderId
    subject := "Test", bodyMd := "Body"
    attachments := #[]
    importance := Importance.high, ackRequired := true
    threadId := none, createdTs := now
  }
  let messageId ← db.insertMessage msg
  -- Add recipient
  let msgRecipient : MessageRecipient := {
    messageId := messageId, agentId := recipientId
    recipientType := RecipientType.toRecipient
    readAt := none, ackedAt := none
  }
  db.insertMessageRecipient msgRecipient
  -- Query recipient status
  let status ← db.queryRecipientStatus messageId recipientId
  match status with
  | some s =>
    s.recipientType ≡ RecipientType.toRecipient
    shouldSatisfy s.readAt.isNone "readAt should be none"
    shouldSatisfy s.ackedAt.isNone "ackedAt should be none"
  | none => throw (IO.userError "Recipient not found")
  db.close

test "Query inbox" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "test" "/test" now
  -- Create agents
  let senderAgent : Agent := {
    id := 0, projectId := projectId, name := "Sender"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let senderId ← db.insertAgent senderAgent
  let recipientAgent : Agent := {
    id := 0, projectId := projectId, name := "Recipient"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let recipientId ← db.insertAgent recipientAgent
  -- Create 3 messages
  for i in [1, 2, 3] do
    let msg : Message := {
      id := 0, projectId := projectId, senderId := senderId
      subject := s!"Message {i}", bodyMd := s!"Body {i}"
      attachments := #[]
      importance := if i == 3 then Importance.urgent else Importance.normal
      ackRequired := false
      threadId := some s!"thread-{i}", createdTs := Chronos.Timestamp.fromSeconds (1700000000 + i)
    }
    let messageId ← db.insertMessage msg
    let msgRecipient : MessageRecipient := {
      messageId := messageId, agentId := recipientId
      recipientType := RecipientType.toRecipient
      readAt := none, ackedAt := none
    }
    db.insertMessageRecipient msgRecipient
  -- Query inbox
  let entries ← db.queryInbox projectId recipientId 10 false none
  entries.size ≡ (3 : Nat)
  -- First entry should be most recent (Message 3)
  (entries.getD 0 default).subject ≡ "Message 3"
  -- Query urgent only
  let urgentEntries ← db.queryInbox projectId recipientId 10 true none
  urgentEntries.size ≡ (1 : Nat)
  (urgentEntries.getD 0 default).subject ≡ "Message 3"
  db.close

test "Update read and ack status" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let later := Chronos.Timestamp.fromSeconds 1700001000
  let projectId ← db.insertProject "test" "/test" now
  -- Create agents
  let senderAgent : Agent := {
    id := 0, projectId := projectId, name := "Sender"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let senderId ← db.insertAgent senderAgent
  let recipientAgent : Agent := {
    id := 0, projectId := projectId, name := "Recipient"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let recipientId ← db.insertAgent recipientAgent
  -- Create message
  let msg : Message := {
    id := 0, projectId := projectId, senderId := senderId
    subject := "Test", bodyMd := "Body"
    attachments := #[]
    importance := Importance.normal, ackRequired := true
    threadId := none, createdTs := now
  }
  let messageId ← db.insertMessage msg
  let msgRecipient : MessageRecipient := {
    messageId := messageId, agentId := recipientId
    recipientType := RecipientType.toRecipient
    readAt := none, ackedAt := none
  }
  db.insertMessageRecipient msgRecipient
  -- Mark as read
  let readUpdated ← db.updateMessageReadAt messageId recipientId later
  shouldSatisfy readUpdated "should have updated read status"
  -- Mark as read again (should not update since already set)
  let readUpdated2 ← db.updateMessageReadAt messageId recipientId (Chronos.Timestamp.fromSeconds 1700002000)
  shouldSatisfy (not readUpdated2) "should not update already read message"
  -- Verify read_at
  let status ← db.queryRecipientStatus messageId recipientId
  match status with
  | some s =>
    match s.readAt with
    | some t => t.seconds ≡ later.seconds
    | none => throw (IO.userError "readAt should be set")
  | none => throw (IO.userError "Recipient not found")
  -- Acknowledge
  let ackUpdated ← db.updateMessageAckedAt messageId recipientId later
  shouldSatisfy ackUpdated "should have updated ack status"
  let statusAfterAck ← db.queryRecipientStatus messageId recipientId
  match statusAfterAck with
  | some s =>
    shouldSatisfy s.ackedAt.isSome "ackedAt should be set"
  | none => throw (IO.userError "Recipient not found after ack")
  db.close

test "Count inbox" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "test" "/test" now
  let senderAgent : Agent := {
    id := 0, projectId := projectId, name := "Sender"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let senderId ← db.insertAgent senderAgent
  let recipientAgent : Agent := {
    id := 0, projectId := projectId, name := "Recipient"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let recipientId ← db.insertAgent recipientAgent
  -- Initially empty
  let count0 ← db.countInbox projectId recipientId
  count0 ≡ (0 : Nat)
  -- Add messages
  for i in [1, 2, 3, 4, 5] do
    let msg : Message := {
      id := 0, projectId := projectId, senderId := senderId
      subject := s!"Msg {i}", bodyMd := ""
      attachments := #[]
      importance := Importance.normal, ackRequired := false
      threadId := none, createdTs := now
    }
    let messageId ← db.insertMessage msg
    let msgRecipient : MessageRecipient := {
      messageId := messageId, agentId := recipientId
      recipientType := RecipientType.toRecipient
      readAt := none, ackedAt := none
    }
    db.insertMessageRecipient msgRecipient
  let count5 ← db.countInbox projectId recipientId
  count5 ≡ (5 : Nat)
  db.close

end Tests.Messaging
