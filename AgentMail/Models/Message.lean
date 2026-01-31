/-
  AgentMail.Models.Message - Message data model
-/
import Chronos
import AgentMail.Models.Types

namespace AgentMail

/-- A message sent between agents -/
structure Message where
  id : Nat
  projectId : Nat
  senderId : Nat
  subject : String
  bodyMd : String
  attachments : Array String
  importance : Importance
  ackRequired : Bool
  threadId : Option String
  createdTs : Chronos.Timestamp
  deriving Repr

namespace Message

instance : Lean.ToJson Message where
  toJson m := Lean.Json.mkObj [
    ("id", Lean.Json.num m.id),
    ("project_id", Lean.Json.num m.projectId),
    ("sender_id", Lean.Json.num m.senderId),
    ("subject", Lean.Json.str m.subject),
    ("body_md", Lean.Json.str m.bodyMd),
    ("attachments", Lean.toJson m.attachments),
    ("importance", Lean.toJson m.importance),
    ("ack_required", Lean.Json.bool m.ackRequired),
    ("thread_id", match m.threadId with
      | some t => Lean.Json.str t
      | none => Lean.Json.null),
    ("created_ts", Lean.Json.num m.createdTs.seconds)
  ]

instance : Lean.FromJson Message where
  fromJson? j := do
    let id ← j.getObjValAs? Nat "id"
    let projectId ← j.getObjValAs? Nat "project_id"
    let senderId ← j.getObjValAs? Nat "sender_id"
    let subject ← j.getObjValAs? String "subject"
    let bodyMd ← j.getObjValAs? String "body_md"
    let attachments := match j.getObjValAs? (Array String) "attachments" with
      | Except.ok arr => arr
      | Except.error _ => #[]
    let importance ← j.getObjValAs? Importance "importance"
    let ackRequired ← j.getObjValAs? Bool "ack_required"
    let threadId : Option String := match j.getObjVal? "thread_id" with
      | Except.ok v => v.getStr?.toOption
      | Except.error _ => none
    let createdTsSecs ← j.getObjValAs? Int "created_ts"
    pure {
      id := id
      projectId := projectId
      senderId := senderId
      subject := subject
      bodyMd := bodyMd
      attachments := attachments
      importance := importance
      ackRequired := ackRequired
      threadId := threadId
      createdTs := Chronos.Timestamp.fromSeconds createdTsSecs
    }

end Message

/-- A recipient entry for a message -/
structure MessageRecipient where
  messageId : Nat
  agentId : Nat
  recipientType : RecipientType
  readAt : Option Chronos.Timestamp
  ackedAt : Option Chronos.Timestamp
  deriving Repr

namespace MessageRecipient

instance : Lean.ToJson MessageRecipient where
  toJson r := Lean.Json.mkObj [
    ("message_id", Lean.Json.num r.messageId),
    ("agent_id", Lean.Json.num r.agentId),
    ("recipient_type", Lean.toJson r.recipientType),
    ("read_at", match r.readAt with
      | some t => Lean.Json.num t.seconds
      | none => Lean.Json.null),
    ("acked_at", match r.ackedAt with
      | some t => Lean.Json.num t.seconds
      | none => Lean.Json.null)
  ]

instance : Lean.FromJson MessageRecipient where
  fromJson? j := do
    let messageId ← j.getObjValAs? Nat "message_id"
    let agentId ← j.getObjValAs? Nat "agent_id"
    let recipientType ← j.getObjValAs? RecipientType "recipient_type"
    let readAt : Option Chronos.Timestamp := match j.getObjVal? "read_at" with
      | Except.ok v => v.getInt?.toOption.map Chronos.Timestamp.fromSeconds
      | Except.error _ => none
    let ackedAt : Option Chronos.Timestamp := match j.getObjVal? "acked_at" with
      | Except.ok v => v.getInt?.toOption.map Chronos.Timestamp.fromSeconds
      | Except.error _ => none
    pure {
      messageId := messageId
      agentId := agentId
      recipientType := recipientType
      readAt := readAt
      ackedAt := ackedAt
    }

end MessageRecipient

end AgentMail
