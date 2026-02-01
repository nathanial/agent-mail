/-
  AgentMail.Resources.Mail - Mail-related MCP resources
-/
import Citadel
import AgentMail.Config
import AgentMail.Storage.Database
import AgentMail.Resources.Core
import AgentMail.Tools.Identity

open Citadel

namespace AgentMail.Resources.Mail

/-- Convert an InboxEntry to JSON -/
private def inboxEntryToJson (entry : Storage.Database.InboxEntry) (includeBodies : Bool) : Lean.Json :=
  let base := [
    ("id", Lean.Json.num entry.id),
    ("sender_name", Lean.Json.str entry.senderName),
    ("subject", Lean.Json.str entry.subject),
    ("importance", Lean.toJson entry.importance),
    ("ack_required", Lean.Json.bool entry.ackRequired),
    ("thread_id", match entry.threadId with | some t => Lean.Json.str t | none => Lean.Json.null),
    ("created_ts", Lean.Json.num entry.createdTs.seconds),
    ("read_at", match entry.readAt with | some t => Lean.Json.num t.seconds | none => Lean.Json.null),
    ("acked_at", match entry.ackedAt with | some t => Lean.Json.num t.seconds | none => Lean.Json.null),
    ("recipient_type", Lean.toJson entry.recipientType)
  ]
  let withBody := if includeBodies then
    base ++ [("body_md", match entry.bodyMd with | some b => Lean.Json.str b | none => Lean.Json.null)]
  else base
  Lean.Json.mkObj withBody

/-- Convert an OutboxEntry to JSON -/
private def outboxEntryToJson (entry : Storage.Database.OutboxEntry) (includeBodies : Bool) : Lean.Json :=
  let base := [
    ("id", Lean.Json.num entry.id),
    ("subject", Lean.Json.str entry.subject),
    ("importance", Lean.toJson entry.importance),
    ("ack_required", Lean.Json.bool entry.ackRequired),
    ("thread_id", match entry.threadId with | some t => Lean.Json.str t | none => Lean.Json.null),
    ("created_ts", Lean.Json.num entry.createdTs.seconds),
    ("recipients", Lean.toJson entry.recipients)
  ]
  let withBody := if includeBodies then
    base ++ [("body_md", match entry.bodyMd with | some b => Lean.Json.str b | none => Lean.Json.null)]
  else base
  Lean.Json.mkObj withBody

/-- Handle GET /resource/message/:id -/
def handleMessage (db : Storage.Database) (cfg : AgentMail.Config) (req : ServerRequest) : IO Response := do
  let idStr ← match req.param "id" with
    | some s => pure s
    | none => return Core.resourceBadRequest "missing id parameter"

  let messageId ← match idStr.toNat? with
    | some n => pure n
    | none => return Core.resourceBadRequest "id must be a number"

  let projectKey := req.queryParam "project"

  -- If project is specified, validate it exists
  if let some key := projectKey then
    match ← Tools.Identity.resolveProject db key with
    | none => return Core.resourceNotFound s!"project not found: {key}"
    | some _ => pure ()

  let message ← match ← db.queryMessageById messageId with
    | some m => pure m
    | none => return Core.resourceNotFound s!"message not found: {messageId}"

  -- Get sender name
  let senderName ← match ← db.queryAgentById message.senderId with
    | some a => pure a.name
    | none => pure "unknown"

  let result := Lean.Json.mkObj [
    ("id", Lean.Json.num message.id),
    ("project_id", Lean.Json.num message.projectId),
    ("sender_id", Lean.Json.num message.senderId),
    ("sender_name", Lean.Json.str senderName),
    ("subject", Lean.Json.str message.subject),
    ("body_md", Lean.Json.str message.bodyMd),
    ("attachments", Lean.toJson message.attachments),
    ("importance", Lean.toJson message.importance),
    ("ack_required", Lean.Json.bool message.ackRequired),
    ("thread_id", match message.threadId with | some t => Lean.Json.str t | none => Lean.Json.null),
    ("created_ts", Lean.Json.num message.createdTs.seconds)
  ]
  Core.resourceOkFormatted cfg req "message" result

/-- Handle GET /resource/thread/:id -/
def handleThread (db : Storage.Database) (cfg : AgentMail.Config) (req : ServerRequest) : IO Response := do
  let threadId ← match req.param "id" with
    | some s => pure s
    | none => return Core.resourceBadRequest "missing id parameter"

  let projectKey := req.queryParam "project"
  let limit := Core.parseLimit (req.queryParam "limit") 50 100
  let includeBodies := Core.parseBool (req.queryParam "include_bodies") true

  let project ← match projectKey with
    | some key => match ← Tools.Identity.resolveProject db key with
      | some p => pure p
      | none => return Core.resourceNotFound s!"project not found: {key}"
    | none => return Core.resourceBadRequest "missing project query parameter"

  let messages ← db.queryMessagesByThread project.id threadId limit
  let messagesJson := messages.map fun m => Lean.Json.mkObj (
    [
      ("id", Lean.Json.num m.id),
      ("sender_name", Lean.Json.str m.senderName),
      ("subject", Lean.Json.str m.subject),
      ("importance", Lean.toJson m.importance),
      ("ack_required", Lean.Json.bool m.ackRequired),
      ("thread_id", match m.threadId with | some t => Lean.Json.str t | none => Lean.Json.null),
      ("created_ts", Lean.Json.num m.createdTs.seconds)
    ] ++ if includeBodies then [("body_md", Lean.Json.str m.bodyMd)] else []
  )

  let result := Lean.Json.mkObj [
    ("thread_id", Lean.Json.str threadId),
    ("project_id", Lean.Json.num project.id),
    ("messages", Lean.Json.arr messagesJson),
    ("count", Lean.Json.num messages.size)
  ]
  Core.resourceOkFormatted cfg req "thread" result

/-- Handle GET /resource/inbox/:agent -/
def handleInbox (db : Storage.Database) (cfg : AgentMail.Config) (req : ServerRequest) : IO Response := do
  let agentName ← match req.param "agent" with
    | some s => pure s
    | none => return Core.resourceBadRequest "missing agent parameter"

  let projectKey := req.queryParam "project"
  let limit := Core.parseLimit (req.queryParam "limit") 50 100
  let includeBodies := Core.parseBool (req.queryParam "include_bodies") false
  let urgentOnly := Core.parseBool (req.queryParam "urgent_only") false
  let sinceTs := (req.queryParam "since_ts").bind (·.toInt?)

  let project ← match projectKey with
    | some key => match ← Tools.Identity.resolveProject db key with
      | some p => pure p
      | none => return Core.resourceNotFound s!"project not found: {key}"
    | none => return Core.resourceBadRequest "missing project query parameter"

  let agent ← match ← db.queryAgentByName project.id agentName with
    | some a => pure a
    | none => return Core.resourceNotFound s!"agent not found: {agentName}"

  let entries ← db.queryInbox project.id agent.id limit urgentOnly sinceTs
  let entriesJson := entries.map (inboxEntryToJson · includeBodies)

  let result := Lean.Json.mkObj [
    ("agent_id", Lean.Json.num agent.id),
    ("agent_name", Lean.Json.str agent.name),
    ("project_id", Lean.Json.num project.id),
    ("messages", Lean.Json.arr entriesJson),
    ("count", Lean.Json.num entries.size)
  ]
  Core.resourceOkFormatted cfg req "inbox" result

/-- Handle GET /resource/outbox/:agent -/
def handleOutbox (db : Storage.Database) (cfg : AgentMail.Config) (req : ServerRequest) : IO Response := do
  let agentName ← match req.param "agent" with
    | some s => pure s
    | none => return Core.resourceBadRequest "missing agent parameter"

  let projectKey := req.queryParam "project"
  let limit := Core.parseLimit (req.queryParam "limit") 50 100
  let includeBodies := Core.parseBool (req.queryParam "include_bodies") false

  let project ← match projectKey with
    | some key => match ← Tools.Identity.resolveProject db key with
      | some p => pure p
      | none => return Core.resourceNotFound s!"project not found: {key}"
    | none => return Core.resourceBadRequest "missing project query parameter"

  let agent ← match ← db.queryAgentByName project.id agentName with
    | some a => pure a
    | none => return Core.resourceNotFound s!"agent not found: {agentName}"

  let entries ← db.queryOutbox project.id agent.id limit
  let entriesJson := entries.map (outboxEntryToJson · includeBodies)

  let result := Lean.Json.mkObj [
    ("agent_id", Lean.Json.num agent.id),
    ("agent_name", Lean.Json.str agent.name),
    ("project_id", Lean.Json.num project.id),
    ("messages", Lean.Json.arr entriesJson),
    ("count", Lean.Json.num entries.size)
  ]
  Core.resourceOkFormatted cfg req "outbox" result

/-- Handle GET /resource/mailbox/:agent -/
def handleMailbox (db : Storage.Database) (cfg : AgentMail.Config) (req : ServerRequest) : IO Response := do
  let agentName ← match req.param "agent" with
    | some s => pure s
    | none => return Core.resourceBadRequest "missing agent parameter"

  let projectKey := req.queryParam "project"
  let limit := Core.parseLimit (req.queryParam "limit") 50 100
  let includeBodies := Core.parseBool (req.queryParam "include_bodies") false
  let urgentOnly := Core.parseBool (req.queryParam "urgent_only") false
  let sinceTs := (req.queryParam "since_ts").bind (·.toInt?)

  let project ← match projectKey with
    | some key => match ← Tools.Identity.resolveProject db key with
      | some p => pure p
      | none => return Core.resourceNotFound s!"project not found: {key}"
    | none => return Core.resourceBadRequest "missing project query parameter"

  let agent ← match ← db.queryAgentByName project.id agentName with
    | some a => pure a
    | none => return Core.resourceNotFound s!"agent not found: {agentName}"

  -- Fetch both inbox and outbox
  let inbox ← db.queryInbox project.id agent.id limit urgentOnly sinceTs
  let outbox ← db.queryOutbox project.id agent.id limit

  let inboxJson := inbox.map (inboxEntryToJson · includeBodies)
  let outboxJson := outbox.map (outboxEntryToJson · includeBodies)

  let result := Lean.Json.mkObj [
    ("agent_id", Lean.Json.num agent.id),
    ("agent_name", Lean.Json.str agent.name),
    ("project_id", Lean.Json.num project.id),
    ("inbox", Lean.Json.mkObj [
      ("messages", Lean.Json.arr inboxJson),
      ("count", Lean.Json.num inbox.size)
    ]),
    ("outbox", Lean.Json.mkObj [
      ("messages", Lean.Json.arr outboxJson),
      ("count", Lean.Json.num outbox.size)
    ])
  ]
  Core.resourceOkFormatted cfg req "mailbox" result

end AgentMail.Resources.Mail
