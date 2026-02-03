/-
  AgentMail.Tools.Messaging - Messaging-related MCP tool handlers
-/
import Chronos
import Citadel
import AgentMail.Config
import AgentMail.Notifications
import AgentMail.Protocol.JsonRpc
import AgentMail.Storage.Database
import AgentMail.Storage.Archive
import AgentMail.Tools.Identity
import AgentMail.SSE

open Citadel

namespace AgentMail.Tools.Messaging

private def publishMailEvent (eventType : String) (project : Project) (payload : Lean.Json) : IO Unit := do
  let enriched := match payload with
    | Lean.Json.obj entries =>
      let base : List (String × Lean.Json) := [
        ("project_slug", Lean.Json.str project.slug),
        ("project_key", Lean.Json.str project.humanKey)
      ]
      Lean.Json.mkObj (base ++ entries.toList)
    | _ => payload
  AgentMail.SSE.publish eventType enriched

/-- Generate a unique thread ID -/
def generateThreadId : IO String := do
  let nanos ← IO.monoNanosNow
  pure s!"thread-{nanos}"

/-- Helper to resolve an agent by name in a project -/
def resolveAgent (db : Storage.Database) (projectId : Nat) (name : String) : IO (Option Agent) :=
  db.queryAgentByName projectId name

/-- Parse since_ts value from ISO 8601 or integer seconds. -/
def parseSinceTs (raw : String) : IO (Option Int) := do
  match raw.toInt? with
  | some n => pure (some n)
  | none =>
    match Chronos.DateTime.parseIso8601 raw with
    | Except.ok dt =>
      let ts ← Chronos.DateTime.toTimestamp dt
      pure (some ts.seconds)
    | Except.error _ => pure none

/-- Handle send_message request -/
def handleSendMessage (db : Storage.Database) (cfg : Config) (req : JsonRpc.Request) : IO Response := do
  let params := req.params.getD Lean.Json.null

  -- Extract required params
  let projectKey ← match params.getObjValAs? String "project_key" with
    | Except.ok k => pure k
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: project_key")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  let senderName ← match params.getObjValAs? String "sender_name" with
    | Except.ok n => pure n
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: sender_name")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  let toRecipients ← match params.getObjValAs? (Array String) "to" with
    | Except.ok arr => pure arr
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: to (array of recipient names)")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  let subject ← match params.getObjValAs? String "subject" with
    | Except.ok s => pure s
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: subject")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  let bodyMd ← match params.getObjValAs? String "body_md" with
    | Except.ok b => pure b
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: body_md")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Optional params
  let ccRecipients := match params.getObjValAs? (Array String) "cc" with
    | Except.ok arr => arr
    | Except.error _ => #[]

  let bccRecipients := match params.getObjValAs? (Array String) "bcc" with
    | Except.ok arr => arr
    | Except.error _ => #[]

  let attachmentPaths := match params.getObjValAs? (Array String) "attachment_paths" with
    | Except.ok arr => arr
    | Except.error _ => #[]

  let importance := match params.getObjValAs? String "importance" with
    | Except.ok s => Importance.fromString? s |>.getD .normal
    | Except.error _ => .normal

  let ackRequired := match params.getObjValAs? Bool "ack_required" with
    | Except.ok b => b
    | Except.error _ => false

  let threadIdOpt := match params.getObjValAs? String "thread_id" with
    | Except.ok t => some t
    | Except.error _ => none

  -- Resolve project
  let project ← match ← Identity.resolveProject db projectKey with
    | some p => pure p
    | none =>
      let err := JsonRpc.Error.invalidParams (some s!"project not found: {projectKey}")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Resolve sender
  let sender ← match ← resolveAgent db project.id senderName with
    | some a => pure a
    | none =>
      let err := JsonRpc.Error.invalidParams (some s!"sender not found: {senderName}")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Ensure at least one recipient
  if toRecipients.isEmpty && ccRecipients.isEmpty && bccRecipients.isEmpty then
    let err := JsonRpc.Error.invalidParams (some "at least one of to/cc/bcc must be provided")
    let resp := JsonRpc.Response.failure req.id err
    return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Resolve all recipients
  let mut toNames : Array String := #[]
  let mut ccNames : Array String := #[]
  let mut bccNames : Array String := #[]
  let mut recipientAgents : Array (Agent × RecipientType) := #[]

  -- Resolve TO recipients
  for name in toRecipients do
    match ← resolveAgent db project.id name with
    | some agent =>
      recipientAgents := recipientAgents.push (agent, .toRecipient)
      toNames := toNames.push agent.name
    | none =>
      let err := JsonRpc.Error.invalidParams (some s!"recipient not found: {name}")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Resolve CC recipients
  for name in ccRecipients do
    match ← resolveAgent db project.id name with
    | some agent =>
      recipientAgents := recipientAgents.push (agent, .cc)
      ccNames := ccNames.push agent.name
    | none =>
      let err := JsonRpc.Error.invalidParams (some s!"cc recipient not found: {name}")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Resolve BCC recipients
  for name in bccRecipients do
    match ← resolveAgent db project.id name with
    | some agent =>
      recipientAgents := recipientAgents.push (agent, .bcc)
      bccNames := bccNames.push agent.name
    | none =>
      let err := JsonRpc.Error.invalidParams (some s!"bcc recipient not found: {name}")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Create message
  let now ← Chronos.Timestamp.now
  let msg : Message := {
    id := 0  -- Will be set by insert
    projectId := project.id
    senderId := sender.id
    subject := subject
    bodyMd := bodyMd
    attachments := attachmentPaths
    importance := importance
    ackRequired := ackRequired
    threadId := threadIdOpt
    createdTs := now
  }

  -- Insert message
  let messageId ← db.insertMessage msg

  -- Insert recipients
  for (agent, recType) in recipientAgents do
    let recipient : MessageRecipient := {
      messageId := messageId
      agentId := agent.id
      recipientType := recType
      readAt := none
      ackedAt := none
    }
    db.insertMessageRecipient recipient

  -- Emit notification signals for to/cc recipients (best-effort)
  let priority := some importance.toString
  for name in toNames do
    Notifications.notifyNewMessage cfg.notifications project.slug name
      (toString messageId) (threadIdOpt.getD "") sender.name priority
  for name in ccNames do
    Notifications.notifyNewMessage cfg.notifications project.slug name
      (toString messageId) (threadIdOpt.getD "") sender.name priority

  -- Write archive artifacts
  let archive ← Storage.ensureProjectArchive cfg project.slug
  let createdIso ← Storage.timestampToIso now
  let frontmatter := Lean.Json.mkObj [
    ("id", Lean.Json.num messageId),
    ("thread_id", match threadIdOpt with | some t => Lean.Json.str t | none => Lean.Json.null),
    ("project", Lean.Json.str project.humanKey),
    ("project_slug", Lean.Json.str project.slug),
    ("from", Lean.Json.str sender.name),
    ("to", Lean.toJson toNames),
    ("cc", Lean.toJson ccNames),
    ("bcc", Lean.toJson bccNames),
    ("subject", Lean.Json.str subject),
    ("importance", Lean.toJson importance),
    ("ack_required", Lean.Json.bool ackRequired),
    ("created", Lean.Json.str createdIso),
    ("attachments", Lean.toJson attachmentPaths)
  ]
  let recipientsForArchive := toNames ++ ccNames ++ bccNames
  Storage.writeMessageBundle archive frontmatter bodyMd sender.name recipientsForArchive now subject threadIdOpt

  -- Broadcast SSE event for live UI updates
  publishMailEvent "message.sent" project (Lean.Json.mkObj [
    ("message_id", Lean.Json.num messageId),
    ("thread_id", match threadIdOpt with | some t => Lean.Json.str t | none => Lean.Json.null),
    ("sender", Lean.Json.str sender.name),
    ("subject", Lean.Json.str subject),
    ("importance", Lean.toJson importance),
    ("ack_required", Lean.Json.bool ackRequired),
    ("created_ts", Lean.Json.num now.seconds)
  ])

  -- Build response
  let payload := Lean.Json.mkObj [
    ("id", Lean.Json.num messageId),
    ("thread_id", match threadIdOpt with | some t => Lean.Json.str t | none => Lean.Json.null),
    ("subject", Lean.Json.str subject),
    ("body_md", Lean.Json.str bodyMd),
    ("importance", Lean.toJson importance),
    ("ack_required", Lean.Json.bool ackRequired),
    ("created_ts", Lean.Json.num now.seconds),
    ("from", Lean.Json.str sender.name),
    ("to", Lean.toJson toNames),
    ("cc", Lean.toJson ccNames),
    ("bcc", Lean.toJson bccNames),
    ("attachments", Lean.toJson attachmentPaths)
  ]
  let deliveries := Lean.Json.arr #[Lean.Json.mkObj [
    ("project", Lean.Json.str project.humanKey),
    ("payload", payload)
  ]]
  let resultFields : List (String × Lean.Json) := [
    ("deliveries", deliveries),
    ("count", Lean.Json.num 1)
  ] ++
    (if attachmentPaths.isEmpty then [] else [("attachments", Lean.toJson attachmentPaths)])
  let result := Lean.Json.mkObj resultFields
  let resp := JsonRpc.Response.success req.id result
  pure (Response.json (Lean.Json.compress (Lean.toJson resp)))

/-- Handle reply_message request -/
def handleReplyMessage (db : Storage.Database) (cfg : Config) (req : JsonRpc.Request) : IO Response := do
  let params := req.params.getD Lean.Json.null

  -- Extract required params
  let projectKey ← match params.getObjValAs? String "project_key" with
    | Except.ok k => pure k
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: project_key")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  let senderName ← match params.getObjValAs? String "sender_name" with
    | Except.ok n => pure n
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: sender_name")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  let originalMessageId ← match params.getObjValAs? Nat "original_message_id" with
    | Except.ok id => pure id
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: original_message_id")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  let bodyMd ← match params.getObjValAs? String "body_md" with
    | Except.ok b => pure b
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: body_md")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Optional params
  let toRecipientsOpt := match params.getObjValAs? (Array String) "to" with
    | Except.ok arr => some arr
    | Except.error _ => none

  let ccRecipients := match params.getObjValAs? (Array String) "cc" with
    | Except.ok arr => arr
    | Except.error _ => #[]

  let bccRecipients := match params.getObjValAs? (Array String) "bcc" with
    | Except.ok arr => arr
    | Except.error _ => #[]

  let subjectPrefix := match params.getObjValAs? String "subject_prefix" with
    | Except.ok s => s.trim
    | Except.error _ => "Re:"

  let importanceOpt := match params.getObjValAs? String "importance" with
    | Except.ok s => some (Importance.fromString? s |>.getD .normal)
    | Except.error _ => none

  -- Resolve project
  let project ← match ← Identity.resolveProject db projectKey with
    | some p => pure p
    | none =>
      let err := JsonRpc.Error.invalidParams (some s!"project not found: {projectKey}")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Query original message
  let originalMsg ← match ← db.queryMessageById originalMessageId with
    | some m => pure m
    | none =>
      let err := JsonRpc.Error.invalidParams (some s!"original message not found: {originalMessageId}")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Verify original message belongs to same project
  if originalMsg.projectId != project.id then
    let err := JsonRpc.Error.invalidParams (some "original message belongs to different project")
    let resp := JsonRpc.Response.failure req.id err
    return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Resolve sender
  let sender ← match ← resolveAgent db project.id senderName with
    | some a => pure a
    | none =>
      let err := JsonRpc.Error.invalidParams (some s!"sender not found: {senderName}")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Get original sender to use as default reply recipient
  let originalSender ← match ← db.queryAgentById originalMsg.senderId with
    | some a => pure a
    | none =>
      let err := JsonRpc.Error.internalError (some "original sender not found")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Determine thread ID (inherit from original or use original id)
  let threadId := match originalMsg.threadId with | some t => t | none => s!"{originalMsg.id}"

  -- Determine subject (prepend "Re: " if not already)
  let subjectPrefixClean := if subjectPrefix.isEmpty then "Re:" else subjectPrefix
  let subject :=
    if originalMsg.subject.toLower.startsWith subjectPrefixClean.toLower then originalMsg.subject
    else s!"{subjectPrefixClean} {originalMsg.subject}".trim

  -- Determine TO recipients (default to original sender if not specified)
  let toRecipients := match toRecipientsOpt with
    | some arr => arr
    | none => #[originalSender.name]

  -- Ensure at least one recipient
  if toRecipients.isEmpty && ccRecipients.isEmpty && bccRecipients.isEmpty then
    let err := JsonRpc.Error.invalidParams (some "at least one of to/cc/bcc must be provided")
    let resp := JsonRpc.Response.failure req.id err
    return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Resolve all recipients
  let mut toNames : Array String := #[]
  let mut ccNames : Array String := #[]
  let mut bccNames : Array String := #[]
  let mut recipientAgents : Array (Agent × RecipientType) := #[]

  -- Resolve TO recipients
  for name in toRecipients do
    match ← resolveAgent db project.id name with
    | some agent =>
      recipientAgents := recipientAgents.push (agent, .toRecipient)
      toNames := toNames.push agent.name
    | none =>
      let err := JsonRpc.Error.invalidParams (some s!"recipient not found: {name}")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Resolve CC recipients
  for name in ccRecipients do
    match ← resolveAgent db project.id name with
    | some agent =>
      recipientAgents := recipientAgents.push (agent, .cc)
      ccNames := ccNames.push agent.name
    | none =>
      let err := JsonRpc.Error.invalidParams (some s!"cc recipient not found: {name}")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Resolve BCC recipients
  for name in bccRecipients do
    match ← resolveAgent db project.id name with
    | some agent =>
      recipientAgents := recipientAgents.push (agent, .bcc)
      bccNames := bccNames.push agent.name
    | none =>
      let err := JsonRpc.Error.invalidParams (some s!"bcc recipient not found: {name}")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Create reply message
  let now ← Chronos.Timestamp.now
  let importance := match importanceOpt with
    | some i => i
    | none => originalMsg.importance
  let msg : Message := {
    id := 0
    projectId := project.id
    senderId := sender.id
    subject := subject
    bodyMd := bodyMd
    attachments := #[]
    importance := importance
    ackRequired := originalMsg.ackRequired
    threadId := some threadId
    createdTs := now
  }

  -- Insert message
  let messageId ← db.insertMessage msg

  -- Insert recipients
  for (agent, recType) in recipientAgents do
    let recipient : MessageRecipient := {
      messageId := messageId
      agentId := agent.id
      recipientType := recType
      readAt := none
      ackedAt := none
    }
    db.insertMessageRecipient recipient

  -- Emit notification signals for to/cc recipients (best-effort)
  let priority := some importance.toString
  for name in toNames do
    Notifications.notifyNewMessage cfg.notifications project.slug name
      (toString messageId) threadId sender.name priority
  for name in ccNames do
    Notifications.notifyNewMessage cfg.notifications project.slug name
      (toString messageId) threadId sender.name priority

  -- Write archive artifacts
  let archive ← Storage.ensureProjectArchive cfg project.slug
  let createdIso ← Storage.timestampToIso now
  let frontmatter := Lean.Json.mkObj [
    ("id", Lean.Json.num messageId),
    ("thread_id", Lean.Json.str threadId),
    ("project", Lean.Json.str project.humanKey),
    ("project_slug", Lean.Json.str project.slug),
    ("from", Lean.Json.str sender.name),
    ("to", Lean.toJson toNames),
    ("cc", Lean.toJson ccNames),
    ("bcc", Lean.toJson bccNames),
    ("subject", Lean.Json.str subject),
    ("importance", Lean.toJson importance),
    ("ack_required", Lean.Json.bool originalMsg.ackRequired),
    ("created", Lean.Json.str createdIso),
    ("attachments", Lean.toJson (#[] : Array String))
  ]
  let recipientsForArchive := toNames ++ ccNames ++ bccNames
  Storage.writeMessageBundle archive frontmatter bodyMd sender.name recipientsForArchive now subject (some threadId)

  -- Broadcast SSE event for live UI updates
  publishMailEvent "message.reply" project (Lean.Json.mkObj [
    ("message_id", Lean.Json.num messageId),
    ("thread_id", Lean.Json.str threadId),
    ("reply_to", Lean.Json.num originalMessageId),
    ("sender", Lean.Json.str sender.name),
    ("subject", Lean.Json.str subject),
    ("importance", Lean.toJson importance),
    ("ack_required", Lean.Json.bool originalMsg.ackRequired),
    ("created_ts", Lean.Json.num now.seconds)
  ])

  -- Build response
  let payload := Lean.Json.mkObj [
    ("id", Lean.Json.num messageId),
    ("thread_id", Lean.Json.str threadId),
    ("reply_to", Lean.Json.num originalMessageId),
    ("subject", Lean.Json.str subject),
    ("body_md", Lean.Json.str bodyMd),
    ("importance", Lean.toJson importance),
    ("ack_required", Lean.Json.bool originalMsg.ackRequired),
    ("created_ts", Lean.Json.num now.seconds),
    ("from", Lean.Json.str sender.name),
    ("to", Lean.toJson toNames),
    ("cc", Lean.toJson ccNames),
    ("bcc", Lean.toJson bccNames),
    ("attachments", Lean.toJson (#[] : Array String))
  ]
  let deliveries := Lean.Json.arr #[Lean.Json.mkObj [
    ("project", Lean.Json.str project.humanKey),
    ("payload", payload)
  ]]
  let result := Lean.Json.mkObj [
    ("id", Lean.Json.num messageId),
    ("thread_id", Lean.Json.str threadId),
    ("reply_to", Lean.Json.num originalMessageId),
    ("subject", Lean.Json.str subject),
    ("body_md", Lean.Json.str bodyMd),
    ("importance", Lean.toJson importance),
    ("ack_required", Lean.Json.bool originalMsg.ackRequired),
    ("created_ts", Lean.Json.num now.seconds),
    ("from", Lean.Json.str sender.name),
    ("to", Lean.toJson toNames),
    ("cc", Lean.toJson ccNames),
    ("bcc", Lean.toJson bccNames),
    ("attachments", Lean.toJson (#[] : Array String)),
    ("deliveries", deliveries),
    ("count", Lean.Json.num 1)
  ]
  let resp := JsonRpc.Response.success req.id result
  pure (Response.json (Lean.Json.compress (Lean.toJson resp)))

/-- Handle fetch_inbox request -/
def handleFetchInbox (db : Storage.Database) (cfg : Config) (req : JsonRpc.Request) : IO Response := do
  let params := req.params.getD Lean.Json.null

  -- Extract required params
  let projectKey ← match params.getObjValAs? String "project_key" with
    | Except.ok k => pure k
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: project_key")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  let agentName ← match params.getObjValAs? String "agent_name" with
    | Except.ok n => pure n
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: agent_name")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Optional params
  let limit := match params.getObjValAs? Nat "limit" with
    | Except.ok n => n
    | Except.error _ => 50

  let urgentOnly := match params.getObjValAs? Bool "urgent_only" with
    | Except.ok b => b
    | Except.error _ => false

  let includeBodies := match params.getObjValAs? Bool "include_bodies" with
    | Except.ok b => b
    | Except.error _ => false

  let sinceTs ← match params.getObjValAs? String "since_ts" with
    | Except.ok ts => parseSinceTs ts
    | Except.error _ =>
      match params.getObjValAs? Int "since_ts" with
      | Except.ok ts => pure (some ts)
      | Except.error _ => pure none

  -- Resolve project
  let project ← match ← Identity.resolveProject db projectKey with
    | some p => pure p
    | none =>
      let err := JsonRpc.Error.invalidParams (some s!"project not found: {projectKey}")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Resolve agent
  let agent ← match ← resolveAgent db project.id agentName with
    | some a => pure a
    | none =>
      let err := JsonRpc.Error.invalidParams (some s!"agent not found: {agentName}")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Query inbox
  let entries ← db.queryInbox project.id agent.id limit urgentOnly sinceTs

  -- Clear notification signal (best-effort)
  Notifications.clearSignal cfg.notifications project.slug agent.name

  -- Convert entries to JSON
  let messages := entries.map fun entry =>
    let baseFields : List (String × Lean.Json) := [
      ("id", Lean.Json.num entry.id),
      ("from", Lean.Json.str entry.senderName),
      ("subject", Lean.Json.str entry.subject),
      ("importance", Lean.toJson entry.importance),
      ("ack_required", Lean.Json.bool entry.ackRequired),
      ("thread_id", match entry.threadId with | some t => Lean.Json.str t | none => Lean.Json.null),
      ("created_ts", Lean.Json.num entry.createdTs.seconds),
      ("kind", Lean.toJson entry.recipientType)
    ]
    let fields := if includeBodies then
      baseFields ++ [("body_md", match entry.bodyMd with | some b => Lean.Json.str b | none => Lean.Json.null)]
    else baseFields
    Lean.Json.mkObj fields

  -- Build response (list of messages)
  let resp := JsonRpc.Response.success req.id (Lean.Json.arr messages)
  pure (Response.json (Lean.Json.compress (Lean.toJson resp)))

/-- Handle mark_message_read request -/
def handleMarkRead (db : Storage.Database) (_cfg : Config) (req : JsonRpc.Request) : IO Response := do
  let params := req.params.getD Lean.Json.null

  -- Extract required params
  let projectKey ← match params.getObjValAs? String "project_key" with
    | Except.ok k => pure k
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: project_key")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  let agentName ← match params.getObjValAs? String "agent_name" with
    | Except.ok n => pure n
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: agent_name")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  let messageId ← match params.getObjValAs? Nat "message_id" with
    | Except.ok id => pure id
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: message_id")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Resolve project
  let project ← match ← Identity.resolveProject db projectKey with
    | some p => pure p
    | none =>
      let err := JsonRpc.Error.invalidParams (some s!"project not found: {projectKey}")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Resolve agent
  let agent ← match ← resolveAgent db project.id agentName with
    | some a => pure a
    | none =>
      let err := JsonRpc.Error.invalidParams (some s!"agent not found: {agentName}")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Verify agent is a recipient
  let recipientStatus ← db.queryRecipientStatus messageId agent.id
  match recipientStatus with
  | none =>
    let err := JsonRpc.Error.invalidParams (some "agent is not a recipient of this message")
    let resp := JsonRpc.Response.failure req.id err
    return Response.json (Lean.Json.compress (Lean.toJson resp))
  | some status =>
    -- Check if already read
    match status.readAt with
    | some existingTs =>
      -- Already read, return existing timestamp
      let result := Lean.Json.mkObj [
        ("message_id", Lean.Json.num messageId),
        ("read", Lean.Json.bool true),
        ("read_at", Lean.Json.num existingTs.seconds)
      ]
      let resp := JsonRpc.Response.success req.id result
      return Response.json (Lean.Json.compress (Lean.toJson resp))
    | none =>
      -- Mark as read
      let now ← Chronos.Timestamp.now
      let updated ← db.updateMessageReadAt messageId agent.id now
      let result := Lean.Json.mkObj [
        ("message_id", Lean.Json.num messageId),
        ("read", Lean.Json.bool updated),
        ("read_at", if updated then Lean.Json.num now.seconds else Lean.Json.null)
      ]
      let resp := JsonRpc.Response.success req.id result
      publishMailEvent "message.read" project (Lean.Json.mkObj [
        ("message_id", Lean.Json.num messageId),
        ("agent_name", Lean.Json.str agent.name),
        ("read_at", if updated then Lean.Json.num now.seconds else Lean.Json.null)
      ])
      pure (Response.json (Lean.Json.compress (Lean.toJson resp)))

/-- Handle acknowledge_message request -/
def handleAcknowledge (db : Storage.Database) (_cfg : Config) (req : JsonRpc.Request) : IO Response := do
  let params := req.params.getD Lean.Json.null

  -- Extract required params
  let projectKey ← match params.getObjValAs? String "project_key" with
    | Except.ok k => pure k
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: project_key")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  let agentName ← match params.getObjValAs? String "agent_name" with
    | Except.ok n => pure n
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: agent_name")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  let messageId ← match params.getObjValAs? Nat "message_id" with
    | Except.ok id => pure id
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: message_id")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Resolve project
  let project ← match ← Identity.resolveProject db projectKey with
    | some p => pure p
    | none =>
      let err := JsonRpc.Error.invalidParams (some s!"project not found: {projectKey}")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Resolve agent
  let agent ← match ← resolveAgent db project.id agentName with
    | some a => pure a
    | none =>
      let err := JsonRpc.Error.invalidParams (some s!"agent not found: {agentName}")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Query message to check ack_required
  let msg ← match ← db.queryMessageById messageId with
    | some m => pure m
    | none =>
      let err := JsonRpc.Error.invalidParams (some s!"message not found: {messageId}")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Verify message belongs to same project
  if msg.projectId != project.id then
    let err := JsonRpc.Error.invalidParams (some "message belongs to different project")
    let resp := JsonRpc.Response.failure req.id err
    return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Verify agent is a recipient
  let recipientStatus ← db.queryRecipientStatus messageId agent.id
  match recipientStatus with
  | none =>
    let err := JsonRpc.Error.invalidParams (some "agent is not a recipient of this message")
    let resp := JsonRpc.Response.failure req.id err
    return Response.json (Lean.Json.compress (Lean.toJson resp))
  | some status =>
    -- Check if already acknowledged
    match status.ackedAt with
    | some existingTs =>
      -- Already acknowledged, return existing timestamp
      let result := Lean.Json.mkObj [
        ("message_id", Lean.Json.num messageId),
        ("acknowledged", Lean.Json.bool true),
        ("acknowledged_at", Lean.Json.num existingTs.seconds),
        ("read_at", match status.readAt with | some t => Lean.Json.num t.seconds | none => Lean.Json.null)
      ]
      let resp := JsonRpc.Response.success req.id result
      return Response.json (Lean.Json.compress (Lean.toJson resp))
    | none =>
      -- Acknowledge
      let now ← Chronos.Timestamp.now
      let _ ← db.updateMessageReadAt messageId agent.id now
      let updated ← db.updateMessageAckedAt messageId agent.id now
      let result := Lean.Json.mkObj [
        ("message_id", Lean.Json.num messageId),
        ("acknowledged", Lean.Json.bool updated),
        ("acknowledged_at", if updated then Lean.Json.num now.seconds else Lean.Json.null),
        ("read_at", Lean.Json.num now.seconds)
      ]
      let resp := JsonRpc.Response.success req.id result
      publishMailEvent "message.ack" project (Lean.Json.mkObj [
        ("message_id", Lean.Json.num messageId),
        ("agent_name", Lean.Json.str agent.name),
        ("acknowledged_at", if updated then Lean.Json.num now.seconds else Lean.Json.null),
        ("read_at", Lean.Json.num now.seconds)
      ])
      pure (Response.json (Lean.Json.compress (Lean.toJson resp)))

end AgentMail.Tools.Messaging
