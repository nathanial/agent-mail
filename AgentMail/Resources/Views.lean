/-
  AgentMail.Resources.Views - View-related MCP resources for filtered message views
-/
import Citadel
import AgentMail.Config
import AgentMail.Storage.Database
import AgentMail.Resources.Core
import AgentMail.Tools.Identity

open Citadel

namespace AgentMail.Resources.Views

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

/-- Handle GET /resource/views/urgent-unread/:agent -/
def handleUrgentUnread (db : Storage.Database) (cfg : AgentMail.Config) (req : ServerRequest) : IO Response := do
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

  let entries ← db.queryUnreadUrgentMessages project.id agent.id limit
  let entriesJson := entries.map (inboxEntryToJson · includeBodies)

  let result := Lean.Json.mkObj [
    ("view", Lean.Json.str "urgent-unread"),
    ("agent_id", Lean.Json.num agent.id),
    ("agent_name", Lean.Json.str agent.name),
    ("project_id", Lean.Json.num project.id),
    ("messages", Lean.Json.arr entriesJson),
    ("count", Lean.Json.num entries.size)
  ]
  Core.resourceOkFormatted cfg req "views.urgent_unread" result

/-- Handle GET /resource/views/ack-required/:agent -/
def handleAckRequired (db : Storage.Database) (cfg : AgentMail.Config) (req : ServerRequest) : IO Response := do
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

  let entries ← db.queryAckRequiredMessages project.id agent.id limit
  let entriesJson := entries.map (inboxEntryToJson · includeBodies)

  let result := Lean.Json.mkObj [
    ("view", Lean.Json.str "ack-required"),
    ("agent_id", Lean.Json.num agent.id),
    ("agent_name", Lean.Json.str agent.name),
    ("project_id", Lean.Json.num project.id),
    ("messages", Lean.Json.arr entriesJson),
    ("count", Lean.Json.num entries.size)
  ]
  Core.resourceOkFormatted cfg req "views.ack_required" result

/-- Handle GET /resource/views/acks-stale/:agent -/
def handleAcksStale (db : Storage.Database) (cfg : AgentMail.Config) (req : ServerRequest) : IO Response := do
  let agentName ← match req.param "agent" with
    | some s => pure s
    | none => return Core.resourceBadRequest "missing agent parameter"

  let projectKey := req.queryParam "project"
  let limit := Core.parseLimit (req.queryParam "limit") 50 100
  let includeBodies := Core.parseBool (req.queryParam "include_bodies") false
  -- Default stale threshold is 24 hours (86400 seconds)
  let thresholdSeconds := Core.parseInt (req.queryParam "threshold_seconds") 86400

  let project ← match projectKey with
    | some key => match ← Tools.Identity.resolveProject db key with
      | some p => pure p
      | none => return Core.resourceNotFound s!"project not found: {key}"
    | none => return Core.resourceBadRequest "missing project query parameter"

  let agent ← match ← db.queryAgentByName project.id agentName with
    | some a => pure a
    | none => return Core.resourceNotFound s!"agent not found: {agentName}"

  let entries ← db.queryStaleAckMessages project.id agent.id (Int.ofNat thresholdSeconds) limit
  let entriesJson := entries.map (inboxEntryToJson · includeBodies)

  let result := Lean.Json.mkObj [
    ("view", Lean.Json.str "acks-stale"),
    ("agent_id", Lean.Json.num agent.id),
    ("agent_name", Lean.Json.str agent.name),
    ("project_id", Lean.Json.num project.id),
    ("threshold_seconds", Lean.Json.num thresholdSeconds),
    ("messages", Lean.Json.arr entriesJson),
    ("count", Lean.Json.num entries.size)
  ]
  Core.resourceOkFormatted cfg req "views.acks_stale" result

/-- Handle GET /resource/views/ack-overdue/:agent -/
def handleAckOverdue (db : Storage.Database) (cfg : AgentMail.Config) (req : ServerRequest) : IO Response := do
  let agentName ← match req.param "agent" with
    | some s => pure s
    | none => return Core.resourceBadRequest "missing agent parameter"

  let projectKey := req.queryParam "project"
  let limit := Core.parseLimit (req.queryParam "limit") 50 100
  let includeBodies := Core.parseBool (req.queryParam "include_bodies") false
  -- Default overdue threshold is 60 minutes
  let thresholdMinutes := Core.parseInt (req.queryParam "threshold_minutes") 60

  let project ← match projectKey with
    | some key => match ← Tools.Identity.resolveProject db key with
      | some p => pure p
      | none => return Core.resourceNotFound s!"project not found: {key}"
    | none => return Core.resourceBadRequest "missing project query parameter"

  let agent ← match ← db.queryAgentByName project.id agentName with
    | some a => pure a
    | none => return Core.resourceNotFound s!"agent not found: {agentName}"

  let entries ← db.queryAckOverdueMessages project.id agent.id thresholdMinutes limit
  let entriesJson := entries.map (inboxEntryToJson · includeBodies)

  let result := Lean.Json.mkObj [
    ("view", Lean.Json.str "ack-overdue"),
    ("agent_id", Lean.Json.num agent.id),
    ("agent_name", Lean.Json.str agent.name),
    ("project_id", Lean.Json.num project.id),
    ("threshold_minutes", Lean.Json.num thresholdMinutes),
    ("messages", Lean.Json.arr entriesJson),
    ("count", Lean.Json.num entries.size)
  ]
  Core.resourceOkFormatted cfg req "views.ack_overdue" result

end AgentMail.Resources.Views
