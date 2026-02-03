/-
  AgentMail.Resources.Threads - Thread summary resources for live UI
-/
import Citadel
import AgentMail.Config
import AgentMail.Storage.Database
import AgentMail.Resources.Core
import AgentMail.Tools.Identity

open Citadel

namespace AgentMail.Resources.Threads

private def threadSummaryToJson (summary : Storage.Database.ThreadSummary) (includeBodies : Bool) : Lean.Json :=
  let base := [
    ("thread_id", Lean.Json.str summary.threadId),
    ("message_count", Lean.Json.num summary.messageCount),
    ("last_message_id", Lean.Json.num summary.lastMessageId),
    ("last_subject", Lean.Json.str summary.lastSubject),
    ("last_sender_name", Lean.Json.str summary.lastSenderName),
    ("last_importance", Lean.toJson summary.lastImportance),
    ("last_ack_required", Lean.Json.bool summary.lastAckRequired),
    ("last_created_ts", Lean.Json.num summary.lastCreatedTs.seconds),
    ("unread_count", match summary.unreadCount with | some n => Lean.Json.num n | none => Lean.Json.null)
  ]
  let withBody :=
    if includeBodies then
      base ++ [("last_body_md", match summary.lastBodyMd with | some b => Lean.Json.str b | none => Lean.Json.null)]
    else
      base ++ [("last_body_md", Lean.Json.null)]
  Lean.Json.mkObj withBody

/-- Handle GET /resource/threads/:project_key -/
def handleThreads (db : Storage.Database) (cfg : AgentMail.Config) (req : ServerRequest) : IO Response := do
  let projectKey ← match req.param "project_key" with
    | some s => pure s
    | none => return Core.resourceBadRequest "missing project_key parameter"

  let limit := Core.parseLimit (req.queryParam "limit") 50 200
  let includeBodies := Core.parseBool (req.queryParam "include_bodies") false
  let agentNameOpt := req.queryParam "agent"

  let project ← match ← Tools.Identity.resolveProject db projectKey with
    | some p => pure p
    | none => return Core.resourceNotFound s!"project not found: {projectKey}"

  let agentIdOpt ← match agentNameOpt with
    | none => pure none
    | some name =>
      match ← db.queryAgentByName project.id name with
      | some agent => pure (some agent.id)
      | none => return Core.resourceNotFound s!"agent not found: {name}"

  let summaries ← db.queryThreadSummaries project.id limit agentIdOpt
  let summariesJson := summaries.map (threadSummaryToJson · includeBodies)

  let result := Lean.Json.mkObj [
    ("project_id", Lean.Json.num project.id),
    ("project_slug", Lean.Json.str project.slug),
    ("agent_name", match agentNameOpt with | some n => Lean.Json.str n | none => Lean.Json.null),
    ("threads", Lean.Json.arr summariesJson),
    ("count", Lean.Json.num summaries.size)
  ]
  Core.resourceOkFormatted cfg req "threads" result

end AgentMail.Resources.Threads
