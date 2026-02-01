/-
  AgentMail.Resources.FileReservations - File reservation MCP resources
-/
import Citadel
import Chronos
import AgentMail.Config
import AgentMail.Storage.Database
import AgentMail.Resources.Core
import AgentMail.Tools.Identity

open Citadel

namespace AgentMail.Resources.FileReservations

/-- Handle GET /resource/file_reservations/:slug -/
def handleFileReservations (db : Storage.Database) (cfg : AgentMail.Config) (req : ServerRequest) : IO Response := do
  let slug ← match req.param "slug" with
    | some s => pure s
    | none => return Core.resourceBadRequest "missing slug parameter"

  let project ← match ← Tools.Identity.resolveProject db slug with
    | some p => pure p
    | none => return Core.resourceNotFound s!"project not found: {slug}"

  let now ← Chronos.Timestamp.now
  let reservations ← db.queryActiveFileReservations project.id now

  -- Get agent names for each reservation
  let mut reservationsJson : Array Lean.Json := #[]
  for r in reservations do
    let agentName ← match ← db.queryAgentById r.agentId with
      | some a => pure a.name
      | none => pure "unknown"
    let json := Lean.Json.mkObj [
      ("id", Lean.Json.num r.id),
      ("agent_id", Lean.Json.num r.agentId),
      ("agent_name", Lean.Json.str agentName),
      ("path_pattern", Lean.Json.str r.pathPattern),
      ("exclusive", Lean.Json.bool r.exclusive),
      ("reason", Lean.Json.str r.reason),
      ("created_ts", Lean.Json.num r.createdTs.seconds),
      ("expires_ts", Lean.Json.num r.expiresTs.seconds),
      ("ttl_seconds", Lean.Json.num (r.expiresTs.seconds - now.seconds))
    ]
    reservationsJson := reservationsJson.push json

  let result := Lean.Json.mkObj [
    ("project_id", Lean.Json.num project.id),
    ("project_slug", Lean.Json.str project.slug),
    ("reservations", Lean.Json.arr reservationsJson),
    ("count", Lean.Json.num reservationsJson.size)
  ]
  Core.resourceOkFormatted cfg req "file_reservations" result

end AgentMail.Resources.FileReservations
