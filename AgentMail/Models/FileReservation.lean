/-
  AgentMail.Models.FileReservation - File reservation data model
-/
import Chronos

namespace AgentMail

/-- A file reservation claims exclusive or shared access to file paths -/
structure FileReservation where
  id : Nat
  projectId : Nat
  agentId : Nat
  pathPattern : String
  exclusive : Bool
  reason : String
  createdTs : Chronos.Timestamp
  expiresTs : Chronos.Timestamp
  releasedTs : Option Chronos.Timestamp
  deriving Repr

instance : Inhabited FileReservation where
  default := {
    id := 0
    projectId := 0
    agentId := 0
    pathPattern := ""
    exclusive := true
    reason := ""
    createdTs := Chronos.Timestamp.fromSeconds 0
    expiresTs := Chronos.Timestamp.fromSeconds 0
    releasedTs := none
  }

namespace FileReservation

instance : Lean.ToJson FileReservation where
  toJson r := Lean.Json.mkObj [
    ("id", Lean.Json.num r.id),
    ("project_id", Lean.Json.num r.projectId),
    ("agent_id", Lean.Json.num r.agentId),
    ("path_pattern", Lean.Json.str r.pathPattern),
    ("exclusive", Lean.Json.bool r.exclusive),
    ("reason", Lean.Json.str r.reason),
    ("created_ts", Lean.Json.num r.createdTs.seconds),
    ("expires_ts", Lean.Json.num r.expiresTs.seconds),
    ("released_ts", match r.releasedTs with
      | some t => Lean.Json.num t.seconds
      | none => Lean.Json.null)
  ]

instance : Lean.FromJson FileReservation where
  fromJson? j := do
    let id ← j.getObjValAs? Nat "id"
    let projectId ← j.getObjValAs? Nat "project_id"
    let agentId ← j.getObjValAs? Nat "agent_id"
    let pathPattern ← j.getObjValAs? String "path_pattern"
    let exclusive ← j.getObjValAs? Bool "exclusive"
    let reason ← j.getObjValAs? String "reason"
    let createdTsSecs ← j.getObjValAs? Int "created_ts"
    let expiresTsSecs ← j.getObjValAs? Int "expires_ts"
    let releasedTs : Option Chronos.Timestamp := match j.getObjVal? "released_ts" with
      | Except.ok v => v.getInt?.toOption.map Chronos.Timestamp.fromSeconds
      | Except.error _ => none
    pure {
      id := id
      projectId := projectId
      agentId := agentId
      pathPattern := pathPattern
      exclusive := exclusive
      reason := reason
      createdTs := Chronos.Timestamp.fromSeconds createdTsSecs
      expiresTs := Chronos.Timestamp.fromSeconds expiresTsSecs
      releasedTs := releasedTs
    }

/-- Check if a reservation is currently active (not expired, not released) -/
def isActive (r : FileReservation) (now : Chronos.Timestamp) : Bool :=
  r.releasedTs.isNone && now.seconds < r.expiresTs.seconds

end FileReservation

end AgentMail
