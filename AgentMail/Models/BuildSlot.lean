/-
  AgentMail.Models.BuildSlot - Build slot data model for exclusive build access
-/
import Chronos

namespace AgentMail

/-- A build slot claims exclusive access to a named resource (like "build", "deploy") -/
structure BuildSlot where
  id : Nat
  projectId : Nat
  agentId : Nat
  slotName : String
  createdTs : Chronos.Timestamp
  expiresTs : Chronos.Timestamp
  releasedTs : Option Chronos.Timestamp
  deriving Repr

instance : Inhabited BuildSlot where
  default := {
    id := 0
    projectId := 0
    agentId := 0
    slotName := ""
    createdTs := Chronos.Timestamp.fromSeconds 0
    expiresTs := Chronos.Timestamp.fromSeconds 0
    releasedTs := none
  }

namespace BuildSlot

instance : Lean.ToJson BuildSlot where
  toJson s := Lean.Json.mkObj [
    ("id", Lean.Json.num s.id),
    ("project_id", Lean.Json.num s.projectId),
    ("agent_id", Lean.Json.num s.agentId),
    ("slot_name", Lean.Json.str s.slotName),
    ("created_ts", Lean.Json.num s.createdTs.seconds),
    ("expires_ts", Lean.Json.num s.expiresTs.seconds),
    ("released_ts", match s.releasedTs with
      | some t => Lean.Json.num t.seconds
      | none => Lean.Json.null)
  ]

instance : Lean.FromJson BuildSlot where
  fromJson? j := do
    let id ← j.getObjValAs? Nat "id"
    let projectId ← j.getObjValAs? Nat "project_id"
    let agentId ← j.getObjValAs? Nat "agent_id"
    let slotName ← j.getObjValAs? String "slot_name"
    let createdTsSecs ← j.getObjValAs? Int "created_ts"
    let expiresTsSecs ← j.getObjValAs? Int "expires_ts"
    let releasedTs : Option Chronos.Timestamp := match j.getObjVal? "released_ts" with
      | Except.ok v => v.getInt?.toOption.map Chronos.Timestamp.fromSeconds
      | Except.error _ => none
    pure {
      id := id
      projectId := projectId
      agentId := agentId
      slotName := slotName
      createdTs := Chronos.Timestamp.fromSeconds createdTsSecs
      expiresTs := Chronos.Timestamp.fromSeconds expiresTsSecs
      releasedTs := releasedTs
    }

/-- Check if a build slot is currently active (not expired, not released) -/
def isActive (s : BuildSlot) (now : Chronos.Timestamp) : Bool :=
  s.releasedTs.isNone && now.seconds < s.expiresTs.seconds

end BuildSlot

end AgentMail
