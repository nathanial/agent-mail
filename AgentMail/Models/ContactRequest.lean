/-
  AgentMail.Models.ContactRequest - Contact request data model
-/
import Chronos
import Lean.Data.Json

namespace AgentMail

/-- Status of a contact request -/
inductive ContactRequestStatus where
  | pending
  | accepted
  | rejected
  deriving Repr, DecidableEq, Inhabited

namespace ContactRequestStatus

def toString : ContactRequestStatus → String
  | pending => "pending"
  | accepted => "accepted"
  | rejected => "rejected"

def fromString? : String → Option ContactRequestStatus
  | "pending" => some pending
  | "accepted" => some accepted
  | "rejected" => some rejected
  | _ => none

instance : Lean.ToJson ContactRequestStatus where
  toJson s := Lean.Json.str s.toString

instance : Lean.FromJson ContactRequestStatus where
  fromJson? j := do
    let s ← j.getStr?
    match fromString? s with
    | some status => pure status
    | none => throw s!"Invalid ContactRequestStatus: {s}"

end ContactRequestStatus

/-- A contact request between two agents -/
structure ContactRequest where
  id : Nat
  projectId : Nat
  fromAgentId : Nat
  toAgentId : Nat
  message : String
  status : ContactRequestStatus
  createdTs : Chronos.Timestamp
  respondedAt : Option Chronos.Timestamp
  deriving Repr

namespace ContactRequest

instance : Lean.ToJson ContactRequest where
  toJson r := Lean.Json.mkObj [
    ("id", Lean.Json.num r.id),
    ("project_id", Lean.Json.num r.projectId),
    ("from_agent_id", Lean.Json.num r.fromAgentId),
    ("to_agent_id", Lean.Json.num r.toAgentId),
    ("message", Lean.Json.str r.message),
    ("status", Lean.toJson r.status),
    ("created_ts", Lean.Json.num r.createdTs.seconds),
    ("responded_at", match r.respondedAt with
      | some ts => Lean.Json.num ts.seconds
      | none => Lean.Json.null)
  ]

instance : Lean.FromJson ContactRequest where
  fromJson? j := do
    let id ← j.getObjValAs? Nat "id"
    let projectId ← j.getObjValAs? Nat "project_id"
    let fromAgentId ← j.getObjValAs? Nat "from_agent_id"
    let toAgentId ← j.getObjValAs? Nat "to_agent_id"
    let message ← j.getObjValAs? String "message"
    let status ← j.getObjValAs? ContactRequestStatus "status"
    let createdTsSecs ← j.getObjValAs? Int "created_ts"
    let respondedAt : Option Chronos.Timestamp :=
      match j.getObjValAs? Int "responded_at" with
      | Except.ok secs => some (Chronos.Timestamp.fromSeconds secs)
      | Except.error _ => none
    pure {
      id, projectId, fromAgentId, toAgentId, message, status
      createdTs := Chronos.Timestamp.fromSeconds createdTsSecs
      respondedAt
    }

end ContactRequest

end AgentMail
