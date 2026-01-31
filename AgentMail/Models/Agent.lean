/-
  AgentMail.Models.Agent - Agent data model
-/
import Chronos
import AgentMail.Models.Types

namespace AgentMail

/-- An agent represents a Claude Code instance working on a project -/
structure Agent where
  id : Nat
  projectId : Nat
  name : String
  program : String
  model : String
  taskDescription : String
  contactPolicy : ContactPolicy
  attachmentsPolicy : AttachmentsPolicy
  inceptionTs : Chronos.Timestamp
  lastActiveTs : Chronos.Timestamp
  deriving Repr

namespace Agent

instance : Lean.ToJson Agent where
  toJson a := Lean.Json.mkObj [
    ("id", Lean.Json.num a.id),
    ("project_id", Lean.Json.num a.projectId),
    ("name", Lean.Json.str a.name),
    ("program", Lean.Json.str a.program),
    ("model", Lean.Json.str a.model),
    ("task_description", Lean.Json.str a.taskDescription),
    ("contact_policy", Lean.toJson a.contactPolicy),
    ("attachments_policy", Lean.toJson a.attachmentsPolicy),
    ("inception_ts", Lean.Json.num a.inceptionTs.seconds),
    ("last_active_ts", Lean.Json.num a.lastActiveTs.seconds)
  ]

instance : Lean.FromJson Agent where
  fromJson? j := do
    let id ← j.getObjValAs? Nat "id"
    let projectId ← j.getObjValAs? Nat "project_id"
    let name ← j.getObjValAs? String "name"
    let program ← j.getObjValAs? String "program"
    let model ← j.getObjValAs? String "model"
    let taskDescription ← j.getObjValAs? String "task_description"
    let contactPolicy ← j.getObjValAs? ContactPolicy "contact_policy"
    let attachmentsPolicy ← j.getObjValAs? AttachmentsPolicy "attachments_policy"
    let inceptionTsSecs ← j.getObjValAs? Int "inception_ts"
    let lastActiveTsSecs ← j.getObjValAs? Int "last_active_ts"
    pure {
      id := id
      projectId := projectId
      name := name
      program := program
      model := model
      taskDescription := taskDescription
      contactPolicy := contactPolicy
      attachmentsPolicy := attachmentsPolicy
      inceptionTs := Chronos.Timestamp.fromSeconds inceptionTsSecs
      lastActiveTs := Chronos.Timestamp.fromSeconds lastActiveTsSecs
    }

end Agent

end AgentMail
