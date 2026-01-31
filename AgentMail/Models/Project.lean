/-
  AgentMail.Models.Project - Project data model
-/
import Chronos

namespace AgentMail

/-- A project represents a workspace or codebase that agents work on -/
structure Project where
  id : Nat
  slug : String
  humanKey : String
  createdAt : Chronos.Timestamp
  deriving Repr

namespace Project

instance : Lean.ToJson Project where
  toJson p := Lean.Json.mkObj [
    ("id", Lean.Json.num p.id),
    ("slug", Lean.Json.str p.slug),
    ("human_key", Lean.Json.str p.humanKey),
    ("created_at", Lean.Json.num p.createdAt.seconds)
  ]

instance : Lean.FromJson Project where
  fromJson? j := do
    let id ← j.getObjValAs? Nat "id"
    let slug ← j.getObjValAs? String "slug"
    let humanKey ← j.getObjValAs? String "human_key"
    let createdAtSecs ← j.getObjValAs? Int "created_at"
    pure {
      id := id
      slug := slug
      humanKey := humanKey
      createdAt := Chronos.Timestamp.fromSeconds createdAtSecs
    }

end Project

end AgentMail
