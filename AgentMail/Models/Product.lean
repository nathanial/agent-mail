/-
  AgentMail.Models.Product - Product namespace model for cross-project coordination
-/
import Chronos

namespace AgentMail

/-- A product is a namespace that groups multiple projects together -/
structure Product where
  id : Nat
  productId : String  -- user-provided identifier (e.g., "my-product")
  createdAt : Chronos.Timestamp
  deriving Repr

namespace Product

instance : Lean.ToJson Product where
  toJson p := Lean.Json.mkObj [
    ("id", Lean.Json.num p.id),
    ("product_id", Lean.Json.str p.productId),
    ("created_at", Lean.Json.num p.createdAt.seconds)
  ]

instance : Lean.FromJson Product where
  fromJson? j := do
    let id ← j.getObjValAs? Nat "id"
    let productId ← j.getObjValAs? String "product_id"
    let createdAtSecs ← j.getObjValAs? Int "created_at"
    pure {
      id := id
      productId := productId
      createdAt := Chronos.Timestamp.fromSeconds createdAtSecs
    }

end Product

/-- A link between a product and a project -/
structure ProductProject where
  id : Nat
  productDbId : Nat   -- FK to products.id
  projectId : Nat     -- FK to projects.id
  linkedAt : Chronos.Timestamp
  deriving Repr

namespace ProductProject

instance : Lean.ToJson ProductProject where
  toJson pp := Lean.Json.mkObj [
    ("id", Lean.Json.num pp.id),
    ("product_db_id", Lean.Json.num pp.productDbId),
    ("project_id", Lean.Json.num pp.projectId),
    ("linked_at", Lean.Json.num pp.linkedAt.seconds)
  ]

instance : Lean.FromJson ProductProject where
  fromJson? j := do
    let id ← j.getObjValAs? Nat "id"
    let productDbId ← j.getObjValAs? Nat "product_db_id"
    let projectId ← j.getObjValAs? Nat "project_id"
    let linkedAtSecs ← j.getObjValAs? Int "linked_at"
    pure {
      id := id
      productDbId := productDbId
      projectId := projectId
      linkedAt := Chronos.Timestamp.fromSeconds linkedAtSecs
    }

end ProductProject

end AgentMail
