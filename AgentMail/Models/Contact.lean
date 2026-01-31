/-
  AgentMail.Models.Contact - Contact relationship data model
-/
import Chronos
import Lean.Data.Json

namespace AgentMail

/-- A bidirectional contact relationship between two agents -/
structure Contact where
  id : Nat
  projectId : Nat
  agentId1 : Nat   -- Bidirectional: stored with agentId1 < agentId2
  agentId2 : Nat
  createdTs : Chronos.Timestamp
  deriving Repr

namespace Contact

instance : Lean.ToJson Contact where
  toJson c := Lean.Json.mkObj [
    ("id", Lean.Json.num c.id),
    ("project_id", Lean.Json.num c.projectId),
    ("agent_id_1", Lean.Json.num c.agentId1),
    ("agent_id_2", Lean.Json.num c.agentId2),
    ("created_ts", Lean.Json.num c.createdTs.seconds)
  ]

instance : Lean.FromJson Contact where
  fromJson? j := do
    let id ← j.getObjValAs? Nat "id"
    let projectId ← j.getObjValAs? Nat "project_id"
    let agentId1 ← j.getObjValAs? Nat "agent_id_1"
    let agentId2 ← j.getObjValAs? Nat "agent_id_2"
    let createdTsSecs ← j.getObjValAs? Int "created_ts"
    pure {
      id, projectId, agentId1, agentId2
      createdTs := Chronos.Timestamp.fromSeconds createdTsSecs
    }

/-- Check if this contact involves a specific agent -/
def involvesAgent (c : Contact) (agentId : Nat) : Bool :=
  c.agentId1 == agentId || c.agentId2 == agentId

/-- Get the other agent in the contact relationship -/
def otherAgent (c : Contact) (agentId : Nat) : Option Nat :=
  if c.agentId1 == agentId then some c.agentId2
  else if c.agentId2 == agentId then some c.agentId1
  else none

end Contact

end AgentMail
