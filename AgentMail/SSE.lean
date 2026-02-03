/-
  AgentMail.SSE - SSE manager and broadcast helpers for live UI updates
-/
import Citadel
import Lean.Data.Json

namespace AgentMail.SSE

/-- Default SSE topic for agent-mail updates. -/
def defaultTopic : String := "agent-mail"

initialize sseManagerRef : IO.Ref (Option Citadel.SSE.ConnectionManager) ← IO.mkRef none

/-- Attach a connection manager so producers can broadcast events. -/
def setManager (manager : Citadel.SSE.ConnectionManager) : IO Unit :=
  sseManagerRef.set (some manager)

/-- Remove the connection manager (disables broadcasts). -/
def clearManager : IO Unit :=
  sseManagerRef.set none

/-- Publish a JSON payload to the default topic as a named event. -/
def publish (eventType : String) (payload : Lean.Json) : IO Unit := do
  let data := Lean.Json.compress payload
  let event := Citadel.SSE.Event.named eventType data
  match ← sseManagerRef.get with
  | some manager => manager.broadcast defaultTopic event
  | none => pure ()

end AgentMail.SSE
