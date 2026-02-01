/-
  AgentMail.Notifications - Filesystem signal notifications for agents
-/
import Lean.Data.Json

namespace AgentMail.Notifications

/-- Notification configuration -/
structure NotificationConfig where
  /-- Whether notifications are enabled -/
  enabled : Bool := false
  /-- Directory for signal files -/
  signalsDir : String := "~/.mcp_agent_mail/signals"
  /-- Whether to include metadata in signal files -/
  includeMetadata : Bool := true
  /-- Debounce time in milliseconds -/
  debounceMs : Nat := 100
  deriving Repr, Inhabited

namespace NotificationConfig

/-- Default configuration (disabled) -/
def default : NotificationConfig := {}

end NotificationConfig

/-- Expand tilde in path to home directory -/
private def expandTilde (path : String) : IO String := do
  if path.startsWith "~/" then
    match ← IO.getEnv "HOME" with
    | some home => pure (home ++ path.drop 1)
    | none => pure path
  else
    pure path

/-- Ensure directory exists, creating it if necessary -/
private def ensureDir (path : String) : IO Unit := do
  let expanded ← expandTilde path
  -- Use mkdir -p to create directory and parents
  let _ ← IO.Process.output {
    cmd := "mkdir"
    args := #["-p", expanded]
  }

/-- Get the signal file path for an agent -/
def getSignalPath (config : NotificationConfig) (projectSlug agentName : String) : IO String := do
  let base ← expandTilde config.signalsDir
  pure s!"{base}/{projectSlug}/{agentName}.signal"

/-- Notification metadata -/
structure NotificationMetadata where
  /-- Timestamp of the notification -/
  timestamp : Nat
  /-- Type of event (e.g., "new_message", "ack_required") -/
  eventType : String
  /-- Message ID if applicable -/
  messageId : Option String := none
  /-- Thread ID if applicable -/
  threadId : Option String := none
  /-- From agent -/
  fromAgent : Option String := none
  /-- Priority level -/
  priority : Option String := none
  deriving Repr

instance : Lean.ToJson NotificationMetadata where
  toJson m := Lean.Json.mkObj [
    ("timestamp", Lean.Json.num m.timestamp),
    ("event_type", Lean.Json.str m.eventType),
    ("message_id", match m.messageId with | some id => Lean.Json.str id | none => Lean.Json.null),
    ("thread_id", match m.threadId with | some id => Lean.Json.str id | none => Lean.Json.null),
    ("from_agent", match m.fromAgent with | some a => Lean.Json.str a | none => Lean.Json.null),
    ("priority", match m.priority with | some p => Lean.Json.str p | none => Lean.Json.null)
  ]

/-- Touch signal file to notify agent of event -/
def notifyAgent (config : NotificationConfig) (projectSlug agentName : String)
    (eventType : String) (metadata : Option NotificationMetadata := none) : IO Unit := do
  if !config.enabled then
    return

  -- Ensure signals directory exists
  let signalDir ← expandTilde s!"{config.signalsDir}/{projectSlug}"
  ensureDir signalDir

  let signalPath ← getSignalPath config projectSlug agentName

  if config.includeMetadata then
    -- Write metadata to signal file
    let m := match metadata with
      | some m => m
      | none =>
        let now ← IO.monoMsNow
        { timestamp := now, eventType }
    let content := Lean.Json.compress (Lean.toJson m)
    IO.FS.writeFile signalPath content
  else
    -- Just touch the file (write empty content or use touch command)
    let _ ← IO.Process.output {
      cmd := "touch"
      args := #[signalPath]
    }

/-- Notify agent of a new message -/
def notifyNewMessage (config : NotificationConfig) (projectSlug agentName : String)
    (messageId threadId fromAgent : String) (priority : Option String := none) : IO Unit := do
  let now ← IO.monoMsNow
  let m : NotificationMetadata := {
    timestamp := now
    eventType := "new_message"
    messageId := some messageId
    threadId := some threadId
    fromAgent := some fromAgent
    priority := priority
  }
  notifyAgent config projectSlug agentName "new_message" (some m)

/-- Notify agent of an acknowledgment required -/
def notifyAckRequired (config : NotificationConfig) (projectSlug agentName : String)
    (messageId threadId fromAgent : String) : IO Unit := do
  let now ← IO.monoMsNow
  let m : NotificationMetadata := {
    timestamp := now
    eventType := "ack_required"
    messageId := some messageId
    threadId := some threadId
    fromAgent := some fromAgent
    priority := some "high"
  }
  notifyAgent config projectSlug agentName "ack_required" (some m)

/-- Clear signal file for an agent -/
def clearSignal (config : NotificationConfig) (projectSlug agentName : String) : IO Unit := do
  if !config.enabled then
    return

  let signalPath ← getSignalPath config projectSlug agentName

  -- Remove signal file if it exists
  let _ ← IO.Process.output {
    cmd := "rm"
    args := #["-f", signalPath]
  }

end AgentMail.Notifications
