/-
  AgentMail.CLI.Output - Output formatting for CLI commands (JSON and text).
-/
import AgentMail.Models.Project
import AgentMail.Storage.Database
import Chronos

namespace AgentMail.CLI.Output

open AgentMail

/-- Left-pad a string to a minimum length -/
private def padLeft (s : String) (len : Nat) (c : Char) : String :=
  let padding := len - s.length
  if padding > 0 then
    String.ofList (List.replicate padding c) ++ s
  else s

/-- Right-pad a string to a minimum length -/
private def padRight (s : String) (len : Nat) (c : Char) : String :=
  let padding := len - s.length
  if padding > 0 then
    s ++ String.ofList (List.replicate padding c)
  else s

/-- Escape a string for JSON output -/
private def escapeJson (s : String) : String :=
  s.replace "\\" "\\\\"
    |>.replace "\"" "\\\""
    |>.replace "\n" "\\n"
    |>.replace "\r" "\\r"
    |>.replace "\t" "\\t"

/-- Output mode -/
inductive Mode where
  | json
  | text
  deriving BEq, Inhabited

/-- Pending acknowledgement info -/
structure PendingAck where
  messageId : Nat
  projectSlug : String
  senderName : String
  recipientName : String
  subject : String
  createdTs : Chronos.Timestamp
  deriving Repr

/-- Format a single project for output -/
def formatProject (project : Project) (mode : Mode) : String :=
  match mode with
  | .json => Lean.Json.compress (Lean.toJson project)
  | .text =>
    s!"{project.id}\t{project.slug}\t{project.humanKey}\t{project.createdAt.seconds}"

/-- Format a list of projects for output -/
def formatProjects (projects : Array Project) (mode : Mode) : String :=
  match mode with
  | .json =>
    let items := projects.map (Lean.toJson ·) |>.toList
    Lean.Json.compress (Lean.Json.arr items.toArray)
  | .text =>
    if projects.isEmpty then "No projects found."
    else
      let header := s!"Found {projects.size} project(s):\n"
      let colHeader := "ID\tSlug\t\tHuman Key\t\t\t\tCreated"
      let rows := projects.map fun p =>
        s!"{padLeft (toString p.id) 2 ' '}\t{padRight p.slug 12 ' '}\t{padRight p.humanKey 30 ' '}\t{p.createdAt.seconds}"
      header ++ colHeader ++ "\n" ++ String.intercalate "\n" rows.toList

/-- Format a list of pending acknowledgements for output -/
def formatAcks (acks : Array PendingAck) (mode : Mode) : String :=
  match mode with
  | .json =>
    let items := acks.map fun a => Lean.Json.mkObj [
      ("message_id", Lean.Json.num a.messageId),
      ("project_slug", Lean.Json.str a.projectSlug),
      ("sender_name", Lean.Json.str a.senderName),
      ("recipient_name", Lean.Json.str a.recipientName),
      ("subject", Lean.Json.str a.subject),
      ("created_ts", Lean.Json.num a.createdTs.seconds)
    ]
    Lean.Json.compress (Lean.Json.arr items)
  | .text =>
    if acks.isEmpty then "No pending acknowledgements."
    else
      let header := s!"Found {acks.size} pending acknowledgement(s):\n"
      let colHeader := "MsgID\tProject\t\tFrom\t\tTo\t\tSubject"
      let rows := acks.map fun a =>
        s!"{padLeft (toString a.messageId) 5 ' '}\t{padRight a.projectSlug 10 ' '}\t{padRight a.senderName 10 ' '}\t{padRight a.recipientName 10 ' '}\t{a.subject.take 30}"
      header ++ colHeader ++ "\n" ++ String.intercalate "\n" rows.toList

/-- Format port configuration for output -/
def formatPort (port : UInt16) (mode : Mode) : String :=
  match mode with
  | .json => s!"\{\"port\": {port}}"
  | .text => s!"Server port: {port}"

/-- Format doctor check results -/
structure DoctorResult where
  integrityOk : Bool
  orphanAgents : Nat
  orphanMessages : Nat
  orphanRecipients : Nat
  deriving Repr

def formatDoctorResult (result : DoctorResult) (mode : Mode) : String :=
  match mode with
  | .json => Lean.Json.compress (Lean.Json.mkObj [
      ("integrity_ok", Lean.Json.bool result.integrityOk),
      ("orphan_agents", Lean.Json.num result.orphanAgents),
      ("orphan_messages", Lean.Json.num result.orphanMessages),
      ("orphan_recipients", Lean.Json.num result.orphanRecipients)
    ])
  | .text =>
    let statusIcon := if result.integrityOk then "[OK]" else "[FAIL]"
    let lines := [
      s!"{statusIcon} Database integrity check",
      s!"  Orphan agents: {result.orphanAgents}",
      s!"  Orphan messages: {result.orphanMessages}",
      s!"  Orphan recipients: {result.orphanRecipients}"
    ]
    if result.integrityOk && result.orphanAgents == 0 &&
       result.orphanMessages == 0 && result.orphanRecipients == 0 then
      String.intercalate "\n" lines ++ "\n\nDatabase is healthy."
    else
      String.intercalate "\n" lines ++ "\n\nIssues detected. Run 'doctor repair' to fix."

/-- Format a success message -/
def formatSuccess (message : String) (mode : Mode) : String :=
  match mode with
  | .json => s!"\{\"status\": \"success\", \"message\": \"{escapeJson message}\"}"
  | .text => message

/-- Format an error message with optional actionable suggestion -/
def formatError (message : String) (mode : Mode) (suggestion : Option String := none) : String :=
  match mode with
  | .json =>
    let suggestionJson := match suggestion with
      | some s => s!", \"suggestion\": \"{escapeJson s}\""
      | none => ""
    s!"\{\"status\": \"error\", \"message\": \"{escapeJson message}\"{suggestionJson}}"
  | .text =>
    match suggestion with
    | some s => s!"Error: {message}\n  Hint: {s}"
    | none => s!"Error: {message}"

end AgentMail.CLI.Output
