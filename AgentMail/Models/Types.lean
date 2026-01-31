/-
  AgentMail.Models.Types - Shared enumeration types for agent-mail
-/
import Lean.Data.Json

namespace AgentMail

/-- Contact policy for an agent -/
inductive ContactPolicy where
  | openPolicy   -- Accept messages from anyone
  | auto         -- Automatic filtering
  | contactsOnly -- Only from known contacts
  | blockAll     -- Block all incoming messages
  deriving Repr, DecidableEq, Inhabited

namespace ContactPolicy

def toString : ContactPolicy → String
  | openPolicy => "open"
  | auto => "auto"
  | contactsOnly => "contacts_only"
  | blockAll => "block_all"

def fromString? : String → Option ContactPolicy
  | "open" => some openPolicy
  | "auto" => some auto
  | "contacts_only" => some contactsOnly
  | "block_all" => some blockAll
  | _ => none

instance : Lean.ToJson ContactPolicy where
  toJson p := Lean.Json.str p.toString

instance : Lean.FromJson ContactPolicy where
  fromJson? j := do
    let s ← j.getStr?
    match fromString? s with
    | some p => pure p
    | none => throw s!"Invalid ContactPolicy: {s}"

end ContactPolicy

/-- Attachments policy for an agent -/
inductive AttachmentsPolicy where
  | auto    -- Default behavior (server decides)
  | inline  -- Inline attachments when possible
  | file    -- Store attachments as files
  deriving Repr, DecidableEq, Inhabited

namespace AttachmentsPolicy

def toString : AttachmentsPolicy → String
  | auto => "auto"
  | inline => "inline"
  | file => "file"

def fromString? : String → Option AttachmentsPolicy
  | "auto" => some auto
  | "inline" => some inline
  | "file" => some file
  | _ => none

instance : Lean.ToJson AttachmentsPolicy where
  toJson p := Lean.Json.str p.toString

instance : Lean.FromJson AttachmentsPolicy where
  fromJson? j := do
    let s ← j.getStr?
    match fromString? s with
    | some p => pure p
    | none => throw s!"Invalid AttachmentsPolicy: {s}"

end AttachmentsPolicy

/-- Message importance level -/
inductive Importance where
  | low
  | normal
  | high
  | urgent
  deriving Repr, DecidableEq, Inhabited

namespace Importance

def toString : Importance → String
  | low => "low"
  | normal => "normal"
  | high => "high"
  | urgent => "urgent"

def fromString? : String → Option Importance
  | "low" => some low
  | "normal" => some normal
  | "high" => some high
  | "urgent" => some urgent
  | _ => none

instance : Lean.ToJson Importance where
  toJson i := Lean.Json.str i.toString

instance : Lean.FromJson Importance where
  fromJson? j := do
    let s ← j.getStr?
    match fromString? s with
    | some i => pure i
    | none => throw s!"Invalid Importance: {s}"

end Importance

/-- Recipient type in a message -/
inductive RecipientType where
  | toRecipient  -- Primary recipient
  | cc           -- Carbon copy
  | bcc          -- Blind carbon copy
  deriving Repr, DecidableEq, Inhabited

namespace RecipientType

def toString : RecipientType → String
  | toRecipient => "to"
  | cc => "cc"
  | bcc => "bcc"

def fromString? : String → Option RecipientType
  | "to" => some toRecipient
  | "cc" => some cc
  | "bcc" => some bcc
  | _ => none

instance : Lean.ToJson RecipientType where
  toJson r := Lean.Json.str r.toString

instance : Lean.FromJson RecipientType where
  fromJson? j := do
    let s ← j.getStr?
    match fromString? s with
    | some r => pure r
    | none => throw s!"Invalid RecipientType: {s}"

end RecipientType

end AgentMail
