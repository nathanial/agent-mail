/-
  AgentMail.Resources.Core - Common types and helpers for MCP Resources
-/
import Citadel
import Lean.Data.Json
import AgentMail.Config
import AgentMail.OutputFormat

open Citadel
open Herald.Core (StatusCode)

namespace AgentMail.Resources.Core

/-- Parse a boolean query parameter -/
def parseBool (value : Option String) (default : Bool := false) : Bool :=
  match value with
  | none => default
  | some s =>
    let lower := s.toLower
    lower == "true" || lower == "1" || lower == "yes"

/-- Parse an integer query parameter -/
def parseInt (value : Option String) (default : Nat := 0) : Nat :=
  match value with
  | none => default
  | some s => s.toNat?.getD default

/-- Parse a limit query parameter with a maximum bound -/
def parseLimit (value : Option String) (default : Nat := 50) (max : Nat := 100) : Nat :=
  let parsed := parseInt value default
  if parsed > max then max else parsed

/-- Create a JSON error response for resource not found -/
def resourceNotFound (message : String) : Response :=
  let json := Lean.Json.mkObj [
    ("error", Lean.Json.str "not_found"),
    ("message", Lean.Json.str message)
  ]
  ResponseBuilder.withStatus StatusCode.notFound
    |>.withText (Lean.Json.compress json)
    |>.withContentType "application/json"
    |>.build

/-- Create a JSON error response for bad request -/
def resourceBadRequest (message : String) : Response :=
  let json := Lean.Json.mkObj [
    ("error", Lean.Json.str "bad_request"),
    ("message", Lean.Json.str message)
  ]
  ResponseBuilder.withStatus StatusCode.badRequest
    |>.withText (Lean.Json.compress json)
    |>.withContentType "application/json"
    |>.build

/-- Create a JSON success response -/
def resourceOk (json : Lean.Json) : Response :=
  Response.json (Lean.Json.compress json)

/-- Create a JSON success response with optional output formatting. -/
def resourceOkFormatted (cfg : AgentMail.Config) (req : ServerRequest) (name : String) (json : Lean.Json) : IO Response := do
  let formatValue := req.queryParam "format"
  match ← AgentMail.OutputFormat.apply json cfg formatValue name with
  | .ok payload => pure (Response.json (Lean.Json.compress payload))
  | .error e => pure (resourceBadRequest e)

end AgentMail.Resources.Core
