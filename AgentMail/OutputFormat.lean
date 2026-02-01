/-
  AgentMail.OutputFormat - Output format handling for tool/resource responses
-/
import Lean.Data.Json
import AgentMail.Config

namespace AgentMail.OutputFormat

/-- Decision about output formatting. -/
structure FormatDecision where
  resolved : String
  source : String
  requested : Option String
  deriving Repr

/-- Values that mean "use default/auto". -/
private def autoValues : List String := ["", "auto", "default", "none", "null"]

private def normalize (value : Option String) : Except String (Option String) :=
  match value with
  | none => .ok none
  | some raw =>
    let text := raw.trim.toLower
    if autoValues.contains text then
      .ok none
    else
      let normalized :=
        match text with
        | "application/json" => "json"
        | "text/json" => "json"
        | "application/toon" => "toon"
        | "text/toon" => "toon"
        | _ => text
      if normalized == "json" || normalized == "toon" then
        .ok (some normalized)
      else
        .error s!"Invalid format '{raw}'. Expected 'json' or 'toon'."

/-- Resolve an output format based on explicit value or defaults. -/
def resolve (value : Option String) (cfg : AgentMail.Config) : Except String FormatDecision := do
  let normalized ← normalize value
  match normalized with
  | some v => pure { resolved := v, source := "param", requested := some v }
  | none =>
    let defaultRaw := if !cfg.outputFormatDefault.isEmpty then some cfg.outputFormatDefault else
      if !cfg.toonDefaultFormat.isEmpty then some cfg.toonDefaultFormat else none
    let defaultNormalized := match normalize defaultRaw with
      | .ok v => v
      | .error _ => none
    match defaultNormalized with
    | some v => pure { resolved := v, source := "default", requested := some v }
    | none => pure { resolved := "json", source := "implicit", requested := none }

private def splitCommand (raw : String) : Array String :=
  raw.splitOn " " |>.filter (fun s => !s.isEmpty) |>.toArray

private def toonCommand (cfg : AgentMail.Config) : Array String :=
  let raw := cfg.toonBin.trim
  if raw.isEmpty then
    #["tru"]
  else
    splitCommand raw

private def buildToonMeta (decision : FormatDecision) (encoder : Option String) (extra : List (String × Lean.Json) := []) : Lean.Json :=
  let base := [
    ("requested", Lean.Json.str (decision.requested.getD "toon")),
    ("source", Lean.Json.str decision.source)
  ] ++
  (match encoder with
    | some enc => [("encoder", Lean.Json.str enc)]
    | none => [])
  Lean.Json.mkObj (base ++ extra)

private def encodeToon (payload : Lean.Json) (cfg : AgentMail.Config) (decision : FormatDecision) (_name : String) : IO Lean.Json := do
  let jsonPayload := Lean.Json.compress payload
  let cmdParts := toonCommand cfg
  if cmdParts.isEmpty then
    pure (Lean.Json.mkObj [
      ("format", Lean.Json.str "json"),
      ("data", payload),
      ("meta", buildToonMeta decision none [
        ("toon_error", Lean.Json.str "TOON encoder not configured")
      ])
    ])
  else
    let cmd := cmdParts[0]!
    let args := (cmdParts.toList.drop 1).toArray ++
      #["--encode"] ++
      (if cfg.toonStatsEnabled then #["--stats"] else #[])
    try
      let result ← IO.Process.output { cmd := cmd, args := args } (some jsonPayload)
      if result.exitCode != 0 then
        pure (Lean.Json.mkObj [
          ("format", Lean.Json.str "json"),
          ("data", payload),
          ("meta", buildToonMeta decision (some cmd) [
            ("toon_error", Lean.Json.str s!"TOON encoder exited with {result.exitCode}"),
            ("toon_stderr", Lean.Json.str result.stderr.trim)
          ])
        ])
      else
        pure (Lean.Json.mkObj [
          ("format", Lean.Json.str "toon"),
          ("data", Lean.Json.str result.stdout.trim),
          ("meta", buildToonMeta decision (some cmd))
        ])
    catch _ =>
      pure (Lean.Json.mkObj [
        ("format", Lean.Json.str "json"),
        ("data", payload),
        ("meta", buildToonMeta decision (some cmd) [
          ("toon_error", Lean.Json.str "TOON encoder failed")
        ])
      ])

/-- Apply output formatting to a JSON payload. -/
def apply (payload : Lean.Json) (cfg : AgentMail.Config) (formatValue : Option String) (name : String) : IO (Except String Lean.Json) := do
  match resolve formatValue cfg with
  | .error e => pure (.error e)
  | .ok decision =>
    if decision.resolved == "toon" then
      let formatted ← encodeToon payload cfg decision name
      pure (.ok formatted)
    else
      pure (.ok payload)

end AgentMail.OutputFormat
