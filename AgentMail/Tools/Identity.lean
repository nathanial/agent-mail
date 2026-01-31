/-
  AgentMail.Tools.Identity - Identity-related MCP tool handlers
-/
import Chronos
import Citadel
import AgentMail.Config
import AgentMail.Protocol.JsonRpc
import AgentMail.Storage.Database
import AgentMail.Storage.Archive
import AgentMail.Utils.NameGenerator

open Citadel

namespace AgentMail.Tools.Identity

/-- Generate a slug from a human key (path) -/
def generateSlug (humanKey : String) : String :=
  -- Sanitize the path: lowercase, replace separators, trim
  let s := humanKey.toLower
  let s := s.replace "/" "-"
  let s := s.replace "\\" "-"
  let s := s.replace " " "-"
  let s := s.replace "_" "-"
  -- Just take last 50 chars of the sanitized string
  let chars := s.toList.reverse.take 50 |>.reverse
  -- Remove leading/trailing dashes
  let trimmed := chars.dropWhile (· == '-') |>.reverse |>.dropWhile (· == '-') |>.reverse
  String.ofList trimmed

/-- Resolve a project by human key or slug -/
def resolveProject (db : Storage.Database) (projectKey : String) : IO (Option Project) := do
  match ← db.queryProjectByHumanKey projectKey with
  | some p => pure (some p)
  | none => db.queryProjectBySlug projectKey

/-- Generate a unique slug for a new project -/
def generateUniqueSlug (db : Storage.Database) (humanKey : String) : IO String := do
  let base := 
    let slug := generateSlug humanKey
    if slug.isEmpty then "project" else slug
  let mut suffix := 0
  let mut candidate := base
  let mut done := false
  while !done do
    match ← db.queryProjectBySlug candidate with
    | none => done := true
    | some _ =>
      suffix := suffix + 1
      candidate := s!"{base}-{suffix}"
  pure candidate

/-- Handle health_check request -/
def handleHealthCheck (db : Storage.Database) (cfg : Config) (req : JsonRpc.Request) : IO Response := do
  let result := Lean.Json.mkObj [
    ("status", Lean.Json.str "ok"),
    ("environment", Lean.Json.str cfg.environment),
    ("http_host", Lean.Json.str cfg.host),
    ("http_port", Lean.Json.num cfg.port.toNat),
    ("database_url", Lean.Json.str db.path)
  ]
  let resp := JsonRpc.Response.success req.id result
  pure (Response.json (Lean.Json.compress (Lean.toJson resp)))

/-- Handle ensure_project request -/
def handleEnsureProject (db : Storage.Database) (cfg : Config) (req : JsonRpc.Request) : IO Response := do
  -- Extract params
  let params := req.params.getD Lean.Json.null
  let humanKey ← match params.getObjValAs? String "human_key" with
    | Except.ok k => pure k
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: human_key")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Check if project already exists
  match ← db.queryProjectByHumanKey humanKey with
  | some project =>
    -- Ensure archive exists
    let _ ← Storage.ensureProjectArchive cfg project.slug
    let resp := JsonRpc.Response.success req.id (Lean.toJson project)
    pure (Response.json (Lean.Json.compress (Lean.toJson resp)))
  | none =>
    let now ← Chronos.Timestamp.now
    let slug ← generateUniqueSlug db humanKey
    let id ← db.insertProject slug humanKey now
    let project : Project := { id, slug, humanKey, createdAt := now }
    let _ ← Storage.ensureProjectArchive cfg project.slug
    let resp := JsonRpc.Response.success req.id (Lean.toJson project)
    pure (Response.json (Lean.Json.compress (Lean.toJson resp)))

/-- Handle register_agent request -/
def handleRegisterAgent (db : Storage.Database) (cfg : Config) (req : JsonRpc.Request) : IO Response := do
  -- Extract params
  let params := req.params.getD Lean.Json.null

  let projectKey ← match params.getObjValAs? String "project_key" with
    | Except.ok k => pure k
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: project_key")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  let program ← match params.getObjValAs? String "program" with
    | Except.ok p => pure p
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: program")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  let model ← match params.getObjValAs? String "model" with
    | Except.ok m => pure m
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: model")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Optional params
  let nameOpt := match params.getObjValAs? String "name" with
    | Except.ok n => some n
    | Except.error _ => none

  let taskDescription := match params.getObjValAs? String "task_description" with
    | Except.ok t => t
    | Except.error _ => ""

  let attachmentsPolicy := match params.getObjValAs? String "attachments_policy" with
    | Except.ok s => AttachmentsPolicy.fromString? s |>.getD .auto
    | Except.error _ => .auto

  -- Look up project
  let project ← match ← resolveProject db projectKey with
    | some p => pure p
    | none =>
      let err := JsonRpc.Error.invalidParams (some s!"project not found: {projectKey}")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Check if agent with this name exists (if name provided)
  match nameOpt with
  | some name =>
    match ← db.queryAgentByName project.id name with
    | some existingAgent =>
      -- Update last_active_ts and return existing agent
      let now ← Chronos.Timestamp.now
      let updatedAgent := { existingAgent with
        program := program
        model := model
        taskDescription := taskDescription
        attachmentsPolicy := attachmentsPolicy
        lastActiveTs := now
      }
      db.updateAgentProfile updatedAgent
      let archive ← Storage.ensureProjectArchive cfg project.slug
      Storage.writeAgentProfile archive updatedAgent
      let resp := JsonRpc.Response.success req.id (Lean.toJson updatedAgent)
      pure (Response.json (Lean.Json.compress (Lean.toJson resp)))
    | none =>
      -- Create agent with provided name
      let now ← Chronos.Timestamp.now
      let agent : Agent := {
        id := 0  -- Will be set by insert
        projectId := project.id
        name := name
        program := program
        model := model
        taskDescription := taskDescription
        contactPolicy := .auto
        attachmentsPolicy := attachmentsPolicy
        inceptionTs := now
        lastActiveTs := now
      }
      let id ← db.insertAgent agent
      let newAgent := { agent with id }
      let archive ← Storage.ensureProjectArchive cfg project.slug
      Storage.writeAgentProfile archive newAgent
      let resp := JsonRpc.Response.success req.id (Lean.toJson newAgent)
      pure (Response.json (Lean.Json.compress (Lean.toJson resp)))
  | none =>
    -- Generate a unique name
    let now ← Chronos.Timestamp.now
    let mut attempts := 0
    let mut chosen : Option String := none
    while attempts < 128 && chosen.isNone do
      let candidate ← Utils.NameGenerator.generateName
      match ← db.queryAgentByName project.id candidate with
      | none => chosen := some candidate
      | some _ => attempts := attempts + 1
    let name ← match chosen with
      | some n => pure n
      | none =>
        let err := JsonRpc.Error.internalError (some "unable to generate unique agent name")
        let resp := JsonRpc.Response.failure req.id err
        return Response.json (Lean.Json.compress (Lean.toJson resp))
    -- Create agent
    let agent : Agent := {
      id := 0
      projectId := project.id
      name := name
      program := program
      model := model
      taskDescription := taskDescription
      contactPolicy := .auto
      attachmentsPolicy := attachmentsPolicy
      inceptionTs := now
      lastActiveTs := now
    }
    let id ← db.insertAgent agent
    let newAgent := { agent with id }
    let archive ← Storage.ensureProjectArchive cfg project.slug
    Storage.writeAgentProfile archive newAgent
    let resp := JsonRpc.Response.success req.id (Lean.toJson newAgent)
    pure (Response.json (Lean.Json.compress (Lean.toJson resp)))

/-- Handle whois request -/
def handleWhois (db : Storage.Database) (_cfg : Config) (req : JsonRpc.Request) : IO Response := do
  -- Extract params
  let params := req.params.getD Lean.Json.null

  let projectKey ← match params.getObjValAs? String "project_key" with
    | Except.ok k => pure k
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: project_key")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  let agentName ← match params.getObjValAs? String "agent_name" with
    | Except.ok n => pure n
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: agent_name")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Look up project
  let project ← match ← resolveProject db projectKey with
    | some p => pure p
    | none =>
      let err := JsonRpc.Error.invalidParams (some s!"project not found: {projectKey}")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Look up agent
  match ← db.queryAgentByName project.id agentName with
  | some agent =>
    -- Return agent profile (without commits for now - Phase 6)
    let result := Lean.Json.mkObj [
      ("id", Lean.Json.num agent.id),
      ("name", Lean.Json.str agent.name),
      ("program", Lean.Json.str agent.program),
      ("model", Lean.Json.str agent.model),
      ("task_description", Lean.Json.str agent.taskDescription),
      ("contact_policy", Lean.toJson agent.contactPolicy),
      ("attachments_policy", Lean.toJson agent.attachmentsPolicy),
      ("inception_ts", Lean.Json.num agent.inceptionTs.seconds),
      ("last_active_ts", Lean.Json.num agent.lastActiveTs.seconds),
      ("recent_commits", Lean.Json.arr #[])
    ]
    let resp := JsonRpc.Response.success req.id result
    pure (Response.json (Lean.Json.compress (Lean.toJson resp)))
  | none =>
    let err := JsonRpc.Error.invalidParams (some s!"agent not found: {agentName}")
    let resp := JsonRpc.Response.failure req.id err
    pure (Response.json (Lean.Json.compress (Lean.toJson resp)))

end AgentMail.Tools.Identity
