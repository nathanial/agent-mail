/-
  AgentMail.Resources.Discovery - Discovery-related MCP resources
-/
import Citadel
import AgentMail.Config
import AgentMail.Storage.Database
import AgentMail.Resources.Core
import AgentMail.Tools.Identity

open Citadel

namespace AgentMail.Resources.Discovery

/-- Handle GET /resource/projects -/
def handleProjects (db : Storage.Database) (cfg : AgentMail.Config) (req : ServerRequest) : IO Response := do
  let projects ← db.queryAllProjects
  let projectsJson := projects.map fun p => Lean.Json.mkObj [
    ("id", Lean.Json.num p.id),
    ("slug", Lean.Json.str p.slug),
    ("human_key", Lean.Json.str p.humanKey),
    ("created_at", Lean.Json.num p.createdAt.seconds)
  ]
  let result := Lean.Json.mkObj [
    ("projects", Lean.Json.arr projectsJson),
    ("count", Lean.Json.num projects.size)
  ]
  Core.resourceOkFormatted cfg req "projects" result

/-- Handle GET /resource/project/:slug -/
def handleProject (db : Storage.Database) (cfg : AgentMail.Config) (req : ServerRequest) : IO Response := do
  let slug ← match req.param "slug" with
    | some s => pure s
    | none => return Core.resourceBadRequest "missing slug parameter"

  -- Try slug first, then human_key
  let projectOpt ← db.queryProjectBySlug slug
  let project ← match projectOpt with
    | some p => pure p
    | none => match ← db.queryProjectByHumanKey slug with
      | some p => pure p
      | none => return Core.resourceNotFound s!"project not found: {slug}"

  let agents ← db.queryAgentsByProject project.id
  let agentJson := agents.map fun a => Lean.Json.mkObj [
    ("id", Lean.Json.num a.id),
    ("name", Lean.Json.str a.name),
    ("program", Lean.Json.str a.program),
    ("model", Lean.Json.str a.model),
    ("task_description", Lean.Json.str a.taskDescription),
    ("contact_policy", Lean.toJson a.contactPolicy),
    ("attachments_policy", Lean.toJson a.attachmentsPolicy),
    ("inception_ts", Lean.Json.num a.inceptionTs.seconds),
    ("last_active_ts", Lean.Json.num a.lastActiveTs.seconds)
  ]
  let result := Lean.Json.mkObj [
    ("id", Lean.Json.num project.id),
    ("slug", Lean.Json.str project.slug),
    ("human_key", Lean.Json.str project.humanKey),
    ("created_at", Lean.Json.num project.createdAt.seconds),
    ("agents", Lean.Json.arr agentJson),
    ("agent_count", Lean.Json.num agents.size)
  ]
  Core.resourceOkFormatted cfg req "project" result

/-- Handle GET /resource/agents/:project_key -/
def handleAgents (db : Storage.Database) (cfg : AgentMail.Config) (req : ServerRequest) : IO Response := do
  let projectKey ← match req.param "project_key" with
    | some k => pure k
    | none => return Core.resourceBadRequest "missing project_key parameter"

  let project ← match ← Tools.Identity.resolveProject db projectKey with
    | some p => pure p
    | none => return Core.resourceNotFound s!"project not found: {projectKey}"

  let agents ← db.queryAgentsByProject project.id
  let agentJson := agents.map fun a => Lean.Json.mkObj [
    ("id", Lean.Json.num a.id),
    ("name", Lean.Json.str a.name),
    ("program", Lean.Json.str a.program),
    ("model", Lean.Json.str a.model),
    ("task_description", Lean.Json.str a.taskDescription),
    ("contact_policy", Lean.toJson a.contactPolicy),
    ("attachments_policy", Lean.toJson a.attachmentsPolicy),
    ("inception_ts", Lean.Json.num a.inceptionTs.seconds),
    ("last_active_ts", Lean.Json.num a.lastActiveTs.seconds)
  ]
  let result := Lean.Json.mkObj [
    ("project_id", Lean.Json.num project.id),
    ("project_slug", Lean.Json.str project.slug),
    ("agents", Lean.Json.arr agentJson),
    ("count", Lean.Json.num agents.size)
  ]
  Core.resourceOkFormatted cfg req "agents" result

/-- Handle GET /resource/identity/:project -/
def handleIdentity (db : Storage.Database) (cfg : AgentMail.Config) (req : ServerRequest) : IO Response := do
  let projectKey ← match req.param "project" with
    | some k => pure k
    | none => return Core.resourceBadRequest "missing project parameter"

  let project ← match ← Tools.Identity.resolveProject db projectKey with
    | some p => pure p
    | none => return Core.resourceNotFound s!"project not found: {projectKey}"

  let agents ← db.queryAgentsByProject project.id
  let agentNames := agents.map (·.name)

  let result := Lean.Json.mkObj [
    ("project", Lean.Json.mkObj [
      ("id", Lean.Json.num project.id),
      ("slug", Lean.Json.str project.slug),
      ("human_key", Lean.Json.str project.humanKey),
      ("created_at", Lean.Json.num project.createdAt.seconds)
    ]),
    ("agent_names", Lean.toJson agentNames),
    ("agent_count", Lean.Json.num agents.size)
  ]
  Core.resourceOkFormatted cfg req "identity" result

/-- Handle GET /resource/product/:key -/
def handleProduct (db : Storage.Database) (cfg : AgentMail.Config) (req : ServerRequest) : IO Response := do
  let productKey ← match req.param "key" with
    | some k => pure k
    | none => return Core.resourceBadRequest "missing key parameter"

  let product ← match ← db.queryProductByProductId productKey with
    | some p => pure p
    | none => return Core.resourceNotFound s!"product not found: {productKey}"

  let linkedProjects ← db.queryProjectsByProduct product.id
  let projectsJson := linkedProjects.map fun p => Lean.Json.mkObj [
    ("id", Lean.Json.num p.id),
    ("slug", Lean.Json.str p.slug),
    ("human_key", Lean.Json.str p.humanKey),
    ("created_at", Lean.Json.num p.createdAt.seconds)
  ]

  let result := Lean.Json.mkObj [
    ("id", Lean.Json.num product.id),
    ("product_id", Lean.Json.str product.productId),
    ("created_at", Lean.Json.num product.createdAt.seconds),
    ("linked_projects", Lean.Json.arr projectsJson),
    ("project_count", Lean.Json.num linkedProjects.size)
  ]
  Core.resourceOkFormatted cfg req "product" result

end AgentMail.Resources.Discovery
