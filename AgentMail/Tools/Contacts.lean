/-
  AgentMail.Tools.Contacts - Contact management MCP tool handlers
-/
import Chronos
import Citadel
import AgentMail.Protocol.JsonRpc
import AgentMail.Storage.Database
import AgentMail.Tools.Identity

open Citadel

namespace AgentMail.Tools.Contacts

/-- Handle request_contact request -/
def handleRequestContact (db : Storage.Database) (req : JsonRpc.Request) : IO Response := do
  let params := req.params.getD Lean.Json.null

  -- Extract required params
  let projectKey ← match params.getObjValAs? String "project_key" with
    | Except.ok k => pure k
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: project_key")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  let fromAgentName ← match params.getObjValAs? String "from_agent" with
    | Except.ok n => pure n
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: from_agent")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  let toAgentName ← match params.getObjValAs? String "to_agent" with
    | Except.ok n => pure n
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: to_agent")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Optional message
  let message := match params.getObjValAs? String "message" with
    | Except.ok m => m
    | Except.error _ => ""

  -- Resolve project
  let project ← match ← Identity.resolveProject db projectKey with
    | some p => pure p
    | none =>
      let err := JsonRpc.Error.invalidParams (some s!"project not found: {projectKey}")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Resolve from_agent
  let fromAgent ← match ← db.queryAgentByName project.id fromAgentName with
    | some a => pure a
    | none =>
      let err := JsonRpc.Error.invalidParams (some s!"from_agent not found: {fromAgentName}")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Resolve to_agent
  let toAgent ← match ← db.queryAgentByName project.id toAgentName with
    | some a => pure a
    | none =>
      let err := JsonRpc.Error.invalidParams (some s!"to_agent not found: {toAgentName}")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Check for self-contact
  if fromAgent.id == toAgent.id then
    let err := JsonRpc.Error.invalidParams (some "cannot request contact with yourself")
    let resp := JsonRpc.Response.failure req.id err
    return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Check if contact already exists
  match ← db.queryContactBetween project.id fromAgent.id toAgent.id with
  | some _ =>
    let err := JsonRpc.Error.invalidParams (some s!"contact already exists with {toAgentName}")
    let resp := JsonRpc.Response.failure req.id err
    return Response.json (Lean.Json.compress (Lean.toJson resp))
  | none => pure ()

  -- Check for existing pending request
  match ← db.queryContactRequestBetween project.id fromAgent.id toAgent.id with
  | some existing =>
    if existing.status == .pending then
      -- Return existing pending request
      let result := Lean.Json.mkObj [
        ("request_id", Lean.Json.num existing.id),
        ("status", Lean.toJson existing.status),
        ("from_agent", Lean.Json.str fromAgentName),
        ("to_agent", Lean.Json.str toAgentName),
        ("created_ts", Lean.Json.num existing.createdTs.seconds),
        ("already_exists", Lean.Json.bool true)
      ]
      let resp := JsonRpc.Response.success req.id result
      return Response.json (Lean.Json.compress (Lean.toJson resp))
    else
      -- Refresh an existing non-pending request
      if toAgent.contactPolicy == .blockAll then
        let err := JsonRpc.Error.invalidParams (some s!"{toAgentName} is not accepting contact requests")
        let resp := JsonRpc.Response.failure req.id err
        return Response.json (Lean.Json.compress (Lean.toJson resp))
      let now ← Chronos.Timestamp.now
      db.resetContactRequestToPending existing.id message now
      let result := Lean.Json.mkObj [
        ("request_id", Lean.Json.num existing.id),
        ("status", Lean.Json.str "pending"),
        ("from_agent", Lean.Json.str fromAgentName),
        ("to_agent", Lean.Json.str toAgentName),
        ("created_ts", Lean.Json.num now.seconds),
        ("already_exists", Lean.Json.bool true)
      ]
      let resp := JsonRpc.Response.success req.id result
      return Response.json (Lean.Json.compress (Lean.toJson resp))
  | none => pure ()

  -- Check to_agent's contact policy
  if toAgent.contactPolicy == .blockAll then
    let err := JsonRpc.Error.invalidParams (some s!"{toAgentName} is not accepting contact requests")
    let resp := JsonRpc.Response.failure req.id err
    return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Insert contact request
  let now ← Chronos.Timestamp.now
  let requestId ← db.insertContactRequest project.id fromAgent.id toAgent.id message now

  let result := Lean.Json.mkObj [
    ("request_id", Lean.Json.num requestId),
    ("status", Lean.Json.str "pending"),
    ("from_agent", Lean.Json.str fromAgentName),
    ("to_agent", Lean.Json.str toAgentName),
    ("created_ts", Lean.Json.num now.seconds)
  ]
  let resp := JsonRpc.Response.success req.id result
  pure (Response.json (Lean.Json.compress (Lean.toJson resp)))

/-- Handle respond_contact request -/
def handleRespondContact (db : Storage.Database) (req : JsonRpc.Request) : IO Response := do
  let params := req.params.getD Lean.Json.null

  -- Extract required params
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

  let requestId ← match params.getObjValAs? Nat "request_id" with
    | Except.ok id => pure id
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: request_id")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  let accept ← match params.getObjValAs? Bool "accept" with
    | Except.ok b => pure b
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: accept (boolean)")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  let _message := match params.getObjValAs? String "message" with
    | Except.ok m => m
    | Except.error _ => ""

  -- Resolve project
  let project ← match ← Identity.resolveProject db projectKey with
    | some p => pure p
    | none =>
      let err := JsonRpc.Error.invalidParams (some s!"project not found: {projectKey}")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Resolve agent
  let agent ← match ← db.queryAgentByName project.id agentName with
    | some a => pure a
    | none =>
      let err := JsonRpc.Error.invalidParams (some s!"agent not found: {agentName}")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Query contact request
  let contactReq ← match ← db.queryContactRequestById requestId with
    | some r => pure r
    | none =>
      let err := JsonRpc.Error.invalidParams (some s!"contact request not found: {requestId}")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Ensure request belongs to this project
  if contactReq.projectId != project.id then
    let err := JsonRpc.Error.invalidParams (some "contact request does not belong to this project")
    let resp := JsonRpc.Response.failure req.id err
    return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Verify agent is the to_agent of the request
  if contactReq.toAgentId != agent.id then
    let err := JsonRpc.Error.invalidParams (some "you can only respond to requests sent to you")
    let resp := JsonRpc.Response.failure req.id err
    return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Verify request is pending
  if contactReq.status != .pending then
    let err := JsonRpc.Error.invalidParams (some s!"contact request already {contactReq.status.toString}")
    let resp := JsonRpc.Response.failure req.id err
    return Response.json (Lean.Json.compress (Lean.toJson resp))

  let now ← Chronos.Timestamp.now
  let newStatus := if accept then ContactRequestStatus.accepted else ContactRequestStatus.rejected

  -- Update request status
  db.updateContactRequestStatus requestId newStatus now

  -- If accepted, create contact relationship
  let contactId ← if accept then do
    match ← db.queryContactBetween project.id contactReq.fromAgentId contactReq.toAgentId with
    | some existing => pure (some existing.id)
    | none =>
      let id ← db.insertContact project.id contactReq.fromAgentId contactReq.toAgentId now
      pure (some id)
  else
    pure none

  let resultFields : List (String × Lean.Json) := [
    ("request_id", Lean.Json.num requestId),
    ("status", Lean.toJson newStatus)
  ] ++ (match contactId with
    | some id => [("contact_id", Lean.Json.num id)]
    | none => [])

  let result := Lean.Json.mkObj resultFields
  let resp := JsonRpc.Response.success req.id result
  pure (Response.json (Lean.Json.compress (Lean.toJson resp)))

/-- Handle list_contacts request -/
def handleListContacts (db : Storage.Database) (req : JsonRpc.Request) : IO Response := do
  let params := req.params.getD Lean.Json.null

  -- Extract required params
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

  -- Resolve project
  let project ← match ← Identity.resolveProject db projectKey with
    | some p => pure p
    | none =>
      let err := JsonRpc.Error.invalidParams (some s!"project not found: {projectKey}")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Resolve agent
  let agent ← match ← db.queryAgentByName project.id agentName with
    | some a => pure a
    | none =>
      let err := JsonRpc.Error.invalidParams (some s!"agent not found: {agentName}")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Query contacts
  let contacts ← db.queryContacts project.id agent.id
  let contactsJson := contacts.map fun c =>
    Lean.Json.mkObj [
      ("agent_name", Lean.Json.str c.agentName),
      ("since_ts", Lean.Json.num c.sinceTs.seconds)
    ]

  let result := Lean.Json.arr contactsJson
  let resp := JsonRpc.Response.success req.id result
  pure (Response.json (Lean.Json.compress (Lean.toJson resp)))

/-- Handle set_contact_policy request -/
def handleSetContactPolicy (db : Storage.Database) (req : JsonRpc.Request) : IO Response := do
  let params := req.params.getD Lean.Json.null

  -- Extract required params
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

  let policyStr ← match params.getObjValAs? String "policy" with
    | Except.ok p => pure p
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: policy")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Parse policy
  let policy ← match ContactPolicy.fromString? policyStr with
    | some p => pure p
    | none =>
      let err := JsonRpc.Error.invalidParams (some s!"invalid policy: {policyStr}. Valid values: open, auto, contacts_only, block_all")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Resolve project
  let project ← match ← Identity.resolveProject db projectKey with
    | some p => pure p
    | none =>
      let err := JsonRpc.Error.invalidParams (some s!"project not found: {projectKey}")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Resolve agent
  let agent ← match ← db.queryAgentByName project.id agentName with
    | some a => pure a
    | none =>
      let err := JsonRpc.Error.invalidParams (some s!"agent not found: {agentName}")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Update policy
  let now ← Chronos.Timestamp.now
  db.updateAgentContactPolicy agent.id policy

  let result := Lean.Json.mkObj [
    ("agent_name", Lean.Json.str agentName),
    ("policy", Lean.toJson policy),
    ("updated_at", Lean.Json.num now.seconds)
  ]
  let resp := JsonRpc.Response.success req.id result
  pure (Response.json (Lean.Json.compress (Lean.toJson resp)))

end AgentMail.Tools.Contacts
