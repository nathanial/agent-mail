/-
  AgentMail.Tools.Macros - Compound operation macro handlers
-/
import Chronos
import Citadel
import AgentMail.Config
import AgentMail.Protocol.JsonRpc
import AgentMail.Storage.Database
import AgentMail.Storage.Archive
import AgentMail.Tools.Identity
import AgentMail.Tools.FileReservations
import AgentMail.Utils.NameGenerator

open Citadel

namespace AgentMail.Tools.Macros

/-- Handle macro_start_session request
    Combines: ensure_project + generate_name + register_agent
-/
def handleMacroStartSession (db : Storage.Database) (cfg : Config) (req : JsonRpc.Request) : IO Response := do
  let params := req.params.getD Lean.Json.null

  -- Extract required params
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
  let taskDescription := match params.getObjValAs? String "task_description" with
    | Except.ok t => t
    | Except.error _ => ""

  -- Single timestamp for consistency
  let now ← Chronos.Timestamp.now

  -- Step 1: Ensure project exists (auto-create if needed)
  let project ← match ← Identity.resolveProject db projectKey with
    | some p => pure p
    | none =>
      let slug ← Identity.generateUniqueSlug db projectKey
      let id ← db.insertProject slug projectKey now
      let p : Project := { id, slug, humanKey := projectKey, createdAt := now }
      let _ ← Storage.ensureProjectArchive cfg p.slug
      pure p

  -- Step 2: Generate unique agent name
  let mut attempts := 0
  let mut chosen : Option String := none
  while attempts < 128 && chosen.isNone do
    let candidate ← Utils.NameGenerator.generateName
    match ← db.queryAgentByName project.id candidate with
    | none => chosen := some candidate
    | some _ => attempts := attempts + 1
  let agentName ← match chosen with
    | some n => pure n
    | none =>
      let err := JsonRpc.Error.internalError (some "unable to generate unique agent name")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Step 3: Create agent
  let agent : Agent := {
    id := 0
    projectId := project.id
    name := agentName
    program := program
    model := model
    taskDescription := taskDescription
    contactPolicy := .auto
    attachmentsPolicy := .auto
    inceptionTs := now
    lastActiveTs := now
  }
  let agentId ← db.insertAgent agent
  let newAgent := { agent with id := agentId }

  -- Step 4: Write agent profile to archive
  let archive ← Storage.ensureProjectArchive cfg project.slug
  Storage.writeAgentProfile archive newAgent

  -- Generate session ID
  let sessionId := s!"session-{now.seconds}-{agentId}"
  let registeredAtIso ← Storage.timestampToIso now

  -- Build response
  let result := Lean.Json.mkObj [
    ("agent_name", Lean.Json.str agentName),
    ("project", Lean.Json.str project.humanKey),
    ("project_slug", Lean.Json.str project.slug),
    ("registered_at", Lean.Json.str registeredAtIso),
    ("session_id", Lean.Json.str sessionId)
  ]
  let resp := JsonRpc.Response.success req.id result
  pure (Response.json (Lean.Json.compress (Lean.toJson resp)))

/-- Handle macro_prepare_thread request
    Combines: fetch thread messages + mark all as read
-/
def handleMacroPrepareThread (db : Storage.Database) (_cfg : Config) (req : JsonRpc.Request) : IO Response := do
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

  let threadId ← match params.getObjValAs? String "thread_id" with
    | Except.ok t => pure t
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: thread_id")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Single timestamp for consistency
  let now ← Chronos.Timestamp.now

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

  -- Query all messages in thread (up to 1000)
  let messages ← db.queryMessagesByThread project.id threadId 1000

  -- Mark messages as read where agent is recipient
  let mut messagesMarkedRead : Nat := 0
  for msg in messages do
    let status ← db.queryRecipientStatus msg.id agent.id
    match status with
    | some recipientStatus =>
      if recipientStatus.readAt.isNone then
        let updated ← db.updateMessageReadAt msg.id agent.id now
        if updated then
          messagesMarkedRead := messagesMarkedRead + 1
    | none => pure ()

  -- Convert messages to JSON
  let messagesJson := messages.map fun msg =>
    Lean.Json.mkObj [
      ("id", Lean.Json.num msg.id),
      ("from", Lean.Json.str msg.senderName),
      ("subject", Lean.Json.str msg.subject),
      ("body_md", Lean.Json.str msg.bodyMd),
      ("importance", Lean.toJson msg.importance),
      ("created_ts", Lean.Json.num msg.createdTs.seconds)
    ]

  -- Build response
  let result := Lean.Json.mkObj [
    ("thread_id", Lean.Json.str threadId),
    ("messages", Lean.Json.arr messagesJson),
    ("total_messages", Lean.Json.num messages.size),
    ("messages_marked_read", Lean.Json.num messagesMarkedRead)
  ]
  let resp := JsonRpc.Response.success req.id result
  pure (Response.json (Lean.Json.compress (Lean.toJson resp)))

/-- Handle macro_file_reservation_cycle request
    Combines: reserve files + report conflicts transparently
-/
def handleMacroFileReservationCycle (db : Storage.Database) (cfg : Config) (req : JsonRpc.Request) : IO Response := do
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

  let paths ← match params.getObjValAs? (Array String) "paths" with
    | Except.ok p => pure p
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: paths (array of strings)")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Optional params with defaults
  let ttlSeconds := match params.getObjValAs? Nat "ttl_seconds" with
    | Except.ok t => t
    | Except.error _ => 3600  -- default 1 hour
  let ttlSeconds := if ttlSeconds < 60 then 60 else ttlSeconds

  let exclusive := match params.getObjValAs? Bool "exclusive" with
    | Except.ok e => e
    | Except.error _ => true  -- default exclusive

  -- Single timestamp for consistency
  let now ← Chronos.Timestamp.now
  let expiresTs := Chronos.Timestamp.fromSeconds (now.seconds + ttlSeconds)

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

  -- Query all active reservations for this project
  let activeReservations ← db.queryActiveFileReservations project.id now

  -- Process each requested path (advisory: always grant, report conflicts)
  let mut reservations : Array Lean.Json := #[]
  let mut conflicts : Array Lean.Json := #[]
  let mut archiveRecords : Array Lean.Json := #[]

  for path in paths do
    -- Check for conflicts
    for existing in activeReservations do
      if FileReservations.patternsOverlap existing.pathPattern path then
        if existing.agentId != agent.id then
          if existing.exclusive || exclusive then
            let conflictAgentName ← match ← db.queryAgentById existing.agentId with
              | some a => pure a.name
              | none => pure s!"Agent#{existing.agentId}"
            conflicts := conflicts.push (Lean.Json.mkObj [
              ("path_pattern", Lean.Json.str existing.pathPattern),
              ("held_by", Lean.Json.str conflictAgentName),
              ("expires_ts", Lean.Json.num existing.expiresTs.seconds)
            ])

    -- Always grant the reservation (advisory)
    let reservationId ← db.insertFileReservation project.id agent.id path exclusive "" now expiresTs
    reservations := reservations.push (Lean.Json.mkObj [
      ("id", Lean.Json.num reservationId),
      ("path_pattern", Lean.Json.str path),
      ("expires_ts", Lean.Json.num expiresTs.seconds),
      ("exclusive", Lean.Json.bool exclusive)
    ])

    -- Prepare archive record
    let createdIso ← Storage.timestampToIso now
    let expiresIso ← Storage.timestampToIso expiresTs
    archiveRecords := archiveRecords.push (Lean.Json.mkObj [
      ("id", Lean.Json.num reservationId),
      ("project", Lean.Json.str project.humanKey),
      ("agent", Lean.Json.str agent.name),
      ("path_pattern", Lean.Json.str path),
      ("exclusive", Lean.Json.bool exclusive),
      ("reason", Lean.Json.str ""),
      ("created_ts", Lean.Json.str createdIso),
      ("expires_ts", Lean.Json.str expiresIso)
    ])

  -- Write archive records
  if !archiveRecords.isEmpty then
    let archive ← Storage.ensureProjectArchive cfg project.slug
    Storage.writeFileReservationRecords archive archiveRecords

  -- Build response
  let result := Lean.Json.mkObj [
    ("reservations", Lean.Json.arr reservations),
    ("conflicts", Lean.Json.arr conflicts),
    ("granted_count", Lean.Json.num reservations.size),
    ("conflict_count", Lean.Json.num conflicts.size)
  ]
  let resp := JsonRpc.Response.success req.id result
  pure (Response.json (Lean.Json.compress (Lean.toJson resp)))

/-- Handle macro_contact_handshake request
    Combines: request_contact + auto-accept for instant bidirectional contact
-/
def handleMacroContactHandshake (db : Storage.Database) (_cfg : Config) (req : JsonRpc.Request) : IO Response := do
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

  -- Single timestamp for consistency
  let now ← Chronos.Timestamp.now

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
    let err := JsonRpc.Error.invalidParams (some "cannot establish contact with yourself")
    let resp := JsonRpc.Response.failure req.id err
    return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Check if contact already exists
  match ← db.queryContactBetween project.id fromAgent.id toAgent.id with
  | some existing =>
    -- Contact already exists, return success with was_existing flag
    let establishedAtIso ← Storage.timestampToIso existing.createdTs
    let result := Lean.Json.mkObj [
      ("contact_established", Lean.Json.bool true),
      ("from_agent", Lean.Json.str fromAgentName),
      ("to_agent", Lean.Json.str toAgentName),
      ("established_at", Lean.Json.str establishedAtIso),
      ("was_existing", Lean.Json.bool true)
    ]
    let resp := JsonRpc.Response.success req.id result
    return Response.json (Lean.Json.compress (Lean.toJson resp))
  | none => pure ()

  -- Check for existing contact request in either direction and handle it
  let existingRequestForward ← db.queryContactRequestBetween project.id fromAgent.id toAgent.id
  let existingRequest ← match existingRequestForward with
    | some req => pure (some req)
    | none => db.queryContactRequestBetween project.id toAgent.id fromAgent.id

  match existingRequest with
  | some contactReq =>
    if contactReq.status == .pending then
      -- Accept the pending request
      db.updateContactRequestStatus contactReq.id .accepted now
      let _ ← db.insertContact project.id fromAgent.id toAgent.id now
    else
      -- Respect block_all when refreshing a non-pending request
      if toAgent.contactPolicy == .blockAll then
        let err := JsonRpc.Error.invalidParams (some s!"{toAgentName} is not accepting contact requests")
        let resp := JsonRpc.Response.failure req.id err
        return Response.json (Lean.Json.compress (Lean.toJson resp))
      -- Reset to pending then accept
      db.resetContactRequestToPending contactReq.id message now
      db.updateContactRequestStatus contactReq.id .accepted now
      let _ ← db.insertContact project.id fromAgent.id toAgent.id now
  | none =>
    -- Enforce contact policy for new requests
    if toAgent.contactPolicy == .blockAll then
      let err := JsonRpc.Error.invalidParams (some s!"{toAgentName} is not accepting contact requests")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))
    -- Create new contact request and immediately accept it
    let requestId ← db.insertContactRequest project.id fromAgent.id toAgent.id message now
    db.updateContactRequestStatus requestId .accepted now
    let _ ← db.insertContact project.id fromAgent.id toAgent.id now

  let establishedAtIso ← Storage.timestampToIso now
  let result := Lean.Json.mkObj [
    ("contact_established", Lean.Json.bool true),
    ("from_agent", Lean.Json.str fromAgentName),
    ("to_agent", Lean.Json.str toAgentName),
    ("established_at", Lean.Json.str establishedAtIso),
    ("was_existing", Lean.Json.bool false)
  ]
  let resp := JsonRpc.Response.success req.id result
  pure (Response.json (Lean.Json.compress (Lean.toJson resp)))

end AgentMail.Tools.Macros
