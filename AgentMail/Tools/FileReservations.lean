/-
  AgentMail.Tools.FileReservations - File reservation MCP tool handlers
-/
import Chronos
import Citadel
import Rune
import AgentMail.Config
import AgentMail.Protocol.JsonRpc
import AgentMail.Storage.Database
import AgentMail.Storage.Archive
import AgentMail.Tools.Identity

open Citadel

namespace AgentMail.Tools.FileReservations

/-- Normalize reservation patterns/paths (match reference: forward slashes + no leading slash). -/
private def normalizePattern (s : String) : String :=
  s.replace "\\" "/" |>.dropWhile (· == '/')

/-- Check if a pattern contains any glob markers. -/
private def containsGlob (s : String) : Bool :=
  s.any (fun c => c == '*' || c == '?' || c == '[')

private def escapeRegexChar (c : Char) : String :=
  if c == '.' || c == '+' || c == '(' || c == ')' || c == '{' || c == '}' || c == '|' || c == '^' || c == '$' || c == '\\' then
    "\\" ++ String.singleton c
  else
    String.singleton c

private def escapeClassChar (c : Char) : String :=
  if c == '\\' then "\\\\"
  else String.singleton c

private partial def takeClass (chars : List Char) : Option (List Char × List Char) :=
  match chars with
  | [] => none
  | ']' :: rest => some ([], rest)
  | c :: rest =>
      match takeClass rest with
      | some (cls, rem) => some (c :: cls, rem)
      | none => none

private def renderClass (chars : List Char) : String :=
  match chars with
  | [] => ""
  | '!' :: rest => "^" ++ (String.intercalate "" (rest.map escapeClassChar))
  | '^' :: rest => "^" ++ (String.intercalate "" (rest.map escapeClassChar))
  | _ => String.intercalate "" (chars.map escapeClassChar)

private partial def globToRegexAux (chars : List Char) : String :=
  match chars with
  | [] => ""
  | '*' :: rest => ".*" ++ globToRegexAux rest
  | '?' :: rest => "." ++ globToRegexAux rest
  | '[' :: rest =>
      match takeClass rest with
      | some (cls, rem) =>
          if cls.isEmpty then "\\[" ++ globToRegexAux rem
          else "[" ++ renderClass cls ++ "]" ++ globToRegexAux rem
      | none => "\\[" ++ globToRegexAux rest
  | c :: rest => escapeRegexChar c ++ globToRegexAux rest

private def globToRegex (pattern : String) : String :=
  "^" ++ globToRegexAux pattern.toList ++ "$"

private def globMatches (pattern input : String) : Bool :=
  let regex := globToRegex pattern
  match Rune.Regex.compile regex with
  | .ok re => (re.isMatch input).isSome
  | .error _ => false

private def patternMatchesPath (pattern path : String) : Bool :=
  let p := normalizePattern pattern
  let s := normalizePattern path
  if containsGlob p then
    globMatches p s
  else if p.endsWith "/" then
    s.startsWith p
  else
    s == p

/-- Check if two glob patterns potentially overlap using symmetric glob matching. -/
def patternsOverlap (p1 p2 : String) : Bool :=
  let a := normalizePattern p1
  let b := normalizePattern p2
  if a == b then true
  else patternMatchesPath a b || patternMatchesPath b a

private def fileReservationConflicts (existing : AgentMail.FileReservation) (candidatePath : String)
    (candidateExclusive : Bool) (candidateAgentId : Nat) : Bool :=
  if existing.releasedTs.isSome then
    false
  else if existing.agentId == candidateAgentId then
    false
  else if !existing.exclusive && !candidateExclusive then
    false
  else
    patternsOverlap existing.pathPattern candidatePath

/-- Handle file_reservation_paths request -/
def handleFileReservationPaths (db : Storage.Database) (cfg : Config) (req : JsonRpc.Request) : IO Response := do
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

  let reason := match params.getObjValAs? String "reason" with
    | Except.ok r => r
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

  let now ← Chronos.Timestamp.now
  let expiresTs := Chronos.Timestamp.fromSeconds (now.seconds + ttlSeconds)

  -- Query all active reservations for this project
  let activeReservations ← db.queryActiveFileReservations project.id now

  -- Process each requested path (advisory: always grant, report conflicts)
  let mut granted : Array Lean.Json := #[]
  let mut conflicts : Array Lean.Json := #[]
  let mut archiveRecords : Array Lean.Json := #[]

  for path in paths do
    let mut holders : Array Lean.Json := #[]
    for existing in activeReservations do
      if fileReservationConflicts existing path exclusive agent.id then
        let conflictAgentName ← match ← db.queryAgentById existing.agentId with
          | some a => pure a.name
          | none => pure s!"Agent#{existing.agentId}"
        let existingExpiresIso ← Storage.timestampToIso existing.expiresTs
        holders := holders.push (Lean.Json.mkObj [
          ("agent", Lean.Json.str conflictAgentName),
          ("path_pattern", Lean.Json.str existing.pathPattern),
          ("exclusive", Lean.Json.bool existing.exclusive),
          ("expires_ts", Lean.Json.str existingExpiresIso)
        ])
    if holders.size > 0 then
      conflicts := conflicts.push (Lean.Json.mkObj [
        ("path", Lean.Json.str path),
        ("holders", Lean.Json.arr holders)
      ])

    let reservationId ← db.insertFileReservation project.id agent.id path exclusive reason now expiresTs
    let createdIso ← Storage.timestampToIso now
    let expiresIso ← Storage.timestampToIso expiresTs
    granted := granted.push (Lean.Json.mkObj [
      ("id", Lean.Json.num reservationId),
      ("path_pattern", Lean.Json.str path),
      ("exclusive", Lean.Json.bool exclusive),
      ("reason", Lean.Json.str reason),
      ("expires_ts", Lean.Json.str expiresIso)
    ])
    archiveRecords := archiveRecords.push (Lean.Json.mkObj [
      ("id", Lean.Json.num reservationId),
      ("project", Lean.Json.str project.humanKey),
      ("agent", Lean.Json.str agent.name),
      ("path_pattern", Lean.Json.str path),
      ("exclusive", Lean.Json.bool exclusive),
      ("reason", Lean.Json.str reason),
      ("created_ts", Lean.Json.str createdIso),
      ("expires_ts", Lean.Json.str expiresIso)
    ])

  if !archiveRecords.isEmpty then
    let archive ← Storage.ensureProjectArchive cfg project.slug
    Storage.writeFileReservationRecords archive archiveRecords

  let result := Lean.Json.mkObj [
    ("granted", Lean.Json.arr granted),
    ("conflicts", Lean.Json.arr conflicts)
  ]
  let resp := JsonRpc.Response.success req.id result
  pure (Response.json (Lean.Json.compress (Lean.toJson resp)))

/-- Handle release_file_reservations request -/
def handleReleaseFileReservations (db : Storage.Database) (cfg : Config) (req : JsonRpc.Request) : IO Response := do
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

  -- Optional params: paths or file_reservation_ids
  let pathsOpt := match params.getObjValAs? (Array String) "paths" with
    | Except.ok p => some p
    | Except.error _ => none

  let idsOpt := match params.getObjValAs? (Array Nat) "file_reservation_ids" with
    | Except.ok ids => some ids
    | Except.error _ =>
      match params.getObjValAs? (Array Nat) "reservation_ids" with
        | Except.ok ids => some ids
        | Except.error _ => none

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

  let now ← Chronos.Timestamp.now
  let mut releasedCount : Nat := 0
  let mut releasedRecords : Array Lean.Json := #[]
  let agentReservations ← db.queryFileReservationsByAgent project.id agent.id
  for res in agentReservations do
    if res.releasedTs.isSome then
      continue
    if let some ids := idsOpt then
      if !(ids.any (fun id => id == res.id)) then
        continue
    if let some paths := pathsOpt then
      if !(paths.any (fun p => p == res.pathPattern)) then
        continue
    let success ← db.updateFileReservationReleased res.id now
    if success then
      releasedCount := releasedCount + 1
      let createdIso ← Storage.timestampToIso res.createdTs
      let releaseIso ← Storage.timestampToIso now
      let expiresBase := if res.expiresTs.seconds > now.seconds then now else res.expiresTs
      let expiresIso ← Storage.timestampToIso expiresBase
      releasedRecords := releasedRecords.push (Lean.Json.mkObj [
        ("id", Lean.Json.num res.id),
        ("project", Lean.Json.str project.humanKey),
        ("agent", Lean.Json.str agent.name),
        ("path_pattern", Lean.Json.str res.pathPattern),
        ("exclusive", Lean.Json.bool res.exclusive),
        ("reason", Lean.Json.str res.reason),
        ("created_ts", Lean.Json.str createdIso),
        ("expires_ts", Lean.Json.str expiresIso),
        ("released_ts", Lean.Json.str releaseIso)
      ])

  if !releasedRecords.isEmpty then
    let archive ← Storage.ensureProjectArchive cfg project.slug
    Storage.writeFileReservationRecords archive releasedRecords

  let releasedAtIso ← Storage.timestampToIso now
  let result := Lean.Json.mkObj [
    ("released", Lean.Json.num releasedCount),
    ("released_at", Lean.Json.str releasedAtIso)
  ]
  let resp := JsonRpc.Response.success req.id result
  pure (Response.json (Lean.Json.compress (Lean.toJson resp)))

/-- Handle renew_file_reservations request -/
def handleRenewFileReservations (db : Storage.Database) (cfg : Config) (req : JsonRpc.Request) : IO Response := do
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

  let idsOpt := match params.getObjValAs? (Array Nat) "file_reservation_ids" with
    | Except.ok ids => some ids
    | Except.error _ =>
      match params.getObjValAs? (Array Nat) "reservation_ids" with
        | Except.ok ids => some ids
        | Except.error _ => none

  let pathsOpt := match params.getObjValAs? (Array String) "paths" with
    | Except.ok p => some p
    | Except.error _ => none

  -- Optional extend_seconds (default 30 min), with min 60s
  let extendSeconds := match params.getObjValAs? Nat "extend_seconds" with
    | Except.ok s => s
    | Except.error _ =>
      match params.getObjValAs? Nat "additional_seconds" with
        | Except.ok s => s
        | Except.error _ => 1800
  let extendSeconds := if extendSeconds < 60 then 60 else extendSeconds

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

  let now ← Chronos.Timestamp.now
  let mut updated : Array Lean.Json := #[]
  let mut archiveRecords : Array Lean.Json := #[]

  let agentReservations ← db.queryFileReservationsByAgent project.id agent.id
  for res in agentReservations do
    if res.releasedTs.isSome then
      continue
    if let some ids := idsOpt then
      if !(ids.any (fun id => id == res.id)) then
        continue
    if let some paths := pathsOpt then
      if !(paths.any (fun p => p == res.pathPattern)) then
        continue
    let base := if res.expiresTs.seconds > now.seconds then res.expiresTs.seconds else now.seconds
    let newExpiresTs := Chronos.Timestamp.fromSeconds (base + extendSeconds)
    let success ← db.updateFileReservationExpires res.id newExpiresTs
    if success then
      let oldIso ← Storage.timestampToIso res.expiresTs
      let newIso ← Storage.timestampToIso newExpiresTs
      updated := updated.push (Lean.Json.mkObj [
        ("id", Lean.Json.num res.id),
        ("path_pattern", Lean.Json.str res.pathPattern),
        ("old_expires_ts", Lean.Json.str oldIso),
        ("new_expires_ts", Lean.Json.str newIso)
      ])
      let createdIso ← Storage.timestampToIso res.createdTs
      let expiresIso ← Storage.timestampToIso newExpiresTs
      archiveRecords := archiveRecords.push (Lean.Json.mkObj [
        ("id", Lean.Json.num res.id),
        ("project", Lean.Json.str project.humanKey),
        ("agent", Lean.Json.str agent.name),
        ("path_pattern", Lean.Json.str res.pathPattern),
        ("exclusive", Lean.Json.bool res.exclusive),
        ("reason", Lean.Json.str res.reason),
        ("created_ts", Lean.Json.str createdIso),
        ("expires_ts", Lean.Json.str expiresIso)
      ])

  if !archiveRecords.isEmpty then
    let archive ← Storage.ensureProjectArchive cfg project.slug
    Storage.writeFileReservationRecords archive archiveRecords

  let result := Lean.Json.mkObj [
    ("renewed", Lean.Json.num updated.size),
    ("file_reservations", Lean.Json.arr updated)
  ]
  let resp := JsonRpc.Response.success req.id result
  pure (Response.json (Lean.Json.compress (Lean.toJson resp)))

/-- Handle force_release_file_reservation request -/
def handleForceReleaseFileReservation (db : Storage.Database) (cfg : Config) (req : JsonRpc.Request) : IO Response := do
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

  let reservationId ← match params.getObjValAs? Nat "file_reservation_id" with
    | Except.ok id => pure id
    | Except.error _ =>
      match params.getObjValAs? Nat "reservation_id" with
        | Except.ok id => pure id
        | Except.error _ =>
          let err := JsonRpc.Error.invalidParams (some "missing required param: file_reservation_id")
          let resp := JsonRpc.Response.failure req.id err
          return Response.json (Lean.Json.compress (Lean.toJson resp))

  let _note := match params.getObjValAs? String "note" with
    | Except.ok n => n
    | Except.error _ =>
      match params.getObjValAs? String "reason" with
        | Except.ok r => r
        | Except.error _ => ""

  let _notifyPrevious := match params.getObjValAs? Bool "notify_previous" with
    | Except.ok b => b
    | Except.error _ => true

  -- Resolve project
  let project ← match ← Identity.resolveProject db projectKey with
    | some p => pure p
    | none =>
      let err := JsonRpc.Error.invalidParams (some s!"project not found: {projectKey}")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Resolve agent (actor)
  let _actor ← match ← db.queryAgentByName project.id agentName with
    | some a => pure a
    | none =>
      let err := JsonRpc.Error.invalidParams (some s!"agent not found: {agentName}")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Query reservation and verify it belongs to project
  let reservation ← match ← db.queryFileReservationById reservationId with
    | some r => pure r
    | none =>
      let err := JsonRpc.Error.invalidParams (some s!"reservation not found: {reservationId}")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  if reservation.projectId != project.id then
    let err := JsonRpc.Error.invalidParams (some "reservation does not belong to this project")
    let resp := JsonRpc.Response.failure req.id err
    return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Force release (regardless of owner)
  let releaseTs ← Chronos.Timestamp.now
  let released ←
    if reservation.releasedTs.isSome then
      pure false
    else
      db.updateFileReservationReleased reservationId releaseTs
  let releasedAt := match reservation.releasedTs with
    | some ts => ts
    | none => releaseTs

  let holderName ← match ← db.queryAgentById reservation.agentId with
    | some a => pure a.name
    | none => pure s!"Agent#{reservation.agentId}"

  if released then
    let createdIso ← Storage.timestampToIso reservation.createdTs
    let releaseIso ← Storage.timestampToIso releaseTs
    let expiresBase := if reservation.expiresTs.seconds > releaseTs.seconds then releaseTs else reservation.expiresTs
    let expiresIso ← Storage.timestampToIso expiresBase
    let archive ← Storage.ensureProjectArchive cfg project.slug
    let record := Lean.Json.mkObj [
      ("id", Lean.Json.num reservation.id),
      ("project", Lean.Json.str project.humanKey),
      ("agent", Lean.Json.str holderName),
      ("path_pattern", Lean.Json.str reservation.pathPattern),
      ("exclusive", Lean.Json.bool reservation.exclusive),
      ("reason", Lean.Json.str reservation.reason),
      ("created_ts", Lean.Json.str createdIso),
      ("expires_ts", Lean.Json.str expiresIso),
      ("released_ts", Lean.Json.str releaseIso)
    ]
    Storage.writeFileReservationRecords archive #[record]

  let createdIso ← Storage.timestampToIso reservation.createdTs
  let expiresIso ← Storage.timestampToIso reservation.expiresTs
  let releasedIso ← Storage.timestampToIso releasedAt
  let reservationJson := Lean.Json.mkObj [
    ("id", Lean.Json.num reservation.id),
    ("agent", Lean.Json.str holderName),
    ("path_pattern", Lean.Json.str reservation.pathPattern),
    ("exclusive", Lean.Json.bool reservation.exclusive),
    ("reason", Lean.Json.str reservation.reason),
    ("created_ts", Lean.Json.str createdIso),
    ("expires_ts", Lean.Json.str expiresIso),
    ("released_ts", match reservation.releasedTs with
      | some _ => Lean.Json.str releasedIso
      | none => Lean.Json.str releasedIso),
    ("stale_reasons", Lean.Json.arr #[]),
    ("last_agent_activity_ts", Lean.Json.null),
    ("last_mail_activity_ts", Lean.Json.null),
    ("last_filesystem_activity_ts", Lean.Json.null),
    ("last_git_activity_ts", Lean.Json.null),
    ("notified", Lean.Json.bool false)
  ]

  let result := Lean.Json.mkObj [
    ("released", Lean.Json.num (if released then 1 else 0)),
    ("released_at", Lean.Json.str releasedIso),
    ("reservation", reservationJson)
  ]
  let resp := JsonRpc.Response.success req.id result
  pure (Response.json (Lean.Json.compress (Lean.toJson resp)))

end AgentMail.Tools.FileReservations
