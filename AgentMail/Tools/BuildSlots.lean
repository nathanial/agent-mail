/-
  AgentMail.Tools.BuildSlots - Build slot MCP tool handlers
-/
import Chronos
import Citadel
import AgentMail.Config
import AgentMail.Protocol.JsonRpc
import AgentMail.Storage.Database
import AgentMail.Storage.Archive
import AgentMail.Tools.Identity

open Citadel

namespace AgentMail.Tools.BuildSlots

/-- Handle acquire_build_slot request -/
def handleAcquireBuildSlot (db : Storage.Database) (cfg : Config) (req : JsonRpc.Request) : IO Response := do
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

  let slotName ← match params.getObjValAs? String "slot_name" with
    | Except.ok s => pure s
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: slot_name")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Optional params with defaults
  let ttlSeconds := match params.getObjValAs? Nat "ttl_seconds" with
    | Except.ok t => t
    | Except.error _ => 3600  -- default 1 hour
  let ttlSeconds := if ttlSeconds < 60 then 60 else ttlSeconds

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

  let result ← db.transaction do
    -- Release any expired slots for this name before attempting acquisition.
    let _ ← db.releaseExpiredBuildSlots project.id slotName now

    -- Check for existing active slot
    match ← db.queryActiveBuildSlot project.id slotName now with
    | some existing =>
        if existing.agentId == agent.id then
          -- Same agent already holds this slot
          let acquiredAtIso ← Storage.timestampToIso existing.createdTs
          pure <| Lean.Json.mkObj [
            ("granted", Lean.Json.bool true),
            ("slot_id", Lean.Json.num existing.id),
            ("slot_name", Lean.Json.str slotName),
            ("expires_ts", Lean.Json.num existing.expiresTs.seconds),
            ("acquired_at", Lean.Json.str acquiredAtIso)
          ]
        else
          -- Different agent holds this slot - return conflict
          let holderName ← match ← db.queryAgentById existing.agentId with
            | some a => pure a.name
            | none => pure s!"Agent#{existing.agentId}"
          let retryAfterSeconds := existing.expiresTs.seconds - now.seconds
          pure <| Lean.Json.mkObj [
            ("granted", Lean.Json.bool false),
            ("slot_name", Lean.Json.str slotName),
            ("holder_agent", Lean.Json.str holderName),
            ("expires_ts", Lean.Json.num existing.expiresTs.seconds),
            ("retry_after_seconds", Lean.Json.num retryAfterSeconds)
          ]
    | none =>
        -- Slot is available - attempt to grant it (unique index enforces exclusivity)
        let expiresTs := Chronos.Timestamp.fromSeconds (now.seconds + ttlSeconds)
        let attempt : IO (Option Nat) := try
          let slotId ← db.insertBuildSlot project.id agent.id slotName now expiresTs
          pure (some slotId)
        catch _ =>
          pure none

        match ← attempt with
        | some slotId =>
            let acquiredAtIso ← Storage.timestampToIso now
            let expiresIso ← Storage.timestampToIso expiresTs

            -- Write to archive
            let archive ← Storage.ensureProjectArchive cfg project.slug
            let record := Lean.Json.mkObj [
              ("id", Lean.Json.num slotId),
              ("project", Lean.Json.str project.humanKey),
              ("agent", Lean.Json.str agent.name),
              ("slot_name", Lean.Json.str slotName),
              ("created_ts", Lean.Json.str acquiredAtIso),
              ("expires_ts", Lean.Json.str expiresIso)
            ]
            Storage.writeBuildSlotRecords archive #[record]

            pure <| Lean.Json.mkObj [
              ("granted", Lean.Json.bool true),
              ("slot_id", Lean.Json.num slotId),
              ("slot_name", Lean.Json.str slotName),
              ("expires_ts", Lean.Json.num expiresTs.seconds),
              ("acquired_at", Lean.Json.str acquiredAtIso)
            ]
        | none =>
            -- Another writer won the race; return conflict info if available.
            match ← db.queryActiveBuildSlot project.id slotName now with
            | some existing =>
                let holderName ← match ← db.queryAgentById existing.agentId with
                  | some a => pure a.name
                  | none => pure s!"Agent#{existing.agentId}"
                let retryAfterSeconds := existing.expiresTs.seconds - now.seconds
                pure <| Lean.Json.mkObj [
                  ("granted", Lean.Json.bool false),
                  ("slot_name", Lean.Json.str slotName),
                  ("holder_agent", Lean.Json.str holderName),
                  ("expires_ts", Lean.Json.num existing.expiresTs.seconds),
                  ("retry_after_seconds", Lean.Json.num retryAfterSeconds)
                ]
            | none =>
                -- Fallback: return a conservative "not granted" response.
                pure <| Lean.Json.mkObj [
                  ("granted", Lean.Json.bool false),
                  ("slot_name", Lean.Json.str slotName),
                  ("retry_after_seconds", Lean.Json.num 0)
                ]

  let resp := JsonRpc.Response.success req.id result
  pure (Response.json (Lean.Json.compress (Lean.toJson resp)))

/-- Handle renew_build_slot request -/
def handleRenewBuildSlot (db : Storage.Database) (cfg : Config) (req : JsonRpc.Request) : IO Response := do
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

  let slotName ← match params.getObjValAs? String "slot_name" with
    | Except.ok s => pure s
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: slot_name")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Optional extend_seconds (default 30 min), with min 60s
  let additionalSeconds := match params.getObjValAs? Nat "additional_seconds" with
    | Except.ok s => s
    | Except.error _ => 1800
  let additionalSeconds := if additionalSeconds < 60 then 60 else additionalSeconds

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

  -- Check for existing active slot
  match ← db.queryActiveBuildSlot project.id slotName now with
  | some existing =>
      if existing.agentId != agent.id then
        -- Not the holder
        let err := JsonRpc.Error.invalidParams (some "only slot holder can renew")
        let resp := JsonRpc.Response.failure req.id err
        return Response.json (Lean.Json.compress (Lean.toJson resp))
      -- Extend from max(current_expiry, now)
      let base := if existing.expiresTs.seconds > now.seconds then existing.expiresTs.seconds else now.seconds
      let newExpiresTs := Chronos.Timestamp.fromSeconds (base + additionalSeconds)
      let success ← db.updateBuildSlotExpires existing.id newExpiresTs
      if success then
        let createdIso ← Storage.timestampToIso existing.createdTs
        let expiresIso ← Storage.timestampToIso newExpiresTs

        -- Write to archive
        let archive ← Storage.ensureProjectArchive cfg project.slug
        let record := Lean.Json.mkObj [
          ("id", Lean.Json.num existing.id),
          ("project", Lean.Json.str project.humanKey),
          ("agent", Lean.Json.str agent.name),
          ("slot_name", Lean.Json.str slotName),
          ("created_ts", Lean.Json.str createdIso),
          ("expires_ts", Lean.Json.str expiresIso)
        ]
        Storage.writeBuildSlotRecords archive #[record]

        let result := Lean.Json.mkObj [
          ("renewed", Lean.Json.bool true),
          ("slot_name", Lean.Json.str slotName),
          ("old_expires_ts", Lean.Json.num existing.expiresTs.seconds),
          ("new_expires_ts", Lean.Json.num newExpiresTs.seconds)
        ]
        let resp := JsonRpc.Response.success req.id result
        pure (Response.json (Lean.Json.compress (Lean.toJson resp)))
      else
        let err := JsonRpc.Error.internalError (some "failed to update slot expiration")
        let resp := JsonRpc.Response.failure req.id err
        pure (Response.json (Lean.Json.compress (Lean.toJson resp)))
  | none =>
      -- No active slot to renew
      let err := JsonRpc.Error.invalidParams (some s!"no active slot found: {slotName}")
      let resp := JsonRpc.Response.failure req.id err
      pure (Response.json (Lean.Json.compress (Lean.toJson resp)))

/-- Handle release_build_slot request -/
def handleReleaseBuildSlot (db : Storage.Database) (cfg : Config) (req : JsonRpc.Request) : IO Response := do
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

  let slotName ← match params.getObjValAs? String "slot_name" with
    | Except.ok s => pure s
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: slot_name")
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

  let now ← Chronos.Timestamp.now

  -- Check for existing active slot
  match ← db.queryActiveBuildSlot project.id slotName now with
  | some existing =>
      if existing.agentId != agent.id then
        -- Not the holder
        let err := JsonRpc.Error.invalidParams (some "only slot holder can release")
        let resp := JsonRpc.Response.failure req.id err
        return Response.json (Lean.Json.compress (Lean.toJson resp))
      let success ← db.updateBuildSlotReleased existing.id now
      if success then
        let createdIso ← Storage.timestampToIso existing.createdTs
        let releasedIso ← Storage.timestampToIso now
        let expiresBase := if existing.expiresTs.seconds > now.seconds then now else existing.expiresTs
        let expiresIso ← Storage.timestampToIso expiresBase

        -- Write to archive
        let archive ← Storage.ensureProjectArchive cfg project.slug
        let record := Lean.Json.mkObj [
          ("id", Lean.Json.num existing.id),
          ("project", Lean.Json.str project.humanKey),
          ("agent", Lean.Json.str agent.name),
          ("slot_name", Lean.Json.str slotName),
          ("created_ts", Lean.Json.str createdIso),
          ("expires_ts", Lean.Json.str expiresIso),
          ("released_ts", Lean.Json.str releasedIso)
        ]
        Storage.writeBuildSlotRecords archive #[record]

        let result := Lean.Json.mkObj [
          ("released", Lean.Json.bool true),
          ("slot_name", Lean.Json.str slotName),
          ("released_at", Lean.Json.str releasedIso)
        ]
        let resp := JsonRpc.Response.success req.id result
        pure (Response.json (Lean.Json.compress (Lean.toJson resp)))
      else
        let err := JsonRpc.Error.internalError (some "failed to release slot")
        let resp := JsonRpc.Response.failure req.id err
        pure (Response.json (Lean.Json.compress (Lean.toJson resp)))
  | none =>
      -- No active slot to release - still return success (idempotent)
      let releasedIso ← Storage.timestampToIso now
      let result := Lean.Json.mkObj [
        ("released", Lean.Json.bool true),
        ("slot_name", Lean.Json.str slotName),
        ("released_at", Lean.Json.str releasedIso)
      ]
      let resp := JsonRpc.Response.success req.id result
      pure (Response.json (Lean.Json.compress (Lean.toJson resp)))

end AgentMail.Tools.BuildSlots
