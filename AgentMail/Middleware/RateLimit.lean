/-
  AgentMail.Middleware.RateLimit - Token bucket rate limiting middleware
-/
import Citadel
import Std.Data.HashMap

open Citadel

namespace AgentMail.Middleware.RateLimit

/-- Rate limit configuration -/
structure RateLimitConfig where
  /-- Whether rate limiting is enabled -/
  enabled : Bool := false
  /-- Maximum tool calls per minute -/
  toolsPerMinute : Nat := 60
  /-- Maximum resource requests per minute -/
  resourcesPerMinute : Nat := 120
  /-- Tool call burst limit -/
  toolsBurst : Nat := 10
  /-- Resource request burst limit -/
  resourcesBurst : Nat := 20
  deriving Repr, Inhabited

/-- Token bucket entry: (available tokens, last update timestamp in ms) -/
abbrev BucketEntry := Float × Nat

/-- Rate limit state with in-memory token buckets -/
structure RateLimitState where
  /-- Map from client key to bucket entry -/
  buckets : IO.Ref (Std.HashMap String BucketEntry)

namespace RateLimitState

/-- Create new rate limit state -/
def create : IO RateLimitState := do
  let buckets ← IO.mkRef ({} : Std.HashMap String BucketEntry)
  pure { buckets }

/-- Get client key from request (uses client IP or falls back to "default") -/
private def getClientKey (req : ServerRequest) : String :=
  -- Try X-Forwarded-For first, then X-Real-IP, then Host
  match req.header "X-Forwarded-For" with
  | some xff => (xff.splitOn ",").head?.getD "default" |>.trim
  | none =>
    match req.header "X-Real-IP" with
    | some ip => ip
    | none =>
      match req.header "Host" with
      | some host => host
      | none => "default"

/-- Determine the endpoint type for rate limiting -/
private def getEndpointType (req : ServerRequest) : String :=
  if req.path == "/rpc" then
    "tools"
  else if req.path.startsWith "/resource" then
    "resources"
  else
    "other"

/-- Consume a token from the bucket.
    Returns true if the request should be allowed, false if rate limited. -/
def consumeToken (state : RateLimitState) (key : String) (perMinute burst : Nat) : IO Bool := do
  let now ← IO.monoMsNow
  let rate := Float.ofNat perMinute / 60000.0  -- tokens per millisecond
  let buckets ← state.buckets.get

  match buckets.get? key with
  | none =>
    -- New bucket, start with burst-1 tokens (we're consuming one)
    let newTokens := Float.ofNat burst - 1.0
    state.buckets.set (buckets.insert key (newTokens, now))
    pure true
  | some (tokens, lastTs) =>
    -- Calculate tokens added since last request
    let elapsed := now - lastTs
    let addedTokens := Float.ofNat elapsed * rate
    let newTokens := min (Float.ofNat burst) (tokens + addedTokens)

    if newTokens >= 1.0 then
      -- Allow request and consume a token
      state.buckets.set (buckets.insert key (newTokens - 1.0, now))
      pure true
    else
      -- Rate limited, update timestamp but don't consume
      state.buckets.set (buckets.insert key (newTokens, now))
      pure false

/-- Clean up stale buckets (older than 5 minutes) -/
def cleanup (state : RateLimitState) : IO Unit := do
  let now ← IO.monoMsNow
  let staleThreshold := now - 300000  -- 5 minutes in ms
  state.buckets.modify fun buckets =>
    buckets.fold (init := ({} : Std.HashMap String BucketEntry)) fun acc key (tokens, lastTs) =>
      if lastTs > staleThreshold then
        acc.insert key (tokens, lastTs)
      else
        acc

end RateLimitState

/-- Rate limiting middleware.

    Uses token bucket algorithm to limit request rates.
    Different limits apply to /rpc (tools) and /resource/* (resources) endpoints.

    - Returns 429 Too Many Requests when rate limited
    - Includes Retry-After header suggesting when to retry
-/
def rateLimit (state : RateLimitState) (config : RateLimitConfig) : Citadel.Middleware :=
  fun handler req => do
    -- Skip if disabled
    if !config.enabled then
      return ← handler req

    -- Skip health checks
    if req.path == "/health" || req.path.startsWith "/health/" then
      return ← handler req

    -- Determine rate limit parameters based on endpoint
    let (perMinute, burst) := match RateLimitState.getEndpointType req with
      | "tools" => (config.toolsPerMinute, config.toolsBurst)
      | "resources" => (config.resourcesPerMinute, config.resourcesBurst)
      | _ => (config.resourcesPerMinute, config.resourcesBurst)

    -- Get client key and check rate limit
    let clientKey := RateLimitState.getClientKey req
    let bucketKey := s!"{clientKey}:{RateLimitState.getEndpointType req}"

    let allowed ← state.consumeToken bucketKey perMinute burst

    if allowed then
      handler req
    else
      -- Calculate suggested retry time (when 1 token will be available)
      let retryAfterSecs := 60 / perMinute
      pure (Response.tooManyRequests (some retryAfterSecs))

end AgentMail.Middleware.RateLimit
