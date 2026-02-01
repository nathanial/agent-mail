/-
  AgentMail.Middleware.Auth - Bearer token authentication middleware
-/
import Citadel

open Citadel

namespace AgentMail.Middleware.Auth

/-- Check if a request is from localhost -/
private def isLocalhost (req : ServerRequest) : Bool :=
  -- Check common localhost indicators from the Host header
  match req.header "Host" with
  | some host =>
    host.startsWith "localhost" ||
    host.startsWith "127.0.0.1" ||
    host.startsWith "[::1]"
  | none => false

/-- Bearer token authentication middleware.

    Validates requests have a valid `Authorization: Bearer <token>` header.

    - Skips authentication for OPTIONS requests (CORS preflight)
    - Skips authentication for /health endpoints
    - Optionally allows localhost requests without authentication
    - Returns 401 Unauthorized for missing or invalid tokens
-/
def bearerAuth (token : String) (allowLocalhost : Bool := true) : Citadel.Middleware :=
  fun handler req => do
    -- Skip OPTIONS (CORS preflight)
    if req.method == .OPTIONS then
      return ← handler req

    -- Skip health endpoints
    if req.path == "/health" || req.path.startsWith "/health/" then
      return ← handler req

    -- Allow localhost if enabled
    if allowLocalhost && isLocalhost req then
      return ← handler req

    -- Check Authorization header
    match req.header "Authorization" with
    | some auth =>
      if auth == s!"Bearer {token}" then
        handler req
      else
        pure (Response.unauthorized "Invalid bearer token")
    | none =>
      pure (Response.unauthorized "Missing Authorization header")

/-- Conditional bearer auth middleware.

    Only applies authentication if a token is configured.
    If no token is provided, acts as identity middleware (no-op).
-/
def optionalBearerAuth (token : Option String) (allowLocalhost : Bool := true) : Citadel.Middleware :=
  match token with
  | some t => bearerAuth t allowLocalhost
  | none => Citadel.Middleware.identity

end AgentMail.Middleware.Auth
