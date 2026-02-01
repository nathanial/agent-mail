/-
  AgentMail.Middleware.CORS - Cross-Origin Resource Sharing middleware
-/
import Citadel

open Citadel

namespace AgentMail.Middleware.CORS

/-- CORS configuration -/
structure CorsConfig where
  /-- Whether CORS handling is enabled -/
  enabled : Bool := false
  /-- Allowed origins (empty list means allow all) -/
  origins : List String := []
  /-- Whether to allow credentials -/
  allowCredentials : Bool := false
  /-- Allowed HTTP methods -/
  allowMethods : List String := ["GET", "POST", "PUT", "DELETE", "PATCH", "OPTIONS"]
  /-- Allowed headers -/
  allowHeaders : List String := ["Content-Type", "Authorization", "Accept"]
  /-- Headers to expose to the client -/
  exposeHeaders : List String := []
  /-- Max age for preflight cache in seconds -/
  maxAge : Nat := 86400  -- 24 hours
  deriving Repr, Inhabited

namespace CorsConfig

/-- Default CORS config (disabled) -/
def default : CorsConfig := {}

/-- Permissive CORS config (allows all origins) -/
def permissive : CorsConfig := {
  enabled := true
  origins := []
  allowCredentials := false
}

end CorsConfig

/-- Check if an origin is allowed -/
private def isOriginAllowed (config : CorsConfig) (origin : String) : Bool :=
  config.origins.isEmpty || config.origins.contains origin

/-- Add a header to a response -/
private def addHeader (resp : Response) (name value : String) : Response :=
  { resp with headers := resp.headers.add name value }

/-- Add CORS headers to a response -/
private def addCorsHeaders (config : CorsConfig) (origin : String) (resp : Response) : Response :=
  let resp := addHeader resp "Access-Control-Allow-Origin" origin

  let resp := if config.allowCredentials then
    addHeader resp "Access-Control-Allow-Credentials" "true"
  else
    resp

  let resp := if !config.exposeHeaders.isEmpty then
    addHeader resp "Access-Control-Expose-Headers" (String.intercalate ", " config.exposeHeaders)
  else
    resp

  resp

/-- Create preflight response with CORS headers -/
private def preflightResponse (config : CorsConfig) (origin : String) : Response :=
  let resp := addHeader Response.noContent "Access-Control-Allow-Origin" origin
  let resp := addHeader resp "Access-Control-Allow-Methods" (String.intercalate ", " config.allowMethods)
  let resp := addHeader resp "Access-Control-Allow-Headers" (String.intercalate ", " config.allowHeaders)
  let resp := addHeader resp "Access-Control-Max-Age" (toString config.maxAge)

  if config.allowCredentials then
    addHeader resp "Access-Control-Allow-Credentials" "true"
  else
    resp

/-- CORS middleware.

    Handles Cross-Origin Resource Sharing headers:
    - Responds to OPTIONS preflight requests
    - Adds Access-Control-* headers to responses
    - Validates origin against allowed list

    If CORS is disabled, acts as identity middleware.
-/
def cors (config : CorsConfig) : Citadel.Middleware :=
  fun handler req => do
    -- Skip if disabled
    if !config.enabled then
      return ← handler req

    -- Get origin header
    let origin := req.header "Origin" |>.getD "*"

    -- Check if origin is allowed
    if !isOriginAllowed config origin then
      return Response.forbidden "Origin not allowed"

    -- Handle preflight requests
    if req.method == .OPTIONS then
      return preflightResponse config origin

    -- Process regular request and add CORS headers
    let resp ← handler req
    pure (addCorsHeaders config origin resp)

end AgentMail.Middleware.CORS
