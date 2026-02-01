/-
  Tests.Middleware.Auth - Tests for bearer token authentication middleware
-/
import Crucible
import AgentMail.Middleware.Auth
import Citadel

open Crucible
open Citadel
open AgentMail.Middleware.Auth

testSuite "Middleware.Auth"

/-- Create a mock request with optional Authorization header -/
private def mockRequest (path : String := "/rpc") (authHeader : Option String := none) (host : String := "example.com") : ServerRequest :=
  let headers := match authHeader with
    | some auth =>
        Herald.Core.Headers.add
          (Herald.Core.Headers.add Herald.Core.Headers.empty "Authorization" auth)
          "Host" host
    | none =>
        Herald.Core.Headers.add Herald.Core.Headers.empty "Host" host
  { request := {
      method := .POST
      path := path
      version := .http11
      headers := headers
      body := ByteArray.empty
    }
    params := []
  }

/-- Helper handler that returns 200 OK -/
private def okHandler : Handler := fun _ => pure (Response.ok "success")

test "Allows request with valid bearer token" := do
  let mw := bearerAuth "secret123" false
  let req := mockRequest "/rpc" (some "Bearer secret123")
  let resp ← mw okHandler req
  resp.status.code ≡ 200

test "Rejects request with invalid bearer token" := do
  let mw := bearerAuth "secret123" false
  let req := mockRequest "/rpc" (some "Bearer wrongtoken")
  let resp ← mw okHandler req
  resp.status.code ≡ 401

test "Rejects request with missing Authorization header" := do
  let mw := bearerAuth "secret123" false
  let req := mockRequest "/rpc" none
  let resp ← mw okHandler req
  resp.status.code ≡ 401

test "Rejects request with malformed Authorization header" := do
  let mw := bearerAuth "secret123" false
  let req := mockRequest "/rpc" (some "Basic dXNlcjpwYXNz")
  let resp ← mw okHandler req
  resp.status.code ≡ 401

test "Skips auth for OPTIONS requests (CORS preflight)" := do
  let mw := bearerAuth "secret123" false
  let req : ServerRequest := {
    request := {
      method := .OPTIONS
      path := "/rpc"
      version := .http11
      headers := Herald.Core.Headers.add Herald.Core.Headers.empty "Host" "example.com"
      body := ByteArray.empty
    }
    params := []
  }
  let resp ← mw okHandler req
  resp.status.code ≡ 200

test "Skips auth for /health endpoint" := do
  let mw := bearerAuth "secret123" false
  let req := mockRequest "/health" none
  let resp ← mw okHandler req
  resp.status.code ≡ 200

test "Skips auth for /health/xxx endpoint" := do
  let mw := bearerAuth "secret123" false
  let req := mockRequest "/health/live" none
  let resp ← mw okHandler req
  resp.status.code ≡ 200

test "Allows localhost when enabled" := do
  let mw := bearerAuth "secret123" true  -- allowLocalhost = true
  let req := mockRequest "/rpc" none "localhost:8765"
  let resp ← mw okHandler req
  resp.status.code ≡ 200

test "Allows 127.0.0.1 when localhost enabled" := do
  let mw := bearerAuth "secret123" true
  let req := mockRequest "/rpc" none "127.0.0.1:8765"
  let resp ← mw okHandler req
  resp.status.code ≡ 200

test "Rejects localhost when disabled" := do
  let mw := bearerAuth "secret123" false
  let req := mockRequest "/rpc" none "localhost:8765"
  let resp ← mw okHandler req
  resp.status.code ≡ 401

test "optionalBearerAuth passes through when no token" := do
  let mw := optionalBearerAuth none
  let req := mockRequest "/rpc" none
  let resp ← mw okHandler req
  resp.status.code ≡ 200

test "optionalBearerAuth enforces auth when token set" := do
  let mw := optionalBearerAuth (some "secret123")
  let req := mockRequest "/rpc" none
  let resp ← mw okHandler req
  resp.status.code ≡ 401
