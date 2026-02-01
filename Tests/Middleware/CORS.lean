/-
  Tests.Middleware.CORS - Tests for CORS middleware
-/
import Crucible
import Staple
import AgentMail.Middleware.CORS
import Citadel

open Crucible
open Citadel
open AgentMail.Middleware.CORS
open Staple (String.containsSubstr)

testSuite "Middleware.CORS"

/-- Create a mock request with optional Origin header -/
private def mockRequest (method : Method := .GET) (origin : Option String := none) : ServerRequest :=
  let headers := match origin with
    | some o => Herald.Core.Headers.add Herald.Core.Headers.empty "Origin" o
    | none => Herald.Core.Headers.empty
  { request := {
      method := method
      path := "/rpc"
      version := .http11
      headers := headers
      body := ByteArray.empty
    }
    params := []
  }

/-- Helper handler that returns 200 OK -/
private def okHandler : Handler := fun _ => pure (Response.ok "success")

test "Passes through when disabled" := do
  let config : CorsConfig := { enabled := false }
  let mw := cors config
  let req := mockRequest .GET (some "http://example.com")
  let resp ← mw okHandler req
  resp.status.code ≡ 200
  -- Should not have CORS headers when disabled
  shouldSatisfy (resp.headers.get "Access-Control-Allow-Origin" |>.isNone) "should not have CORS header"

test "Adds CORS headers when enabled" := do
  let config : CorsConfig := { enabled := true }
  let mw := cors config
  let req := mockRequest .GET (some "http://example.com")
  let resp ← mw okHandler req
  resp.status.code ≡ 200
  shouldSatisfy (resp.headers.get "Access-Control-Allow-Origin" |>.isSome) "should have CORS header"

test "Uses request origin in response" := do
  let config : CorsConfig := { enabled := true }
  let mw := cors config
  let req := mockRequest .GET (some "http://myapp.com")
  let resp ← mw okHandler req
  resp.headers.get "Access-Control-Allow-Origin" ≡ some "http://myapp.com"

test "Handles OPTIONS preflight request" := do
  let config : CorsConfig := { enabled := true }
  let mw := cors config
  let req := mockRequest .OPTIONS (some "http://example.com")
  let resp ← mw okHandler req
  resp.status.code ≡ 204  -- No Content
  shouldSatisfy (resp.headers.get "Access-Control-Allow-Methods" |>.isSome) "should have Allow-Methods"
  shouldSatisfy (resp.headers.get "Access-Control-Allow-Headers" |>.isSome) "should have Allow-Headers"
  shouldSatisfy (resp.headers.get "Access-Control-Max-Age" |>.isSome) "should have Max-Age"

test "Includes configured methods in preflight" := do
  let config : CorsConfig := {
    enabled := true
    allowMethods := ["GET", "POST", "DELETE"]
  }
  let mw := cors config
  let req := mockRequest .OPTIONS (some "http://example.com")
  let resp ← mw okHandler req
  let methods := resp.headers.get "Access-Control-Allow-Methods" |>.getD ""
  shouldSatisfy (methods.containsSubstr "GET") "should include GET"
  shouldSatisfy (methods.containsSubstr "POST") "should include POST"
  shouldSatisfy (methods.containsSubstr "DELETE") "should include DELETE"

test "Includes configured headers in preflight" := do
  let config : CorsConfig := {
    enabled := true
    allowHeaders := ["Content-Type", "X-Custom-Header"]
  }
  let mw := cors config
  let req := mockRequest .OPTIONS (some "http://example.com")
  let resp ← mw okHandler req
  let headers := resp.headers.get "Access-Control-Allow-Headers" |>.getD ""
  shouldSatisfy (headers.containsSubstr "Content-Type") "should include Content-Type"
  shouldSatisfy (headers.containsSubstr "X-Custom-Header") "should include X-Custom-Header"

test "Allows all origins when list is empty" := do
  let config : CorsConfig := { enabled := true, origins := [] }
  let mw := cors config
  let req := mockRequest .GET (some "http://anyorigin.com")
  let resp ← mw okHandler req
  resp.status.code ≡ 200

test "Allows origin in allowed list" := do
  let config : CorsConfig := {
    enabled := true
    origins := ["http://allowed.com", "http://also-allowed.com"]
  }
  let mw := cors config
  let req := mockRequest .GET (some "http://allowed.com")
  let resp ← mw okHandler req
  resp.status.code ≡ 200

test "Rejects origin not in allowed list" := do
  let config : CorsConfig := {
    enabled := true
    origins := ["http://allowed.com"]
  }
  let mw := cors config
  let req := mockRequest .GET (some "http://notallowed.com")
  let resp ← mw okHandler req
  resp.status.code ≡ 403

test "Includes credentials header when enabled" := do
  let config : CorsConfig := {
    enabled := true
    allowCredentials := true
  }
  let mw := cors config
  let req := mockRequest .GET (some "http://example.com")
  let resp ← mw okHandler req
  resp.headers.get "Access-Control-Allow-Credentials" ≡ some "true"

test "Includes expose headers when configured" := do
  let config : CorsConfig := {
    enabled := true
    exposeHeaders := ["X-Total-Count", "X-Page-Count"]
  }
  let mw := cors config
  let req := mockRequest .GET (some "http://example.com")
  let resp ← mw okHandler req
  let exposed := resp.headers.get "Access-Control-Expose-Headers" |>.getD ""
  shouldSatisfy (exposed.containsSubstr "X-Total-Count") "should expose X-Total-Count"
  shouldSatisfy (exposed.containsSubstr "X-Page-Count") "should expose X-Page-Count"

test "Default config is disabled" := do
  let config := CorsConfig.default
  shouldSatisfy (!config.enabled) "default should be disabled"

test "Permissive config allows all origins" := do
  let config := CorsConfig.permissive
  shouldSatisfy config.enabled "permissive should be enabled"
  shouldSatisfy config.origins.isEmpty "permissive should allow all origins"
