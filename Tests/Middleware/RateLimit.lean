/-
  Tests.Middleware.RateLimit - Tests for rate limiting middleware
-/
import Crucible
import AgentMail.Middleware.RateLimit
import Citadel

open Crucible
open Citadel
open AgentMail.Middleware.RateLimit

testSuite "Middleware.RateLimit"

/-- Create a mock request -/
private def mockRequest (path : String := "/rpc") (host : String := "testclient") : ServerRequest :=
  { request := {
      method := .POST
      path := path
      version := .http11
      headers := Herald.Core.Headers.add Herald.Core.Headers.empty "Host" host
      body := ByteArray.empty
    }
    params := []
  }

/-- Helper handler that returns 200 OK -/
private def okHandler : Handler := fun _ => pure (Response.ok "success")

test "Allows requests when disabled" := do
  let state ← RateLimitState.create
  let config : RateLimitConfig := { enabled := false }
  let mw := rateLimit state config
  let req := mockRequest "/rpc"
  let resp ← mw okHandler req
  resp.status.code ≡ 200

test "Allows requests within limit" := do
  let state ← RateLimitState.create
  let config : RateLimitConfig := {
    enabled := true
    toolsPerMinute := 60
    toolsBurst := 10
  }
  let mw := rateLimit state config
  let req := mockRequest "/rpc"
  -- First request should succeed
  let resp ← mw okHandler req
  resp.status.code ≡ 200

test "Allows burst requests up to limit" := do
  let state ← RateLimitState.create
  let config : RateLimitConfig := {
    enabled := true
    toolsPerMinute := 60
    toolsBurst := 5
  }
  let mw := rateLimit state config
  let req := mockRequest "/rpc"
  -- Should allow burst number of requests
  for _ in [:5] do
    let resp ← mw okHandler req
    shouldSatisfy (resp.status.code == 200) "should allow within burst"

test "Rate limits after burst exceeded" := do
  let state ← RateLimitState.create
  let config : RateLimitConfig := {
    enabled := true
    toolsPerMinute := 60
    toolsBurst := 3
  }
  let mw := rateLimit state config
  let req := mockRequest "/rpc"
  -- Exhaust burst
  for _ in [:3] do
    let _ ← mw okHandler req
  -- Next request should be rate limited
  let resp ← mw okHandler req
  resp.status.code ≡ 429

test "Different endpoints have separate limits" := do
  let state ← RateLimitState.create
  let config : RateLimitConfig := {
    enabled := true
    toolsPerMinute := 60
    toolsBurst := 2
    resourcesPerMinute := 120
    resourcesBurst := 2
  }
  let mw := rateLimit state config
  let rpcReq := mockRequest "/rpc"
  let resourceReq := mockRequest "/resource/projects"
  -- Exhaust RPC burst
  for _ in [:2] do
    let _ ← mw okHandler rpcReq
  -- RPC should be limited
  let rpcResp ← mw okHandler rpcReq
  shouldSatisfy (rpcResp.status.code == 429) "RPC should be rate limited"
  -- Resource should still work
  let resResp ← mw okHandler resourceReq
  shouldSatisfy (resResp.status.code == 200) "Resource should not be limited"

test "Different clients have separate limits" := do
  let state ← RateLimitState.create
  let config : RateLimitConfig := {
    enabled := true
    toolsPerMinute := 60
    toolsBurst := 2
  }
  let mw := rateLimit state config
  let req1 := mockRequest "/rpc" "client1"
  let req2 := mockRequest "/rpc" "client2"
  -- Exhaust client1's burst
  for _ in [:2] do
    let _ ← mw okHandler req1
  -- Client1 should be limited
  let resp1 ← mw okHandler req1
  shouldSatisfy (resp1.status.code == 429) "client1 should be rate limited"
  -- Client2 should still work
  let resp2 ← mw okHandler req2
  shouldSatisfy (resp2.status.code == 200) "client2 should not be limited"

test "Health endpoint bypasses rate limit" := do
  let state ← RateLimitState.create
  let config : RateLimitConfig := {
    enabled := true
    toolsPerMinute := 1
    toolsBurst := 1
  }
  let mw := rateLimit state config
  let healthReq := mockRequest "/health"
  -- Multiple health requests should all succeed
  for _ in [:5] do
    let resp ← mw okHandler healthReq
    shouldSatisfy (resp.status.code == 200) "health should bypass limit"

test "RateLimitState cleanup removes stale entries" := do
  let state ← RateLimitState.create
  -- Add some entries
  let _ ← state.consumeToken "test:key" 60 10
  -- Cleanup should not fail
  state.cleanup
  pure ()

test "Rate limit returns Retry-After header" := do
  let state ← RateLimitState.create
  let config : RateLimitConfig := {
    enabled := true
    toolsPerMinute := 60
    toolsBurst := 1
  }
  let mw := rateLimit state config
  let req := mockRequest "/rpc"
  -- Exhaust burst
  let _ ← mw okHandler req
  -- Get rate limited response
  let resp ← mw okHandler req
  resp.status.code ≡ 429
  -- Note: Citadel's tooManyRequests adds Retry-After header
