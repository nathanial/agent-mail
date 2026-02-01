/-
  AgentMail.Middleware.RequestLog - Request logging middleware
-/
import Citadel

open Citadel

namespace AgentMail.Middleware.RequestLog

/-- Request logging middleware.

    Logs incoming requests and their response times.
    When disabled, acts as identity middleware.

    Log format: [LOG] METHOD PATH -> STATUS (elapsed_ms)
-/
def requestLog (enabled : Bool := true) : Citadel.Middleware :=
  fun handler req => do
    if !enabled then
      return ← handler req

    let startTime ← IO.monoMsNow
    let method := req.method.toString
    let path := req.path

    -- Log request
    IO.println s!"[LOG] {method} {path}"

    -- Process request
    let resp ← handler req

    -- Log response with timing
    let elapsed := (← IO.monoMsNow) - startTime
    IO.println s!"[LOG] {method} {path} -> {resp.status.code} ({elapsed}ms)"

    pure resp

end AgentMail.Middleware.RequestLog
