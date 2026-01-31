/-
  AgentMail.Server - HTTP server for agent-mail MCP
-/
import Citadel
import AgentMail.Config
import AgentMail.Protocol.JsonRpc
import AgentMail.Storage.Database

open Citadel

namespace AgentMail.Server

/-- Server version -/
def version : String := "0.1.0"

/-- Handle JSON-RPC requests -/
def handleRpc (_db : Storage.Database) (req : ServerRequest) : IO Response := do
  let body := req.bodyString

  -- Parse JSON
  match Lean.Json.parse body with
  | Except.error e =>
    let err := JsonRpc.Error.parseError (some e)
    let resp := JsonRpc.Response.failure none err
    return Response.json (Lean.Json.compress (Lean.toJson resp))
  | Except.ok json =>
    -- Parse as JSON-RPC request
    match Lean.FromJson.fromJson? json with
    | Except.error e =>
      let err := JsonRpc.Error.invalidRequest (some e)
      let resp := JsonRpc.Response.failure none err
      return Response.json (Lean.Json.compress (Lean.toJson resp))
    | Except.ok (rpcReq : JsonRpc.Request) =>
      -- Notifications must not return a response
      if rpcReq.isNotification then
        pure Response.noContent
      else
        -- For Phase 1, all methods return "method not found"
        -- Phase 2 will implement actual handlers
        let err := JsonRpc.Error.methodNotFound rpcReq.method
        let resp := JsonRpc.Response.failure rpcReq.id err
        pure (Response.json (Lean.Json.compress (Lean.toJson resp)))

/-- Handle health check requests -/
def handleHealth (_req : ServerRequest) : IO Response := do
  let json := Lean.Json.mkObj [
    ("status", Lean.Json.str "ok"),
    ("version", Lean.Json.str version)
  ]
  pure (Response.json (Lean.Json.compress json))

/-- Create and configure the server -/
def create (cfg : Config) (db : Storage.Database) : Citadel.Server :=
  Citadel.Server.create { port := cfg.port, host := cfg.host }
    |>.post "/rpc" (handleRpc db)
    |>.get "/health" handleHealth

/-- Run the server (blocking) -/
def run (cfg : Config) (db : Storage.Database) : IO Unit := do
  IO.println s!"Starting agent-mail server v{version}"
  IO.println s!"  Host: {cfg.host}"
  IO.println s!"  Port: {cfg.port}"
  IO.println s!"  Database: {cfg.databasePath}"
  IO.println ""
  IO.println s!"Endpoints:"
  IO.println s!"  POST /rpc    - JSON-RPC 2.0 endpoint"
  IO.println s!"  GET  /health - Health check"
  IO.println ""
  IO.println "Server running. Press Ctrl+C to stop."

  let server := create cfg db
  server.run

end AgentMail.Server
