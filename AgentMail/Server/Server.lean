/-
  AgentMail.Server - HTTP server for agent-mail MCP
-/
import Citadel
import AgentMail.Config
import AgentMail.Middleware
import AgentMail.Mcp
import AgentMail.Protocol.JsonRpc
import AgentMail.Storage.Database
import AgentMail.ToolFilter
import AgentMail.OutputFormat
import AgentMail.Tools.Identity
import AgentMail.Tools.Messaging
import AgentMail.Tools.Contacts
import AgentMail.Tools.FileReservations
import AgentMail.Tools.GitGuard
import AgentMail.Tools.Search
import AgentMail.Tools.Macros
import AgentMail.Tools.BuildSlots
import AgentMail.Tools.Products
import AgentMail.Resources
import AgentMail.Web.App
import AgentMail.SSE

open Citadel

namespace AgentMail.Server

/-- Server version -/
def version : String := "0.1.0"

private def applyOutputFormatting (cfg : Config) (rpcReq : JsonRpc.Request) (resp : Response) : IO Response := do
  let formatValue := match rpcReq.params with
    | some params =>
      match params.getObjValAs? String "format" with
      | Except.ok v => some v
      | Except.error _ => none
    | none => none

  if formatValue.isNone && cfg.outputFormatDefault.isEmpty && cfg.toonDefaultFormat.isEmpty then
    return resp
  let bodyText := String.fromUTF8! resp.body
  match Lean.Json.parse bodyText with
  | Except.error _ => pure resp
  | Except.ok json =>
    match (Lean.FromJson.fromJson? json : Except String JsonRpc.Response) with
    | Except.error _ => pure resp
    | Except.ok rpcResp =>
      match rpcResp.result with
      | none => pure resp
      | some result =>
        match ← OutputFormat.apply result cfg formatValue rpcReq.method with
        | .error e =>
          let err := JsonRpc.Error.invalidParams (some e)
          let errResp := JsonRpc.Response.failure rpcReq.id err
          pure (Response.json (Lean.Json.compress (Lean.toJson errResp)))
        | .ok formatted =>
          let newResp : JsonRpc.Response := { rpcResp with result := some formatted }
          pure (Response.json (Lean.Json.compress (Lean.toJson newResp)))

private def dispatchTool (db : Storage.Database) (cfg : Config) (rpcReq : JsonRpc.Request) : IO Response := do
  match rpcReq.method with
  | "health_check" => Tools.Identity.handleHealthCheck db cfg rpcReq
  | "ensure_project" => Tools.Identity.handleEnsureProject db cfg rpcReq
  | "register_agent" => Tools.Identity.handleRegisterAgent db cfg rpcReq
  | "whois" => Tools.Identity.handleWhois db cfg rpcReq
  | "send_message" => Tools.Messaging.handleSendMessage db cfg rpcReq
  | "reply_message" => Tools.Messaging.handleReplyMessage db cfg rpcReq
  | "fetch_inbox" => Tools.Messaging.handleFetchInbox db cfg rpcReq
  | "mark_message_read" => Tools.Messaging.handleMarkRead db cfg rpcReq
  | "acknowledge_message" => Tools.Messaging.handleAcknowledge db cfg rpcReq
  | "request_contact" => Tools.Contacts.handleRequestContact db rpcReq
  | "respond_contact" => Tools.Contacts.handleRespondContact db rpcReq
  | "list_contacts" => Tools.Contacts.handleListContacts db rpcReq
  | "set_contact_policy" => Tools.Contacts.handleSetContactPolicy db rpcReq
  | "file_reservation_paths" => Tools.FileReservations.handleFileReservationPaths db cfg rpcReq
  | "release_file_reservations" => Tools.FileReservations.handleReleaseFileReservations db cfg rpcReq
  | "renew_file_reservations" => Tools.FileReservations.handleRenewFileReservations db cfg rpcReq
  | "force_release_file_reservation" => Tools.FileReservations.handleForceReleaseFileReservation db cfg rpcReq
  | "install_precommit_guard" => Tools.GitGuard.handleInstallPrecommitGuard db cfg rpcReq
  | "uninstall_precommit_guard" => Tools.GitGuard.handleUninstallPrecommitGuard db cfg rpcReq
  | "search_messages" => Tools.Search.handleSearchMessages db cfg rpcReq
  | "summarize_thread" => Tools.Search.handleSummarizeThread db cfg rpcReq
  | "macro_start_session" => Tools.Macros.handleMacroStartSession db cfg rpcReq
  | "macro_prepare_thread" => Tools.Macros.handleMacroPrepareThread db cfg rpcReq
  | "macro_file_reservation_cycle" => Tools.Macros.handleMacroFileReservationCycle db cfg rpcReq
  | "macro_contact_handshake" => Tools.Macros.handleMacroContactHandshake db cfg rpcReq
  | "acquire_build_slot" => Tools.BuildSlots.handleAcquireBuildSlot db cfg rpcReq
  | "renew_build_slot" => Tools.BuildSlots.handleRenewBuildSlot db cfg rpcReq
  | "release_build_slot" => Tools.BuildSlots.handleReleaseBuildSlot db cfg rpcReq
  | "ensure_product" => Tools.Products.handleEnsureProduct db cfg rpcReq
  | "products_link" => Tools.Products.handleProductsLink db cfg rpcReq
  | "search_messages_product" => Tools.Products.handleSearchMessagesProduct db cfg rpcReq
  | "fetch_inbox_product" => Tools.Products.handleFetchInboxProduct db cfg rpcReq
  | "summarize_thread_product" => Tools.Products.handleSummarizeThreadProduct db cfg rpcReq
  | _ =>
    let err := JsonRpc.Error.methodNotFound rpcReq.method
    let resp := JsonRpc.Response.failure rpcReq.id err
    pure (Response.json (Lean.Json.compress (Lean.toJson resp)))

private def handleToolRequest (db : Storage.Database) (cfg : Config) (rpcReq : JsonRpc.Request) : IO Response := do
  -- Optional tool filtering (hide tools when not allowed)
  if cfg.toolFilter.enabled && !ToolFilter.isToolAllowed cfg.toolFilter rpcReq.method then
    if rpcReq.isNotification then
      return Response.noContent
    let err := JsonRpc.Error.methodNotFound rpcReq.method
    let resp := JsonRpc.Response.failure rpcReq.id err
    return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Notifications must not return a response
  if rpcReq.isNotification then
    pure Response.noContent
  else
    let resp ← dispatchTool db cfg rpcReq
    applyOutputFormatting cfg rpcReq resp

private def parseJsonRpc (req : ServerRequest) : IO (Except Response JsonRpc.Request) := do
  let body := req.bodyString
  match Lean.Json.parse body with
  | Except.error e =>
    let err := JsonRpc.Error.parseError (some e)
    let resp := JsonRpc.Response.failure none err
    pure (Except.error (Response.json (Lean.Json.compress (Lean.toJson resp))))
  | Except.ok json =>
    match Lean.FromJson.fromJson? json with
    | Except.error e =>
      let err := JsonRpc.Error.invalidRequest (some e)
      let resp := JsonRpc.Response.failure none err
      pure (Except.error (Response.json (Lean.Json.compress (Lean.toJson resp))))
    | Except.ok (rpcReq : JsonRpc.Request) =>
      pure (Except.ok rpcReq)

private def originHost (origin : String) : String :=
  let withoutScheme := match origin.splitOn "://" with
    | _ :: rest => rest.head?.getD origin
    | [] => origin
  match withoutScheme.splitOn "/" with
  | h :: _ => h
  | [] => withoutScheme

private def hostOnly (hostHeader : String) : String :=
  match hostHeader.splitOn ":" with
  | h :: _ => h
  | [] => hostHeader

private def isLocalHost (host : String) : Bool :=
  host == "localhost" || host == "127.0.0.1" || host == "[::1]"

private def isOriginAllowed (cfg : Config) (req : ServerRequest) : Bool :=
  match req.header "Origin" with
  | none => true
  | some origin =>
    let originHost := originHost origin
    let hostHeader := req.header "Host" |>.getD ""
    let hostName := hostOnly hostHeader
    let sameHost := (!hostName.isEmpty) && hostName == originHost
    let localMatch := isLocalHost originHost && isLocalHost hostName
    let inCorsList := !cfg.cors.origins.isEmpty && cfg.cors.origins.contains origin
    sameHost || localMatch || inCorsList

private def handleMcpGet (cfg : Config) (req : ServerRequest) : IO Response := do
  if !isOriginAllowed cfg req then
    return Response.forbidden "Origin not allowed"
  -- Streamable HTTP GET is for SSE. We do not support SSE yet.
  pure (Response.methodNotAllowed ["POST"])

private def handleMcpPost (db : Storage.Database) (cfg : Config) (req : ServerRequest) : IO Response := do
  if !isOriginAllowed cfg req then
    return Response.forbidden "Origin not allowed"
  let rpcReq ← parseJsonRpc req
  match rpcReq with
  | Except.error resp => pure resp
  | Except.ok rpcReq =>
    -- MCP lifecycle and tool routing
    match rpcReq.method with
    | "initialize" =>
      let payload := Mcp.initializeResult "agent-mail" version
      let resp := JsonRpc.Response.success rpcReq.id payload
      pure (Response.json (Lean.Json.compress (Lean.toJson resp)))
    | "notifications/initialized" =>
      pure Response.noContent
    | "ping" =>
      let resp := JsonRpc.Response.success rpcReq.id (Lean.Json.mkObj [])
      pure (Response.json (Lean.Json.compress (Lean.toJson resp)))
    | "tools/list" =>
      let tools := Mcp.allTools.filter fun tool =>
        if cfg.toolFilter.enabled then
          ToolFilter.isToolAllowed cfg.toolFilter tool.name
        else
          true
      let toolsJson := tools.toArray.map Mcp.ToolSpec.toJson
      let payload := Lean.Json.mkObj [
        ("tools", Lean.Json.arr toolsJson)
      ]
      let resp := JsonRpc.Response.success rpcReq.id payload
      pure (Response.json (Lean.Json.compress (Lean.toJson resp)))
    | "tools/call" =>
      let params := rpcReq.params.getD Lean.Json.null
      let toolName ← match params.getObjValAs? String "name" with
        | Except.ok name => pure name
        | Except.error _ =>
          let err := JsonRpc.Error.invalidParams (some "missing required param: name")
          let resp := JsonRpc.Response.failure rpcReq.id err
          return Response.json (Lean.Json.compress (Lean.toJson resp))
      let args := match params.getObjVal? "arguments" with
        | Except.ok v => if v.isNull then none else some v
        | Except.error _ => none
      let toolReq : JsonRpc.Request := { method := toolName, params := args, id := rpcReq.id }
      let toolResp ← handleToolRequest db cfg toolReq
      let bodyText := String.fromUTF8! toolResp.body
      match Lean.Json.parse bodyText with
      | Except.error _ =>
        let payload := Mcp.callToolResult (Lean.Json.str "Failed to parse tool response") true
        let resp := JsonRpc.Response.success rpcReq.id payload
        pure (Response.json (Lean.Json.compress (Lean.toJson resp)))
      | Except.ok json =>
        match (Lean.FromJson.fromJson? json : Except String JsonRpc.Response) with
        | Except.error _ =>
          let payload := Mcp.callToolResult (Lean.Json.str "Invalid tool response format") true
          let resp := JsonRpc.Response.success rpcReq.id payload
          pure (Response.json (Lean.Json.compress (Lean.toJson resp)))
        | Except.ok toolRpc =>
          match toolRpc.error, toolRpc.result with
          | some err, _ =>
            let payload := Mcp.callToolResult (Lean.toJson err) true
            let resp := JsonRpc.Response.success rpcReq.id payload
            pure (Response.json (Lean.Json.compress (Lean.toJson resp)))
          | none, some result =>
            let payload := Mcp.callToolResult result false
            let resp := JsonRpc.Response.success rpcReq.id payload
            pure (Response.json (Lean.Json.compress (Lean.toJson resp)))
          | none, none =>
            let payload := Mcp.callToolResult (Lean.Json.str "Tool returned no result") true
            let resp := JsonRpc.Response.success rpcReq.id payload
            pure (Response.json (Lean.Json.compress (Lean.toJson resp)))
    | _ =>
      -- Legacy JSON-RPC tool call
      handleToolRequest db cfg rpcReq

/-- Handle health check requests -/
def handleHealth (_req : ServerRequest) : IO Response := do
  let json := Lean.Json.mkObj [
    ("status", Lean.Json.str "ok"),
    ("version", Lean.Json.str version)
  ]
  pure (Response.json (Lean.Json.compress json))

/-- Create and configure the server with middleware -/
private def attachWebUi (server : Citadel.Server) (handler : Option Citadel.Handler) : Citadel.Server :=
  match handler with
  | none => server
  | some h =>
    server
      |>.get "/app" h
      |>.get "/app/*" h

/-- Create and configure the server with middleware -/
def create (cfg : Config) (db : Storage.Database) (rateLimitState : Middleware.RateLimit.RateLimitState)
    (webHandler : Option Citadel.Handler := none) : Citadel.Server :=
  -- Build base server with routes
  let server := Citadel.Server.create { port := cfg.port, host := cfg.host }
    |> (fun s => attachWebUi s webHandler)
    |>.post "/rpc" (handleMcpPost db cfg)
    |>.get "/rpc" (handleMcpGet cfg)
    |>.post "/mcp" (handleMcpPost db cfg)
    |>.get "/mcp" (handleMcpGet cfg)
    |>.get "/health" handleHealth
    -- Discovery resources
    |>.get "/resource/projects" (Resources.Discovery.handleProjects db cfg)
    |>.get "/resource/project/:slug" (Resources.Discovery.handleProject db cfg)
    |>.get "/resource/agents/:project_key" (Resources.Discovery.handleAgents db cfg)
    |>.get "/resource/identity/:project" (Resources.Discovery.handleIdentity db cfg)
    |>.get "/resource/product/:key" (Resources.Discovery.handleProduct db cfg)
    |>.get "/resource/threads/:project_key" (Resources.Threads.handleThreads db cfg)
    -- Mail resources
    |>.get "/resource/message/:id" (Resources.Mail.handleMessage db cfg)
    |>.get "/resource/thread/:id" (Resources.Mail.handleThread db cfg)
    |>.get "/resource/inbox/:agent" (Resources.Mail.handleInbox db cfg)
    |>.get "/resource/outbox/:agent" (Resources.Mail.handleOutbox db cfg)
    |>.get "/resource/mailbox/:agent" (Resources.Mail.handleMailbox db cfg)
    -- View resources
    |>.get "/resource/views/urgent-unread/:agent" (Resources.Views.handleUrgentUnread db cfg)
    |>.get "/resource/views/ack-required/:agent" (Resources.Views.handleAckRequired db cfg)
    |>.get "/resource/views/acks-stale/:agent" (Resources.Views.handleAcksStale db cfg)
    |>.get "/resource/views/ack-overdue/:agent" (Resources.Views.handleAckOverdue db cfg)
    -- File reservations
    |>.get "/resource/file_reservations/:slug" (Resources.FileReservations.handleFileReservations db cfg)
    -- Config
    |>.get "/resource/config/environment" (Resources.Config.handleEnvironment cfg)

  -- Apply middleware chain (order: request flows through outer→inner, response flows inner→outer)
  -- 1. Request logging (outermost - logs all requests including rejected ones)
  -- 2. CORS (handle preflight before auth)
  -- 3. Rate limiting (reject before expensive operations)
  -- 4. Authentication (innermost security layer)
  server
    |>.use (Middleware.RequestLog.requestLog cfg.requestLogEnabled)
    |>.use (Middleware.CORS.cors cfg.cors)
    |>.use (Middleware.RateLimit.rateLimit rateLimitState cfg.http.rateLimit)
    |>.use (Middleware.Security.jwtRbac cfg)
    |>.use (Middleware.Auth.optionalBearerAuth cfg.http.bearerToken cfg.http.allowLocalhostUnauthenticated)

/-- Run the server (blocking) -/
def run (cfg : Config) (db : Storage.Database) : IO Unit := do
  IO.println s!"Starting agent-mail server v{version}"
  IO.println s!"  Host: {cfg.host}"
  IO.println s!"  Port: {cfg.port}"
  IO.println s!"  Database: {cfg.databasePath}"
  IO.println s!"  Web UI: http://{cfg.host}:{cfg.port}/app"

  -- Display security settings
  if cfg.http.bearerToken.isSome then
    IO.println s!"  Auth: Bearer token required"
    if cfg.http.allowLocalhostUnauthenticated then
      IO.println s!"  Auth: Localhost bypass enabled"
  else
    IO.println s!"  Auth: Disabled (no token configured)"

  if cfg.http.rateLimit.enabled then
    IO.println s!"  Rate limit: {cfg.http.rateLimit.toolsPerMinute}/min (tools), {cfg.http.rateLimit.resourcesPerMinute}/min (resources)"

  if cfg.cors.enabled then
    let originsDisplay := if cfg.cors.origins.isEmpty then "*" else String.intercalate ", " cfg.cors.origins
    IO.println s!"  CORS: Enabled (origins: {originsDisplay})"

  if cfg.requestLogEnabled then
    IO.println s!"  Request logging: Enabled"

  IO.println ""
  IO.println s!"Endpoints:"
  IO.println s!"  POST /mcp    - MCP JSON-RPC endpoint"
  IO.println s!"  GET  /mcp    - MCP SSE (405: not supported)"
  IO.println s!"  POST /rpc    - Legacy JSON-RPC + MCP endpoint"
  IO.println s!"  GET  /rpc    - MCP SSE (405: not supported)"
  IO.println s!"  GET  /health - Health check"
  IO.println s!"  GET  /app    - Live web UI"
  IO.println s!"  GET  /app/events/mail - Live mail events (SSE)"
  IO.println ""
  IO.println s!"Resources:"
  IO.println s!"  GET  /resource/projects                    - List all projects"
  IO.println s!"  GET  /resource/project/:slug               - Project details"
  IO.println s!"  GET  /resource/agents/:project_key         - Agents in project"
  IO.println s!"  GET  /resource/identity/:project           - Identity resolution"
  IO.println s!"  GET  /resource/product/:key                - Product with projects"
  IO.println s!"  GET  /resource/threads/:project_key        - Thread summaries"
  IO.println s!"  GET  /resource/message/:id                 - Single message"
  IO.println s!"  GET  /resource/thread/:id                  - Thread messages"
  IO.println s!"  GET  /resource/inbox/:agent                - Agent inbox"
  IO.println s!"  GET  /resource/outbox/:agent               - Sent messages"
  IO.println s!"  GET  /resource/mailbox/:agent              - Full mailbox"
  IO.println s!"  GET  /resource/views/urgent-unread/:agent  - Urgent unread"
  IO.println s!"  GET  /resource/views/ack-required/:agent   - Needing ack"
  IO.println s!"  GET  /resource/views/acks-stale/:agent     - Stale acks"
  IO.println s!"  GET  /resource/views/ack-overdue/:agent    - Overdue acks"
  IO.println s!"  GET  /resource/file_reservations/:slug     - File reservations"
  IO.println s!"  GET  /resource/config/environment          - Server config"
  IO.println ""
  IO.println "Server running. Press Ctrl+C to stop."

  -- Initialize rate limit state
  let rateLimitState ← Middleware.RateLimit.RateLimitState.create

  -- Initialize Loom template manager for the web UI
  let webApp := AgentMail.Web.buildApp
  let stencilRef ← match webApp.stencilConfig with
    | some config =>
      IO.println s!"  Templates: Discovering from {config.templateDir}/"
      let manager ← Loom.Stencil.Manager.discover config
      IO.println s!"  Templates: {manager.templateCount} templates, {manager.partialCount} partials, {manager.layoutCount} layouts"
      let ref ← IO.mkRef manager
      pure (some ref)
    | none => pure none

  let webHandler := AgentMail.Web.buildHandler stencilRef

  -- SSE for live UI updates
  let sseManager ← Citadel.SSE.ConnectionManager.create
  AgentMail.SSE.setManager sseManager

  let server := create cfg db rateLimitState (some webHandler)
    |>.withSSE sseManager
    |>.sseRoute "/app/events/mail" AgentMail.SSE.defaultTopic
  server.run

end AgentMail.Server
