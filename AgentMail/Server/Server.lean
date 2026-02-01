/-
  AgentMail.Server - HTTP server for agent-mail MCP
-/
import Citadel
import AgentMail.Config
import AgentMail.Middleware
import AgentMail.Protocol.JsonRpc
import AgentMail.Storage.Database
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

open Citadel

namespace AgentMail.Server

/-- Server version -/
def version : String := "0.1.0"

/-- Handle JSON-RPC requests -/
def handleRpc (db : Storage.Database) (cfg : Config) (req : ServerRequest) : IO Response := do
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
        -- Route to appropriate handler
        match rpcReq.method with
        | "health_check" => Tools.Identity.handleHealthCheck db cfg rpcReq
        | "ensure_project" => Tools.Identity.handleEnsureProject db cfg rpcReq
        | "register_agent" => Tools.Identity.handleRegisterAgent db cfg rpcReq
        | "whois" => Tools.Identity.handleWhois db cfg rpcReq
        | "send_message" => Tools.Messaging.handleSendMessage db cfg rpcReq
        | "reply_message" => Tools.Messaging.handleReplyMessage db cfg rpcReq
        | "fetch_inbox" => Tools.Messaging.handleFetchInbox db rpcReq
        | "mark_message_read" => Tools.Messaging.handleMarkRead db rpcReq
        | "acknowledge_message" => Tools.Messaging.handleAcknowledge db rpcReq
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

/-- Handle health check requests -/
def handleHealth (_req : ServerRequest) : IO Response := do
  let json := Lean.Json.mkObj [
    ("status", Lean.Json.str "ok"),
    ("version", Lean.Json.str version)
  ]
  pure (Response.json (Lean.Json.compress json))

/-- Create and configure the server with middleware -/
def create (cfg : Config) (db : Storage.Database) (rateLimitState : Middleware.RateLimit.RateLimitState) : Citadel.Server :=
  -- Build base server with routes
  let server := Citadel.Server.create { port := cfg.port, host := cfg.host }
    |>.post "/rpc" (handleRpc db cfg)
    |>.get "/health" handleHealth
    -- Discovery resources
    |>.get "/resource/projects" (Resources.Discovery.handleProjects db)
    |>.get "/resource/project/:slug" (Resources.Discovery.handleProject db)
    |>.get "/resource/agents/:project_key" (Resources.Discovery.handleAgents db)
    |>.get "/resource/identity/:project" (Resources.Discovery.handleIdentity db)
    |>.get "/resource/product/:key" (Resources.Discovery.handleProduct db)
    -- Mail resources
    |>.get "/resource/message/:id" (Resources.Mail.handleMessage db)
    |>.get "/resource/thread/:id" (Resources.Mail.handleThread db)
    |>.get "/resource/inbox/:agent" (Resources.Mail.handleInbox db)
    |>.get "/resource/outbox/:agent" (Resources.Mail.handleOutbox db)
    |>.get "/resource/mailbox/:agent" (Resources.Mail.handleMailbox db)
    -- View resources
    |>.get "/resource/views/urgent-unread/:agent" (Resources.Views.handleUrgentUnread db)
    |>.get "/resource/views/ack-required/:agent" (Resources.Views.handleAckRequired db)
    |>.get "/resource/views/acks-stale/:agent" (Resources.Views.handleAcksStale db)
    |>.get "/resource/views/ack-overdue/:agent" (Resources.Views.handleAckOverdue db)
    -- File reservations
    |>.get "/resource/file_reservations/:slug" (Resources.FileReservations.handleFileReservations db)
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
    |>.use (Middleware.Auth.optionalBearerAuth cfg.http.bearerToken cfg.http.allowLocalhostUnauthenticated)

/-- Run the server (blocking) -/
def run (cfg : Config) (db : Storage.Database) : IO Unit := do
  IO.println s!"Starting agent-mail server v{version}"
  IO.println s!"  Host: {cfg.host}"
  IO.println s!"  Port: {cfg.port}"
  IO.println s!"  Database: {cfg.databasePath}"

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
  IO.println s!"  POST /rpc    - JSON-RPC 2.0 endpoint"
  IO.println s!"  GET  /health - Health check"
  IO.println ""
  IO.println s!"Resources:"
  IO.println s!"  GET  /resource/projects                    - List all projects"
  IO.println s!"  GET  /resource/project/:slug               - Project details"
  IO.println s!"  GET  /resource/agents/:project_key         - Agents in project"
  IO.println s!"  GET  /resource/identity/:project           - Identity resolution"
  IO.println s!"  GET  /resource/product/:key                - Product with projects"
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

  let server := create cfg db rateLimitState
  server.run

end AgentMail.Server
