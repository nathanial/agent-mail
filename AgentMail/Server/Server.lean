/-
  AgentMail.Server - HTTP server for agent-mail MCP
-/
import Citadel
import AgentMail.Config
import AgentMail.Protocol.JsonRpc
import AgentMail.Storage.Database
import AgentMail.Tools.Identity
import AgentMail.Tools.Messaging
import AgentMail.Tools.Contacts
import AgentMail.Tools.FileReservations
import AgentMail.Tools.GitGuard
import AgentMail.Tools.Search
import AgentMail.Tools.Macros

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

/-- Create and configure the server -/
def create (cfg : Config) (db : Storage.Database) : Citadel.Server :=
  Citadel.Server.create { port := cfg.port, host := cfg.host }
    |>.post "/rpc" (handleRpc db cfg)
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
