/-
  AgentMail.ToolFilter - Tool filtering based on profiles
-/

namespace AgentMail.ToolFilter

/-- Tool profile presets -/
inductive ToolProfile where
  /-- All tools exposed -/
  | full
  /-- Essential tools: identity, messaging, file_reservations -/
  | core
  /-- Bare minimum: identity, messaging basics -/
  | minimal
  /-- Messaging-focused subset -/
  | messaging
  /-- Use explicit include/exclude lists -/
  | custom
  deriving Repr, BEq, Inhabited

namespace ToolProfile

/-- Parse tool profile from string -/
def fromString (s : String) : ToolProfile :=
  match s.toLower with
  | "full" => .full
  | "core" => .core
  | "minimal" => .minimal
  | "messaging" => .messaging
  | "custom" => .custom
  | _ => .full

/-- Convert tool profile to string -/
def toString : ToolProfile → String
  | .full => "full"
  | .core => "core"
  | .minimal => "minimal"
  | .messaging => "messaging"
  | .custom => "custom"

instance : ToString ToolProfile where
  toString := ToolProfile.toString

end ToolProfile

/-- Tool filter configuration -/
structure ToolFilterConfig where
  /-- Whether tool filtering is enabled -/
  enabled : Bool := false
  /-- Tool profile to use -/
  profile : ToolProfile := .full
  /-- Filter mode: "include" or "exclude" -/
  mode : String := "include"
  /-- Tool clusters to include/exclude -/
  clusters : List String := []
  /-- Individual tools to include/exclude -/
  tools : List String := []
  deriving Repr, Inhabited

/-- Core tools: identity, messaging, file_reservations -/
def coreTools : List String := [
  "health_check",
  "ensure_project",
  "register_agent",
  "whois",
  "send_message",
  "reply_message",
  "fetch_inbox",
  "mark_message_read",
  "acknowledge_message",
  "file_reservation_paths",
  "release_file_reservations",
  "renew_file_reservations"
]

/-- Minimal tools: identity and basic messaging -/
def minimalTools : List String := [
  "health_check",
  "ensure_project",
  "register_agent",
  "send_message",
  "fetch_inbox"
]

/-- Messaging-focused tools -/
def messagingTools : List String := [
  "send_message",
  "reply_message",
  "fetch_inbox",
  "mark_message_read",
  "acknowledge_message",
  "search_messages",
  "summarize_thread"
]

/-- All available tools -/
def allTools : List String := [
  -- Identity
  "health_check",
  "ensure_project",
  "register_agent",
  "whois",
  -- Messaging
  "send_message",
  "reply_message",
  "fetch_inbox",
  "mark_message_read",
  "acknowledge_message",
  -- Contacts
  "request_contact",
  "respond_contact",
  "list_contacts",
  "set_contact_policy",
  -- File reservations
  "file_reservation_paths",
  "release_file_reservations",
  "renew_file_reservations",
  "force_release_file_reservation",
  -- Git guard
  "install_precommit_guard",
  "uninstall_precommit_guard",
  -- Search
  "search_messages",
  "summarize_thread",
  -- Macros
  "macro_start_session",
  "macro_prepare_thread",
  "macro_file_reservation_cycle",
  "macro_contact_handshake",
  -- Build slots
  "acquire_build_slot",
  "renew_build_slot",
  "release_build_slot",
  -- Products
  "ensure_product",
  "products_link",
  "search_messages_product",
  "fetch_inbox_product",
  "summarize_thread_product"
]

/-- Tool clusters for grouping -/
def toolClusters : List (String × List String) := [
  ("identity", ["health_check", "ensure_project", "register_agent", "whois"]),
  ("messaging", ["send_message", "reply_message", "fetch_inbox", "mark_message_read", "acknowledge_message"]),
  ("contacts", ["request_contact", "respond_contact", "list_contacts", "set_contact_policy"]),
  ("file_reservations", ["file_reservation_paths", "release_file_reservations", "renew_file_reservations", "force_release_file_reservation"]),
  ("git_guard", ["install_precommit_guard", "uninstall_precommit_guard"]),
  ("search", ["search_messages", "summarize_thread"]),
  ("macros", ["macro_start_session", "macro_prepare_thread", "macro_file_reservation_cycle", "macro_contact_handshake"]),
  ("build_slots", ["acquire_build_slot", "renew_build_slot", "release_build_slot"]),
  ("products", ["ensure_product", "products_link", "search_messages_product", "fetch_inbox_product", "summarize_thread_product"])
]

/-- Get tools for a cluster name -/
def getClusterTools (cluster : String) : List String :=
  toolClusters.find? (fun (name, _) => name == cluster)
    |>.map (fun (_, tools) => tools)
    |>.getD []

/-- Get tools for a profile -/
def getProfileTools (profile : ToolProfile) : List String :=
  match profile with
  | .full => allTools
  | .core => coreTools
  | .minimal => minimalTools
  | .messaging => messagingTools
  | .custom => allTools  -- Custom uses explicit lists

/-- Filter tools based on configuration -/
def filterTools (config : ToolFilterConfig) (tools : List String) : List String :=
  if !config.enabled then
    tools
  else
    -- Get base tools from profile
    let baseTool := getProfileTools config.profile

    -- Get tools from clusters
    let clusterTools := config.clusters.foldl (fun acc cluster => acc ++ getClusterTools cluster) []

    -- Combine explicit tools
    let explicitTools := config.tools

    -- Apply include/exclude mode
    let filterSet := clusterTools ++ explicitTools

    match config.mode with
    | "include" =>
      if config.profile == .custom then
        -- Custom profile: only include explicitly listed
        tools.filter fun t => filterSet.contains t
      else
        -- Non-custom: start with profile, add includes
        tools.filter fun t => baseTool.contains t || filterSet.contains t
    | "exclude" =>
      -- Exclude mode: remove listed from profile
      tools.filter fun t => baseTool.contains t && !filterSet.contains t
    | _ =>
      -- Default to profile tools
      tools.filter fun t => baseTool.contains t

/-- Check if a tool is allowed -/
def isToolAllowed (config : ToolFilterConfig) (tool : String) : Bool :=
  let filtered := filterTools config [tool]
  !filtered.isEmpty

end AgentMail.ToolFilter
