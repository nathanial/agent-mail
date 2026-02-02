/-
  AgentMail.Mcp - MCP protocol helpers for tool listing and responses
-/
import Lean.Data.Json

namespace AgentMail.Mcp

/-- MCP protocol version supported by this server. -/
def protocolVersion : String := "2025-03-26"

/-- MCP tool specification. -/
structure ToolSpec where
  name : String
  description : String
  inputSchema : Lean.Json

namespace ToolSpec

/-- Convert a tool spec to MCP JSON. -/
def toJson (tool : ToolSpec) : Lean.Json :=
  Lean.Json.mkObj [
    ("name", Lean.Json.str tool.name),
    ("description", Lean.Json.str tool.description),
    ("inputSchema", tool.inputSchema)
  ]

end ToolSpec

/-- Generic input schema that accepts any object. -/
def inputSchemaAny : Lean.Json :=
  Lean.Json.mkObj [
    ("type", Lean.Json.str "object"),
    ("properties", Lean.Json.mkObj []),
    ("additionalProperties", Lean.Json.bool true)
  ]

/-- All MCP tool specs exposed by agent-mail. -/
def allTools : List ToolSpec := [
  { name := "health_check", description := "Server readiness probe.", inputSchema := inputSchemaAny },
  { name := "ensure_project", description := "Idempotently create or fetch a project by absolute path.", inputSchema := inputSchemaAny },
  { name := "register_agent", description := "Register an agent identity for a project.", inputSchema := inputSchemaAny },
  { name := "whois", description := "Look up agent details by name.", inputSchema := inputSchemaAny },
  { name := "send_message", description := "Send a markdown message to one or more agents.", inputSchema := inputSchemaAny },
  { name := "reply_message", description := "Reply to an existing message thread.", inputSchema := inputSchemaAny },
  { name := "fetch_inbox", description := "Fetch recent inbox messages.", inputSchema := inputSchemaAny },
  { name := "mark_message_read", description := "Mark a message as read.", inputSchema := inputSchemaAny },
  { name := "acknowledge_message", description := "Acknowledge receipt of a message.", inputSchema := inputSchemaAny },
  { name := "request_contact", description := "Request contact with another agent.", inputSchema := inputSchemaAny },
  { name := "respond_contact", description := "Accept or reject a contact request.", inputSchema := inputSchemaAny },
  { name := "list_contacts", description := "List current contacts for an agent.", inputSchema := inputSchemaAny },
  { name := "set_contact_policy", description := "Set the contact request policy.", inputSchema := inputSchemaAny },
  { name := "file_reservation_paths", description := "Reserve files or globs for exclusive use.", inputSchema := inputSchemaAny },
  { name := "release_file_reservations", description := "Release existing file reservations.", inputSchema := inputSchemaAny },
  { name := "renew_file_reservations", description := "Extend file reservation TTLs.", inputSchema := inputSchemaAny },
  { name := "force_release_file_reservation", description := "Force-release another agent's reservation.", inputSchema := inputSchemaAny },
  { name := "install_precommit_guard", description := "Install the git pre-commit guard hook.", inputSchema := inputSchemaAny },
  { name := "uninstall_precommit_guard", description := "Remove the git pre-commit guard hook.", inputSchema := inputSchemaAny },
  { name := "search_messages", description := "Search messages by query.", inputSchema := inputSchemaAny },
  { name := "summarize_thread", description := "Summarize a message thread.", inputSchema := inputSchemaAny },
  { name := "macro_start_session", description := "Register and prepare a session (macro).", inputSchema := inputSchemaAny },
  { name := "macro_prepare_thread", description := "Fetch thread and mark read (macro).", inputSchema := inputSchemaAny },
  { name := "macro_file_reservation_cycle", description := "Reserve files and report conflicts (macro).", inputSchema := inputSchemaAny },
  { name := "macro_contact_handshake", description := "Request + auto-accept contact (macro).", inputSchema := inputSchemaAny },
  { name := "acquire_build_slot", description := "Acquire an exclusive build slot.", inputSchema := inputSchemaAny },
  { name := "renew_build_slot", description := "Extend a build slot TTL.", inputSchema := inputSchemaAny },
  { name := "release_build_slot", description := "Release a build slot.", inputSchema := inputSchemaAny },
  { name := "ensure_product", description := "Create or fetch a product namespace.", inputSchema := inputSchemaAny },
  { name := "products_link", description := "Link a project to a product.", inputSchema := inputSchemaAny },
  { name := "search_messages_product", description := "Search messages across product projects.", inputSchema := inputSchemaAny },
  { name := "fetch_inbox_product", description := "Fetch inbox messages across product projects.", inputSchema := inputSchemaAny },
  { name := "summarize_thread_product", description := "Summarize a product thread.", inputSchema := inputSchemaAny }
]

/-- MCP initialize result payload. -/
def initializeResult (serverName serverVersion : String) : Lean.Json :=
  Lean.Json.mkObj [
    ("protocolVersion", Lean.Json.str protocolVersion),
    ("capabilities", Lean.Json.mkObj [
      ("tools", Lean.Json.mkObj [
        ("listChanged", Lean.Json.bool false)
      ])
    ]),
    ("serverInfo", Lean.Json.mkObj [
      ("name", Lean.Json.str serverName),
      ("version", Lean.Json.str serverVersion)
    ])
  ]

/-- Convert a JSON payload to text for MCP tool results. -/
def jsonToText (payload : Lean.Json) : String :=
  match payload.getStr? with
  | Except.ok s => s
  | Except.error _ => Lean.Json.compress payload

/-- MCP callTool result payload. -/
def callToolResult (payload : Lean.Json) (isError : Bool := false) : Lean.Json :=
  Lean.Json.mkObj [
    ("content", Lean.Json.arr #[
      Lean.Json.mkObj [
        ("type", Lean.Json.str "text"),
        ("text", Lean.Json.str (jsonToText payload))
      ]
    ]),
    ("isError", Lean.Json.bool isError)
  ]

end AgentMail.Mcp
