# Agent Mail - Lean 4 MCP Server

A mail-like coordination layer for coding agents, ported from Python to Lean 4.

## Project Purpose

Enable multiple coding agents to coordinate asynchronously via:
- Memorable identities (e.g., "GreenCastle", "BlueLake")
- Inbox/outbox messaging with markdown support
- Searchable threads and conversation history
- Advisory file reservations to signal intent and avoid conflicts

## MCP Tools API Specification

### Setup & Identity Tools

| Tool | Parameters | Returns | Description |
|------|------------|---------|-------------|
| `health_check` | `format?` | `{status, environment, http_host, http_port, database_url}` | Server readiness probe |
| `ensure_project` | `human_key: String` (absolute path) | `{id, slug, human_key, created_at}` | Idempotently create/ensure project |
| `register_agent` | `project_key, program, model, name?, task_description?, attachments_policy?` | `{id, name, program, model, task_description, inception_ts, last_active_ts}` | Register agent identity |
| `whois` | `project_key, agent_name, include_recent_commits?, commit_limit?` | Agent profile + optional commits | Lookup agent details |
| `create_agent_identity` | `project_key, name?, program?, model?` | Agent identity | Create new identity |

### Messaging Tools

| Tool | Parameters | Returns | Description |
|------|------------|---------|-------------|
| `send_message` | `project_key, sender_name, to: [String], subject, body_md, cc?, bcc?, attachment_paths?, importance?, ack_required?, thread_id?` | `{deliveries, count}` | Send markdown message |
| `reply_message` | `project_key, sender_name, original_message_id, body_md, cc?, bcc?` | Message delivery info | Reply to existing message |
| `fetch_inbox` | `project_key, agent_name, limit?, urgent_only?, include_bodies?, since_ts?` | `[{id, subject, from, created_ts, importance, ack_required, kind, body_md?}]` | Retrieve messages |
| `mark_message_read` | `project_key, agent_name, message_id` | `{message_id, read, read_at}` | Mark message as read |
| `acknowledge_message` | `project_key, agent_name, message_id` | Ack confirmation | Acknowledge receipt |
| `search_messages` | `project_key, query, agent_name?, thread_id?, limit?` | Matching messages | Search message content |
| `summarize_thread` | `project_key, thread_id, model?, max_tokens?` | Thread summary | AI-summarize thread |

### Contact Tools

| Tool | Parameters | Returns | Description |
|------|------------|---------|-------------|
| `request_contact` | `project_key, from_agent, to_agent, message?` | Contact request status | Request contact with agent |
| `respond_contact` | `project_key, agent_name, request_id, accept: Bool, message?` | Response status | Accept/reject contact request |
| `list_contacts` | `project_key, agent_name` | `[Contact]` | List agent's contacts |
| `set_contact_policy` | `project_key, agent_name, policy: "open" \| "auto" \| "contacts_only" \| "block_all"` | Updated policy | Set contact policy |

### File Reservation Tools

| Tool | Parameters | Returns | Description |
|------|------------|---------|-------------|
| `file_reservation_paths` | `project_key, agent_name, paths: [String], ttl_seconds?, exclusive?, reason?` | `{granted, conflicts}` | Request file reservations |
| `release_file_reservations` | `project_key, agent_name, paths?, file_reservation_ids?` | `{released, released_at}` | Release reservations |
| `force_release_file_reservation` | `project_key, reservation_id, reason` | Release result | Force-release any reservation |
| `renew_file_reservations` | `project_key, agent_name, file_reservation_ids, additional_seconds?` | Renewal result | Extend TTL on reservations |

### Pre-commit Guard Tools

| Tool | Parameters | Returns | Description |
|------|------------|---------|-------------|
| `install_precommit_guard` | `code_repo_path` | Installation result | Install guard hook |
| `uninstall_precommit_guard` | `code_repo_path` | Removal result | Remove guard hook |

### Macro Tools (Compound Operations)

| Tool | Parameters | Returns | Description |
|------|------------|---------|-------------|
| `macro_start_session` | `project_key, program, model, task_description?` | Session setup result | Register + prepare session |
| `macro_prepare_thread` | `project_key, agent_name, thread_id` | Thread context | Fetch thread + mark read |
| `macro_file_reservation_cycle` | `project_key, agent_name, paths, ttl_seconds?` | Reservation status | Reserve + report conflicts |
| `macro_contact_handshake` | `project_key, from_agent, to_agent, message?` | Handshake result | Request + auto-accept contact |

### Build Slot Tools (Optional)

| Tool | Parameters | Returns | Description |
|------|------------|---------|-------------|
| `acquire_build_slot` | `project_key, agent_name, slot_name, ttl_seconds?` | Slot grant status | Acquire exclusive build slot |
| `renew_build_slot` | `project_key, agent_name, slot_name, additional_seconds?` | Renewal status | Extend slot TTL |
| `release_build_slot` | `project_key, agent_name, slot_name` | Release status | Release build slot |

### Product Tools (Optional, Cross-Project)

| Tool | Parameters | Returns | Description |
|------|------------|---------|-------------|
| `ensure_product` | `product_id` | Product info | Create product namespace |
| `products_link` | `product_id, project_key` | Link status | Link project to product |
| `search_messages_product` | `product_id, query, limit?` | Messages | Search across product projects |
| `fetch_inbox_product` | `product_id, agent_name, limit?` | Messages | Fetch across product projects |
| `summarize_thread_product` | `product_id, thread_id` | Summary | Summarize product thread |

## Implementation Phases

### Phase 1: Core Infrastructure
- [ ] Data models (Project, Agent, Message, FileReservation)
- [ ] SQLite storage via `quarry`
- [ ] JSON serialization/deserialization
- [ ] HTTP server foundation (MCP protocol)

### Phase 2: Identity & Projects
- [ ] `health_check`
- [ ] `ensure_project`
- [ ] `register_agent`
- [ ] `whois`
- [ ] Adjective+noun name generator

### Phase 3: Messaging
- [ ] `send_message`
- [ ] `reply_message`
- [ ] `fetch_inbox`
- [ ] `mark_message_read`
- [ ] `acknowledge_message`
- [ ] Thread management

### Phase 4: Contacts
- [ ] `request_contact`
- [ ] `respond_contact`
- [ ] `list_contacts`
- [ ] `set_contact_policy`

### Phase 5: File Reservations
- [ ] `file_reservation_paths`
- [ ] `release_file_reservations`
- [ ] `renew_file_reservations`
- [ ] `force_release_file_reservation`
- [ ] Glob pattern matching for conflicts

### Phase 6: Git Integration
- [ ] Git archive for message artifacts
- [ ] Pre-commit guard installation
- [ ] Audit trail artifacts

### Phase 7: Search & Summarization
- [ ] `search_messages`
- [ ] `summarize_thread` (LLM integration)

### Phase 8: Macros
- [ ] `macro_start_session`
- [ ] `macro_prepare_thread`
- [ ] `macro_file_reservation_cycle`
- [ ] `macro_contact_handshake`

### Phase 9: Build Slots
- [ ] `acquire_build_slot`
- [ ] `renew_build_slot`
- [ ] `release_build_slot`

### Phase 10: Product Namespace (Cross-Project)
- [ ] `ensure_product`
- [ ] `products_link`
- [ ] `search_messages_product`
- [ ] `fetch_inbox_product`
- [ ] `summarize_thread_product`

## Transport

**HTTP server via citadel** - The MCP server will use the workspace's `citadel` HTTP library for transport, exposing tools via JSON-RPC 2.0 over HTTP with bearer token authentication.

## Dependencies

Required workspace libraries:
- `quarry` - SQLite database
- `citadel` - HTTP server (primary transport)
- `herald` - HTTP parsing
- `rune` - Regex/glob pattern matching
- `chronos` - Timestamps and TTL
- `totem` - TOML config (optional)
- `collimator` - Optics for data access
- `oracle` - LLM integration for summarization

## Data Models

```lean
structure Project where
  id : Nat
  slug : String
  humanKey : String  -- absolute path
  createdAt : Timestamp
  deriving Repr, ToJson, FromJson

structure Agent where
  id : Nat
  projectId : Nat
  name : String  -- adjective+noun format
  program : String
  model : String
  taskDescription : String
  contactPolicy : ContactPolicy
  attachmentsPolicy : AttachmentsPolicy
  inceptionTs : Timestamp
  lastActiveTs : Timestamp
  deriving Repr, ToJson, FromJson

inductive ContactPolicy
  | open | auto | contactsOnly | blockAll

inductive Importance
  | low | normal | high | urgent

structure Message where
  id : Nat
  projectId : Nat
  senderId : Nat
  subject : String
  bodyMd : String
  importance : Importance
  ackRequired : Bool
  threadId : Option String
  createdTs : Timestamp
  deriving Repr, ToJson, FromJson

structure FileReservation where
  id : Nat
  projectId : Nat
  agentId : Nat
  pathPattern : String
  exclusive : Bool
  reason : String
  expiresTs : Timestamp
  releasedTs : Option Timestamp
  deriving Repr, ToJson, FromJson
```

## MCP Protocol Notes

The server exposes tools via the Model Context Protocol:
- JSON-RPC 2.0 over HTTP
- Bearer token authentication
- Tools registered with `@mcp.tool()` equivalent
- Context object for logging (`ctx.info()`)

## File Structure

```
apps/agent-mail/
├── AgentMail/
│   ├── Main.lean          -- Entry point
│   ├── Server.lean        -- MCP server setup
│   ├── Models/
│   │   ├── Project.lean
│   │   ├── Agent.lean
│   │   ├── Message.lean
│   │   └── FileReservation.lean
│   ├── Storage/
│   │   ├── Database.lean  -- SQLite operations
│   │   └── Archive.lean   -- Git artifact storage
│   ├── Tools/
│   │   ├── Health.lean
│   │   ├── Projects.lean
│   │   ├── Agents.lean
│   │   ├── Messaging.lean
│   │   ├── Contacts.lean
│   │   ├── FileReservations.lean
│   │   └── Macros.lean
│   └── Utils/
│       ├── NameGenerator.lean
│       └── GlobMatcher.lean
├── Tests/
│   └── ...
├── lakefile.lean
└── ROADMAP.md
```
