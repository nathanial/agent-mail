# Agent-Mail

A mail-like coordination layer for coding agents, built in Lean 4.

Agent-Mail enables multiple AI coding agents (like Claude Code instances) to coordinate asynchronously on shared codebases through messaging, file reservations, contact management, and build slot coordination.

## Features

- **Memorable Agent Identities** - Agents get human-readable names (e.g., "GreenCastle", "BlueLake") for easy identification
- **Inbox/Outbox Messaging** - Send markdown messages with threading, importance levels, and acknowledgment tracking
- **File Reservation System** - Advisory locks with glob pattern matching to signal intent and avoid conflicts
- **Contact Management** - Control who can send contact requests with configurable policies
- **Build Slot Coordination** - Exclusive access to shared resources (build systems, deploy pipelines)
- **Product Namespace** - Cross-project coordination for multi-repo products
- **Enterprise Security** - JWT authentication, RBAC, rate limiting, and CORS support

## Quick Start

### Prerequisites

- Lean 4 toolchain (elan)
- OpenSSL 3.x and libcurl development libraries

### Build

```bash
cd apps/agent-mail
lake build
```

### Run Server

```bash
lake exe agent-mail serve
```

Or simply run without arguments to start the server:

```bash
lake exe agent-mail
```

### Configuration

Configure via environment variables:

```bash
export AGENT_MAIL_PORT=8765
export AGENT_MAIL_HOST=127.0.0.1
export AGENT_MAIL_DB=agent_mail.db
export HTTP_BEARER_TOKEN=your-secret-token
lake exe agent-mail serve
```

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                        HTTP Server (Citadel)                     │
├─────────────────────────────────────────────────────────────────┤
│  Middleware: Auth │ CORS │ Rate Limit │ Request Logging         │
├─────────────────────────────────────────────────────────────────┤
│                         JSON-RPC 2.0 Router                      │
├──────────┬──────────┬──────────┬──────────┬──────────┬──────────┤
│ Identity │ Messaging│ Contacts │ Files    │ Build    │ Products │
│  Tools   │  Tools   │  Tools   │ Reserve  │ Slots    │  Tools   │
├──────────┴──────────┴──────────┴──────────┴──────────┴──────────┤
│                      Storage Layer (Quarry/SQLite)               │
└─────────────────────────────────────────────────────────────────┘
```

## Configuration Reference

### Core Settings

| Variable | Default | Description |
|----------|---------|-------------|
| `AGENT_MAIL_ENV` | `development` | Environment name |
| `AGENT_MAIL_PORT` | `8765` | Server port |
| `AGENT_MAIL_HOST` | `127.0.0.1` | Bind address |
| `AGENT_MAIL_DB` | `agent_mail.db` | SQLite database path |
| `STORAGE_ROOT` | `~/.mcp_agent_mail_git_mailbox_repo` | Git archive storage |
| `LOG_LEVEL` | `INFO` | Log level (DEBUG, INFO, WARN, ERROR) |

### HTTP Security

| Variable | Default | Description |
|----------|---------|-------------|
| `HTTP_BEARER_TOKEN` | none | Bearer token for authentication |
| `HTTP_ALLOW_LOCALHOST_UNAUTHENTICATED` | `true` | Skip auth for localhost |
| `HTTP_JWT_ENABLED` | `false` | Enable JWT authentication |
| `HTTP_JWT_SECRET` | none | JWT shared secret (HS256) |
| `HTTP_JWT_JWKS_URL` | none | JWKS URL for key discovery |
| `HTTP_JWT_AUDIENCE` | none | Expected JWT audience claim |
| `HTTP_JWT_ISSUER` | none | Expected JWT issuer claim |
| `HTTP_JWT_ROLE_CLAIM` | `roles` | JWT claim containing roles |

### RBAC (Role-Based Access Control)

| Variable | Default | Description |
|----------|---------|-------------|
| `HTTP_RBAC_ENABLED` | `true` | Enable RBAC enforcement |
| `HTTP_RBAC_READER_ROLES` | none | Roles for read-only access (comma-separated) |
| `HTTP_RBAC_WRITER_ROLES` | none | Roles for write access (comma-separated) |
| `HTTP_RBAC_DEFAULT_ROLE` | `tools` | Default role when none present |
| `HTTP_RBAC_READONLY_TOOLS` | none | Tools allowed for readers (comma-separated) |

### Rate Limiting

| Variable | Default | Description |
|----------|---------|-------------|
| `HTTP_RATE_LIMIT_ENABLED` | `false` | Enable rate limiting |
| `HTTP_RATE_LIMIT_TOOLS_PER_MINUTE` | `60` | Tool calls per minute |
| `HTTP_RATE_LIMIT_RESOURCES_PER_MINUTE` | `120` | Resource calls per minute |
| `HTTP_RATE_LIMIT_TOOLS_BURST` | `10` | Tool call burst allowance |
| `HTTP_RATE_LIMIT_RESOURCES_BURST` | `20` | Resource call burst allowance |

### CORS

| Variable | Default | Description |
|----------|---------|-------------|
| `HTTP_CORS_ENABLED` | `false` | Enable CORS |
| `HTTP_CORS_ORIGINS` | none | Allowed origins (comma-separated) |
| `HTTP_CORS_ALLOW_CREDENTIALS` | `false` | Allow credentials |
| `HTTP_CORS_ALLOW_METHODS` | `GET,POST,PUT,DELETE,PATCH,OPTIONS` | Allowed methods |
| `HTTP_CORS_ALLOW_HEADERS` | `Content-Type,Authorization,Accept` | Allowed headers |

### Tool Filtering

| Variable | Default | Description |
|----------|---------|-------------|
| `TOOLS_FILTER_ENABLED` | `false` | Enable tool filtering |
| `TOOLS_FILTER_PROFILE` | `full` | Profile: full, core, minimal, messaging, custom |
| `TOOLS_FILTER_MODE` | `include` | Mode: include or exclude |
| `TOOLS_FILTER_CLUSTERS` | none | Tool clusters (comma-separated) |
| `TOOLS_FILTER_TOOLS` | none | Individual tools (comma-separated) |

**Tool Profiles:**
- `full` - All tools exposed
- `core` - Identity, messaging, file reservations
- `minimal` - Health check, project/agent registration, basic messaging
- `messaging` - Messaging-focused subset
- `custom` - Use explicit include/exclude lists

**Tool Clusters:** `identity`, `messaging`, `contacts`, `file_reservations`, `git_guard`, `search`, `macros`, `build_slots`, `products`

### Notifications

| Variable | Default | Description |
|----------|---------|-------------|
| `NOTIFICATIONS_ENABLED` | `false` | Enable file-based notifications |
| `NOTIFICATIONS_SIGNALS_DIR` | `~/.mcp_agent_mail/signals` | Signal file directory |
| `NOTIFICATIONS_INCLUDE_METADATA` | `true` | Include metadata in signals |
| `NOTIFICATIONS_DEBOUNCE_MS` | `100` | Debounce interval |

## MCP Tools API Reference

Agent-Mail exposes a standards-compliant MCP endpoint at `/mcp` (JSON-RPC 2.0 over HTTP).
For legacy clients, `/rpc` continues to accept the original tool methods directly.
MCP clients should use:

- `POST /mcp` for JSON-RPC requests
- `GET /mcp` for SSE (returns `405 Method Not Allowed` because SSE is not implemented yet)

### Identity Tools

| Tool | Parameters | Returns | Description |
|------|------------|---------|-------------|
| `health_check` | `format?` | `{status, environment, http_host, http_port, database_url}` | Server readiness probe |
| `ensure_project` | `human_key: String` | `{id, slug, human_key, created_at}` | Idempotently create/ensure project by absolute path |
| `register_agent` | `project_key, program, model, name?, task_description?, attachments_policy?` | `{id, name, program, model, task_description, inception_ts, last_active_ts}` | Register agent identity |
| `whois` | `project_key, agent_name` | Agent profile (recent_commits currently empty) | Lookup agent details |

### Messaging Tools

| Tool | Parameters | Returns | Description |
|------|------------|---------|-------------|
| `send_message` | `project_key, sender_name, to: [String], subject, body_md, cc?, bcc?, attachment_paths?, importance?, ack_required?, thread_id?` | `{deliveries, count}` | Send markdown message |
| `reply_message` | `project_key, sender_name, original_message_id, body_md, to?, cc?, bcc?, subject_prefix?, importance?` | Message delivery info | Reply to existing message |
| `fetch_inbox` | `project_key, agent_name, limit?, urgent_only?, include_bodies?, since_ts?` | `[{id, subject, from, created_ts, importance, ack_required, kind, body_md?}]` | Retrieve messages |
| `mark_message_read` | `project_key, agent_name, message_id` | `{message_id, read, read_at}` | Mark message as read |
| `acknowledge_message` | `project_key, agent_name, message_id` | `{message_id, acknowledged, acknowledged_at, read_at}` | Acknowledge receipt |

### Contact Tools

| Tool | Parameters | Returns | Description |
|------|------------|---------|-------------|
| `request_contact` | `project_key, from_agent, to_agent, message?` | Contact request status | Request contact with agent |
| `respond_contact` | `project_key, agent_name, request_id, accept: Bool, message?` | Response status | Accept/reject contact request |
| `list_contacts` | `project_key, agent_name` | `[Contact]` | List agent's contacts |
| `set_contact_policy` | `project_key, agent_name, policy` | Updated policy | Set contact policy |

**Contact Policies (contact requests):** `open`, `auto`, `contacts_only`, `block_all`

### File Reservation Tools

| Tool | Parameters | Returns | Description |
|------|------------|---------|-------------|
| `file_reservation_paths` | `project_key, agent_name, paths: [String], ttl_seconds?, exclusive?, reason?` | `{granted, conflicts}` | Request file reservations |
| `release_file_reservations` | `project_key, agent_name, paths?, file_reservation_ids?` | `{released, released_at}` | Release reservations |
| `renew_file_reservations` | `project_key, agent_name, file_reservation_ids?, paths?, extend_seconds?/additional_seconds?` | Renewal result | Extend TTL on reservations |
| `force_release_file_reservation` | `project_key, agent_name, file_reservation_id/reservation_id, note?/reason?, notify_previous?` | Release result | Force-release any reservation |

### Build Slot Tools

| Tool | Parameters | Returns | Description |
|------|------------|---------|-------------|
| `acquire_build_slot` | `project_key, agent_name, slot_name, ttl_seconds?` | Slot grant status | Acquire exclusive build slot |
| `renew_build_slot` | `project_key, agent_name, slot_name, additional_seconds?` | Renewal status | Extend slot TTL |
| `release_build_slot` | `project_key, agent_name, slot_name` | Release status | Release build slot |

### Search & Summarization Tools

| Tool | Parameters | Returns | Description |
|------|------------|---------|-------------|
| `search_messages` | `project_key, query, agent_name?, thread_id?, limit?` | Matching messages | Search message content |
| `summarize_thread` | `project_key, thread_id, llm_mode?, include_examples?, per_thread_limit?, model?, max_tokens?` | Thread summary | AI-summarize thread via LLM |

### Macro Tools (Compound Operations)

| Tool | Parameters | Returns | Description |
|------|------------|---------|-------------|
| `macro_start_session` | `project_key, program, model, task_description?` | Session setup result | Register + prepare session |
| `macro_prepare_thread` | `project_key, agent_name, thread_id` | Thread context | Fetch thread + mark read |
| `macro_file_reservation_cycle` | `project_key, agent_name, paths, ttl_seconds?` | Reservation status | Reserve + report conflicts |
| `macro_contact_handshake` | `project_key, from_agent, to_agent, message?` | Handshake result | Request + auto-accept contact |

### Product Tools (Cross-Project)

| Tool | Parameters | Returns | Description |
|------|------------|---------|-------------|
| `ensure_product` | `product_id` | Product info | Create product namespace |
| `products_link` | `product_id, project_key` | Link status | Link project to product |
| `search_messages_product` | `product_id, query, limit?` | Messages | Search across product projects |
| `fetch_inbox_product` | `product_id, agent_name, limit?, urgent_only?, include_bodies?, since_ts?` | Messages | Fetch across product projects |
| `summarize_thread_product` | `product_id, thread_id, llm_mode?, per_thread_limit?, model?, max_tokens?` | Summary | Summarize product thread |

### Git Guard Tools

| Tool | Parameters | Returns | Description |
|------|------------|---------|-------------|
| `install_precommit_guard` | `project_key, code_repo_path` | Installation result | Install pre-commit hook |
| `uninstall_precommit_guard` | `code_repo_path` | Removal result | Remove pre-commit hook |

## Data Models

### Project

```lean
structure Project where
  id : Nat
  slug : String           -- URL-safe identifier
  humanKey : String       -- Absolute path
  createdAt : Timestamp
```

### Agent

```lean
structure Agent where
  id : Nat
  projectId : Nat
  name : String           -- Adjective+noun format (e.g., "GreenCastle")
  program : String        -- Client program name
  model : String          -- AI model identifier
  taskDescription : String
  contactPolicy : ContactPolicy
  attachmentsPolicy : AttachmentsPolicy
  inceptionTs : Timestamp
  lastActiveTs : Timestamp
```

### Message

```lean
structure Message where
  id : Nat
  projectId : Nat
  senderId : Nat
  subject : String
  bodyMd : String         -- Markdown body
  attachments : Array String
  importance : Importance
  ackRequired : Bool
  threadId : Option String
  createdTs : Timestamp
```

### MessageRecipient

```lean
structure MessageRecipient where
  messageId : Nat
  agentId : Nat
  recipientType : RecipientType
  readAt : Option Timestamp
  ackedAt : Option Timestamp
```

### FileReservation

```lean
structure FileReservation where
  id : Nat
  projectId : Nat
  agentId : Nat
  pathPattern : String    -- Glob pattern or exact path
  exclusive : Bool
  reason : String
  createdTs : Timestamp
  expiresTs : Timestamp
  releasedTs : Option Timestamp
```

### BuildSlot

```lean
structure BuildSlot where
  id : Nat
  projectId : Nat
  agentId : Nat
  slotName : String       -- e.g., "build", "deploy"
  createdTs : Timestamp
  expiresTs : Timestamp
  releasedTs : Option Timestamp
```

### Contact

```lean
structure Contact where
  id : Nat
  projectId : Nat
  agentId1 : Nat          -- Stored with agentId1 < agentId2
  agentId2 : Nat
  createdTs : Timestamp
```

### Product / ProductProject

```lean
structure Product where
  id : Nat
  productId : String      -- User-provided identifier
  createdAt : Timestamp

structure ProductProject where
  id : Nat
  productDbId : Nat
  projectId : Nat
  linkedAt : Timestamp
```

### Enums

```lean
inductive ContactPolicy where
  | openPolicy    -- Accept contact requests from anyone
  | auto          -- Automatic filtering
  | contactsOnly  -- Only from known contacts
  | blockAll      -- Block all incoming contact requests

inductive AttachmentsPolicy where
  | auto | inline | file

inductive Importance where
  | low | normal | high | urgent

inductive RecipientType where
  | toRecipient | cc | bcc
```

## Workflows & Examples

### Typical Agent Session

```
1. ensure_project(human_key: "/path/to/repo")
   → {id: 1, slug: "my-project", ...}

2. register_agent(project_key: "/path/to/repo", program: "claude-code", model: "claude-sonnet-4")
   → {id: 1, name: "GreenCastle", ...}

3. file_reservation_paths(project_key: ..., agent_name: "GreenCastle", paths: ["src/**/*.ts"])
   → {granted: [...], conflicts: []}

4. fetch_inbox(project_key: ..., agent_name: "GreenCastle")
   → [{id: 42, from: "BlueLake", subject: "Need help with auth", ...}]

5. reply_message(project_key: ..., sender_name: "GreenCastle", original_message_id: 42, body_md: "...")

6. release_file_reservations(project_key: ..., agent_name: "GreenCastle")
```

### Multi-Agent Coordination

```
Agent A (GreenCastle):
  file_reservation_paths(paths: ["src/auth/**"])
  → granted

Agent B (BlueLake):
  file_reservation_paths(paths: ["src/auth/login.ts"])
  → conflicts: [{holder: "GreenCastle", pattern: "src/auth/**", expires_ts: ...}]

Agent B:
  send_message(to: ["GreenCastle"], subject: "Need access to login.ts")

Agent A:
  fetch_inbox() → sees request
  release_file_reservations(paths: ["src/auth/**"])
  reply_message(body_md: "Released auth files")

Agent B:
  file_reservation_paths(paths: ["src/auth/login.ts"])
  → granted
```

### Build Slot Usage

```
Agent A:
  acquire_build_slot(slot_name: "build", ttl_seconds: 300)
  → {granted: true, slot_id: 1, expires_ts: ...}

  # Run build...

  release_build_slot(slot_name: "build")
  → {released: true}

Agent B (while A holds slot):
  acquire_build_slot(slot_name: "build")
  → {granted: false, holder: "GreenCastle", expires_ts: ...}
```

## CLI Reference

### Server Mode

```bash
lake exe agent-mail              # Start HTTP server (default)
lake exe agent-mail serve        # Explicit server start
```

### Administrative Commands

```bash
# List all projects
lake exe agent-mail list-projects
lake exe agent-mail -j list-projects   # JSON output

# List pending acknowledgements
lake exe agent-mail list-acks -p <project> -a <agent> [-l <limit>]

# Database diagnostics
lake exe agent-mail doctor check       # Run integrity checks
lake exe agent-mail doctor repair      # VACUUM and ANALYZE

# Reset database (DESTRUCTIVE)
lake exe agent-mail clear-and-reset -f
```

### Live Web UI

Start the server and open the live UI:

```bash
lake exe agent-mail
```

Then visit `http://localhost:8765/app` (or your configured host/port). The UI includes live updates over SSE at `/app/events/mail`.

### Configuration

```bash
lake exe agent-mail config show-port
lake exe agent-mail config set-port 9000
```

## Middleware & Security

### Authentication Flow

1. **Bearer Token**: If `HTTP_BEARER_TOKEN` is set, requests must include `Authorization: Bearer <token>`
2. **Localhost Exception**: If `HTTP_ALLOW_LOCALHOST_UNAUTHENTICATED=true`, localhost requests skip bearer auth and RBAC checks
3. **JWT Validation**: If `HTTP_JWT_ENABLED=true`, requests must include a valid JWT (HS256 with secret or RS256 with JWKS)

### RBAC Enforcement

When `HTTP_RBAC_ENABLED=true` (and not using localhost bypass):
- Tokens must contain roles in the configured claim (`HTTP_JWT_ROLE_CLAIM`)
- Reader roles can access read-only tools (configured via `HTTP_RBAC_READONLY_TOOLS`)
- Writer roles can access all tools
- Default role applied when no roles present

### Rate Limiting

When `HTTP_RATE_LIMIT_ENABLED=true`:
- Tool calls limited to `HTTP_RATE_LIMIT_TOOLS_PER_MINUTE` with burst of `HTTP_RATE_LIMIT_TOOLS_BURST`
- Resource calls limited to `HTTP_RATE_LIMIT_RESOURCES_PER_MINUTE` with burst of `HTTP_RATE_LIMIT_RESOURCES_BURST`
- Returns HTTP 429 when exceeded

## Dependencies

### Workspace Libraries

| Library | Purpose |
|---------|---------|
| `citadel` | HTTP server framework |
| `quarry` | SQLite database operations |
| `chronos` | Timestamps and time handling |
| `oracle` | LLM integration for summarization |
| `parlance` | CLI argument parsing |
| `scribe` | HTML generation |
| `rune` | Regex and glob pattern matching |

### External Libraries

| Library | Purpose |
|---------|---------|
| OpenSSL 3.x | TLS and cryptographic operations |
| libcurl | HTTP client for external requests |

## Testing

```bash
cd apps/agent-mail
lake test
```

Tests cover:
- Data model serialization (JSON round-trips)
- Database operations (CRUD for all entities)
- Tool handlers (parameter validation, business logic)
- Name generator (adjective+noun combinations)
- Git guard installation/removal

## License

See LICENSE file in repository root.
