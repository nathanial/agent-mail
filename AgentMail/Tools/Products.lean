/-
  AgentMail.Tools.Products - Product namespace MCP tool handlers
-/
import Chronos
import Citadel
import Oracle
import AgentMail.Config
import AgentMail.Protocol.JsonRpc
import AgentMail.Storage.Database
import AgentMail.Storage.Archive
import AgentMail.Tools.Identity
import AgentMail.Tools.Search

open Citadel

namespace AgentMail.Tools.Products

-- =============================================================================
-- Helper Functions
-- =============================================================================

/-- Resolve a product by its string ID -/
def resolveProduct (db : Storage.Database) (productId : String) : IO (Option AgentMail.Product) :=
  db.queryProductByProductId productId

/-- Parse since_ts value from ISO 8601 or integer seconds. -/
def parseSinceTs (raw : String) : IO (Option Int) := do
  match raw.toInt? with
  | some n => pure (some n)
  | none =>
    match Chronos.DateTime.parseIso8601 raw with
    | Except.ok dt =>
      let ts ← Chronos.DateTime.toTimestamp dt
      pure (some ts.seconds)
    | Except.error _ => pure none

-- =============================================================================
-- Handlers
-- =============================================================================

/-- Handle ensure_product request - idempotently create a product namespace -/
def handleEnsureProduct (db : Storage.Database) (_cfg : Config) (req : JsonRpc.Request) : IO Response := do
  let params := req.params.getD Lean.Json.null

  -- Extract required param
  let productId ← match params.getObjValAs? String "product_id" with
    | Except.ok p => pure p
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: product_id")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Check if product already exists
  match ← resolveProduct db productId with
  | some existing =>
    -- Return existing product
    let result := Lean.Json.mkObj [
      ("id", Lean.Json.num existing.id),
      ("product_id", Lean.Json.str existing.productId),
      ("created_at", Lean.Json.num existing.createdAt.seconds)
    ]
    let resp := JsonRpc.Response.success req.id result
    pure (Response.json (Lean.Json.compress (Lean.toJson resp)))
  | none =>
    -- Create new product
    let now ← Chronos.Timestamp.now
    let id ← db.insertProduct productId now
    let result := Lean.Json.mkObj [
      ("id", Lean.Json.num id),
      ("product_id", Lean.Json.str productId),
      ("created_at", Lean.Json.num now.seconds)
    ]
    let resp := JsonRpc.Response.success req.id result
    pure (Response.json (Lean.Json.compress (Lean.toJson resp)))

/-- Handle products_link request - link a project to a product -/
def handleProductsLink (db : Storage.Database) (_cfg : Config) (req : JsonRpc.Request) : IO Response := do
  let params := req.params.getD Lean.Json.null

  -- Extract required params
  let productId ← match params.getObjValAs? String "product_id" with
    | Except.ok p => pure p
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: product_id")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  let projectKey ← match params.getObjValAs? String "project_key" with
    | Except.ok k => pure k
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: project_key")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Resolve product
  let product ← match ← resolveProduct db productId with
    | some p => pure p
    | none =>
      let err := JsonRpc.Error.invalidParams (some s!"product not found: {productId}")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Resolve project
  let project ← match ← Identity.resolveProject db projectKey with
    | some p => pure p
    | none =>
      let err := JsonRpc.Error.invalidParams (some s!"project not found: {projectKey}")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Check if link already exists
  match ← db.queryProductProjectLink product.id project.id with
  | some existing =>
    -- Return success for idempotency
    let result := Lean.Json.mkObj [
      ("linked", Lean.Json.bool true),
      ("product_id", Lean.Json.str product.productId),
      ("project_id", Lean.Json.num project.id),
      ("project_slug", Lean.Json.str project.slug),
      ("linked_at", Lean.Json.num existing.linkedAt.seconds)
    ]
    let resp := JsonRpc.Response.success req.id result
    pure (Response.json (Lean.Json.compress (Lean.toJson resp)))
  | none =>
    -- Create link
    let now ← Chronos.Timestamp.now
    let _ ← db.insertProductProject product.id project.id now
    let result := Lean.Json.mkObj [
      ("linked", Lean.Json.bool true),
      ("product_id", Lean.Json.str product.productId),
      ("project_id", Lean.Json.num project.id),
      ("project_slug", Lean.Json.str project.slug),
      ("linked_at", Lean.Json.num now.seconds)
    ]
    let resp := JsonRpc.Response.success req.id result
    pure (Response.json (Lean.Json.compress (Lean.toJson resp)))

/-- Handle search_messages_product request - search across all product projects -/
def handleSearchMessagesProduct (db : Storage.Database) (_cfg : Config) (req : JsonRpc.Request) : IO Response := do
  let params := req.params.getD Lean.Json.null

  -- Extract required params
  let productId ← match params.getObjValAs? String "product_id" with
    | Except.ok p => pure p
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: product_id")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  let query ← match params.getObjValAs? String "query" with
    | Except.ok q => pure q
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: query")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Optional params
  let limit := match params.getObjValAs? Nat "limit" with
    | Except.ok n => n
    | Except.error _ => 20

  let threadIdOpt := match params.getObjValAs? String "thread_id" with
    | Except.ok t => some t
    | Except.error _ => none

  -- Resolve product
  let product ← match ← resolveProduct db productId with
    | some p => pure p
    | none =>
      let err := JsonRpc.Error.invalidParams (some s!"product not found: {productId}")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Get all projects for this product
  let projects ← db.queryProjectsByProduct product.id

  -- Search across all projects
  let mut allResults : Array Storage.Database.SearchResultWithProject := #[]
  for project in projects do
    let results ← db.searchMessages project.id query limit threadIdOpt none
    let enriched := results.map fun r => {
      id := r.id
      subject := r.subject
      importance := r.importance
      ackRequired := r.ackRequired
      createdTs := r.createdTs
      threadId := r.threadId
      senderName := r.senderName
      projectId := project.id
      projectSlug := project.slug
      projectKey := project.humanKey
    }
    allResults := allResults ++ enriched

  -- Sort by created_ts DESC and take limit
  let sorted := allResults.qsort (fun a b => a.createdTs.seconds > b.createdTs.seconds)
  let limited := (sorted.toSubarray 0 (min limit sorted.size)).toArray

  -- Convert to JSON
  let resultsJson := limited.map fun r =>
    Lean.Json.mkObj [
      ("id", Lean.Json.num r.id),
      ("subject", Lean.Json.str r.subject),
      ("importance", Lean.toJson r.importance),
      ("ack_required", Lean.Json.bool r.ackRequired),
      ("created_ts", Lean.Json.num r.createdTs.seconds),
      ("thread_id", match r.threadId with | some t => Lean.Json.str t | none => Lean.Json.null),
      ("from", Lean.Json.str r.senderName),
      ("project_id", Lean.Json.num r.projectId),
      ("project_slug", Lean.Json.str r.projectSlug),
      ("project_key", Lean.Json.str r.projectKey)
    ]

  let resp := JsonRpc.Response.success req.id (Lean.Json.arr resultsJson)
  pure (Response.json (Lean.Json.compress (Lean.toJson resp)))

/-- Handle fetch_inbox_product request - fetch inbox across all product projects -/
def handleFetchInboxProduct (db : Storage.Database) (_cfg : Config) (req : JsonRpc.Request) : IO Response := do
  let params := req.params.getD Lean.Json.null

  -- Extract required params
  let productId ← match params.getObjValAs? String "product_id" with
    | Except.ok p => pure p
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: product_id")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  let agentName ← match params.getObjValAs? String "agent_name" with
    | Except.ok n => pure n
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: agent_name")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Optional params
  let limit := match params.getObjValAs? Nat "limit" with
    | Except.ok n => n
    | Except.error _ => 50

  let urgentOnly := match params.getObjValAs? Bool "urgent_only" with
    | Except.ok b => b
    | Except.error _ => false

  let includeBodies := match params.getObjValAs? Bool "include_bodies" with
    | Except.ok b => b
    | Except.error _ => false

  let sinceTs ← match params.getObjValAs? String "since_ts" with
    | Except.ok ts => parseSinceTs ts
    | Except.error _ =>
      match params.getObjValAs? Int "since_ts" with
      | Except.ok ts => pure (some ts)
      | Except.error _ => pure none

  -- Resolve product
  let product ← match ← resolveProduct db productId with
    | some p => pure p
    | none =>
      let err := JsonRpc.Error.invalidParams (some s!"product not found: {productId}")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Get all projects for this product
  let projects ← db.queryProjectsByProduct product.id

  -- Fetch inbox across all projects for agents with matching name
  let mut allEntries : Array Storage.Database.InboxEntryWithProject := #[]
  for project in projects do
    -- Try to find agent with matching name in this project
    match ← db.queryAgentByName project.id agentName with
    | none => continue  -- No agent with this name in this project
    | some agent =>
      let entries ← db.queryInbox project.id agent.id limit urgentOnly sinceTs
      let enriched := entries.map fun e => {
        id := e.id
        senderName := e.senderName
        subject := e.subject
        importance := e.importance
        ackRequired := e.ackRequired
        threadId := e.threadId
        createdTs := e.createdTs
        readAt := e.readAt
        ackedAt := e.ackedAt
        bodyMd := e.bodyMd
        recipientType := e.recipientType
        projectId := project.id
        projectSlug := project.slug
        projectKey := project.humanKey
      }
      allEntries := allEntries ++ enriched

  -- Sort by created_ts DESC and take limit
  let sorted := allEntries.qsort (fun a b => a.createdTs.seconds > b.createdTs.seconds)
  let limited := (sorted.toSubarray 0 (min limit sorted.size)).toArray

  -- Convert to JSON
  let messagesJson := limited.map fun entry =>
    let baseFields : List (String × Lean.Json) := [
      ("id", Lean.Json.num entry.id),
      ("from", Lean.Json.str entry.senderName),
      ("subject", Lean.Json.str entry.subject),
      ("importance", Lean.toJson entry.importance),
      ("ack_required", Lean.Json.bool entry.ackRequired),
      ("thread_id", match entry.threadId with | some t => Lean.Json.str t | none => Lean.Json.null),
      ("created_ts", Lean.Json.num entry.createdTs.seconds),
      ("kind", Lean.toJson entry.recipientType),
      ("project_id", Lean.Json.num entry.projectId),
      ("project_slug", Lean.Json.str entry.projectSlug),
      ("project_key", Lean.Json.str entry.projectKey)
    ]
    let fields := if includeBodies then
      baseFields ++ [("body_md", match entry.bodyMd with | some b => Lean.Json.str b | none => Lean.Json.null)]
    else baseFields
    Lean.Json.mkObj fields

  let resp := JsonRpc.Response.success req.id (Lean.Json.arr messagesJson)
  pure (Response.json (Lean.Json.compress (Lean.toJson resp)))

/-- Handle summarize_thread_product request - summarize thread across all product projects -/
def handleSummarizeThreadProduct (db : Storage.Database) (_cfg : Config) (req : JsonRpc.Request) : IO Response := do
  let params := req.params.getD Lean.Json.null

  -- Extract required params
  let productId ← match params.getObjValAs? String "product_id" with
    | Except.ok p => pure p
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: product_id")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  let threadIds ← match params.getObjValAs? (Array String) "thread_id" with
    | Except.ok arr => pure arr
    | Except.error _ =>
      match params.getObjValAs? String "thread_id" with
      | Except.ok t => pure #[t]
      | Except.error _ =>
        let err := JsonRpc.Error.invalidParams (some "missing required param: thread_id")
        let resp := JsonRpc.Response.failure req.id err
        return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Optional params
  let llmMode := match params.getObjValAs? Bool "llm_mode" with
    | Except.ok b => b
    | Except.error _ => true

  let perThreadLimit := match params.getObjValAs? Nat "per_thread_limit" with
    | Except.ok n => n
    | Except.error _ => 50

  let modelOpt := match params.getObjValAs? String "model" with
    | Except.ok m => some m
    | Except.error _ => none

  let maxTokensOpt := match params.getObjValAs? Nat "max_tokens" with
    | Except.ok n => some n
    | Except.error _ => none

  -- Resolve product
  let product ← match ← resolveProduct db productId with
    | some p => pure p
    | none =>
      let err := JsonRpc.Error.invalidParams (some s!"product not found: {productId}")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Get all projects for this product
  let projects ← db.queryProjectsByProduct product.id

  -- Normalize thread IDs
  let threadIds := threadIds.toList.map String.trim |>.filter (· != "") |>.toArray

  if threadIds.isEmpty then
    let err := JsonRpc.Error.invalidParams (some "no valid thread_id provided")
    let resp := JsonRpc.Response.failure req.id err
    return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Get API key for LLM mode
  let apiKey ← if llmMode then
    IO.getEnv "OPENROUTER_API_KEY" |>.map (·.getD "")
  else
    pure ""

  -- Process each thread across all projects
  let mut threadResults : Array Lean.Json := #[]
  let mut aggregateKeyPoints : Array String := #[]
  let mut aggregateActionItems : Array Search.ActionItem := #[]
  let mut aggregateMentions : Array Search.MentionEntry := #[]
  let mut anyMessages : Bool := false

  for threadId in threadIds do
    -- Collect messages from all projects
    let mut allMessages : Array Storage.Database.MessageWithSender := #[]
    for project in projects do
      let messages ← db.queryMessagesByThread project.id threadId perThreadLimit
      allMessages := allMessages ++ messages

    if allMessages.isEmpty then
      continue
    anyMessages := true

    -- Sort by timestamp
    let sortedMessages := allMessages.qsort (fun a b => a.createdTs.seconds < b.createdTs.seconds)

    -- Build heuristic summary
    let heuristic := Search.buildHeuristicSummary sortedMessages

    -- Optionally refine with LLM
    let summary ← if llmMode && !apiKey.isEmpty then
      let excerpts := Search.buildExcerpts sortedMessages 800 15
      Search.refineSummaryWithLLM apiKey modelOpt maxTokensOpt heuristic excerpts
    else
      pure heuristic

    -- Build thread result JSON
    let fields : List (String × Lean.Json) := [
      ("thread_id", Lean.Json.str threadId),
      ("summary", Lean.toJson summary)
    ]

    threadResults := threadResults.push (Lean.Json.mkObj fields)

    -- Aggregate for multi-thread
    aggregateKeyPoints := aggregateKeyPoints ++ summary.keyPoints
    aggregateActionItems := aggregateActionItems ++ summary.actionItems
    for m in summary.mentions do
      match aggregateMentions.findIdx? (fun e => e.name == m.name) with
      | some idx =>
        let existing := aggregateMentions[idx]!
        aggregateMentions := aggregateMentions.set! idx { name := m.name, count := existing.count + m.count }
      | none =>
        aggregateMentions := aggregateMentions.push m

  -- Build response
  if !anyMessages then
    let err := JsonRpc.Error.invalidParams (some "no messages found for provided thread_id(s)")
    let resp := JsonRpc.Response.failure req.id err
    return Response.json (Lean.Json.compress (Lean.toJson resp))

  let result := if threadIds.size == 1 && threadResults.size == 1 then
    -- Single thread response
    threadResults[0]!
  else
    -- Multi-thread response with aggregate
    let topMentions := aggregateMentions.qsort (fun a b => a.count > b.count) |>.toList.take 5 |>.toArray
    Lean.Json.mkObj [
      ("threads", Lean.Json.arr threadResults),
      ("aggregate", Lean.Json.mkObj [
        ("top_mentions", Lean.toJson topMentions),
        ("key_points", Lean.toJson (aggregateKeyPoints.toList.take 10 |>.toArray)),
        ("action_items", Lean.toJson aggregateActionItems)
      ])
    ]

  let resp := JsonRpc.Response.success req.id result
  pure (Response.json (Lean.Json.compress (Lean.toJson resp)))

end AgentMail.Tools.Products
