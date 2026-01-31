/-
  AgentMail.Tools.Search - Search and summarization MCP tool handlers
-/
import Chronos
import Citadel
import Oracle
import AgentMail.Config
import AgentMail.Protocol.JsonRpc
import AgentMail.Storage.Database
import AgentMail.Tools.Identity

open Citadel

namespace AgentMail.Tools.Search

-- =============================================================================
-- Helper Types
-- =============================================================================

/-- Mention with count -/
structure MentionEntry where
  name : String
  count : Nat
  deriving Repr, Inhabited

instance : Lean.ToJson MentionEntry where
  toJson m := Lean.Json.mkObj [
    ("name", Lean.Json.str m.name),
    ("count", Lean.Json.num m.count)
  ]

/-- Action item with completion status -/
structure ActionItem where
  text : String
  done : Bool
  deriving Repr

instance : Lean.ToJson ActionItem where
  toJson a := Lean.Json.mkObj [
    ("text", Lean.Json.str a.text),
    ("done", Lean.Json.bool a.done)
  ]

/-- Thread summary -/
structure ThreadSummary where
  participants : Array String
  keyPoints : Array String
  actionItems : Array ActionItem
  mentions : Array MentionEntry
  codeReferences : Array String
  totalMessages : Nat
  openActions : Nat
  doneActions : Nat
  deriving Repr

instance : Lean.ToJson ThreadSummary where
  toJson s := Lean.Json.mkObj [
    ("participants", Lean.toJson s.participants),
    ("key_points", Lean.toJson s.keyPoints),
    ("action_items", Lean.toJson s.actionItems),
    ("mentions", Lean.toJson s.mentions),
    ("code_references", Lean.toJson s.codeReferences),
    ("total_messages", Lean.Json.num s.totalMessages),
    ("open_actions", Lean.Json.num s.openActions),
    ("done_actions", Lean.Json.num s.doneActions)
  ]

-- =============================================================================
-- Extraction Helpers
-- =============================================================================

/-- Extract @mentions from text and count occurrences -/
def extractMentions (text : String) : Array MentionEntry := Id.run do
  let mut counts : List (String × Nat) := []
  let words := text.splitToList (fun c => c.isWhitespace || c == ',' || c == ':' || c == '.' || c == '!' || c == '?')
  for word in words do
    if word.startsWith "@" && word.length > 1 then
      let name := word.drop 1
      match counts.find? (fun p => p.1 == name) with
      | some (_, n) =>
        counts := counts.filter (fun p => p.1 != name)
        counts := (name, n + 1) :: counts
      | none =>
        counts := (name, 1) :: counts
  counts.toArray.map fun (name, count) => { name, count }

/-- Check if a line is a list item (bullet or numbered) -/
def isListItem (line : String) : Bool :=
  let trimmed := line.trim
  trimmed.startsWith "- " ||
  trimmed.startsWith "* " ||
  trimmed.startsWith "+ " ||
  (trimmed.length > 2 &&
   (trimmed.get ⟨0⟩).isDigit &&
   ((trimmed.get ⟨1⟩) == '.' || (trimmed.get ⟨1⟩) == ')'))

/-- Extract key points (list items) from text -/
def extractKeyPoints (text : String) : Array String := Id.run do
  let lines := text.splitOn "\n"
  let mut points : Array String := #[]
  for line in lines do
    let trimmed := line.trim
    if isListItem trimmed then
      -- Remove bullet/number prefix
      let content := if trimmed.startsWith "- " || trimmed.startsWith "* " || trimmed.startsWith "+ " then
        trimmed.drop 2
      else
        -- Numbered list: find first space after number
        let spacePos := trimmed.posOf ' '
        if spacePos.byteIdx < trimmed.endPos.byteIdx then
          trimmed.drop (spacePos.byteIdx + 1)
        else
          trimmed
      if content.length > 0 then
        points := points.push content.trim
  points

/-- Check if string contains substring -/
def containsSubstr (s sub : String) : Bool :=
  (s.splitOn sub).length > 1

/-- Check if text contains action keywords -/
def hasActionKeyword (text : String) : Bool :=
  let upper := text.toUpper
  containsSubstr upper "TODO" ||
  containsSubstr upper "ACTION" ||
  containsSubstr upper "FIXME" ||
  containsSubstr upper "NEXT" ||
  containsSubstr upper "BLOCKED"

/-- Extract action items from text -/
def extractActionItems (text : String) : Array ActionItem := Id.run do
  let lines := text.splitOn "\n"
  let mut items : Array ActionItem := #[]
  for line in lines do
    let trimmed := line.trim
    -- Check for checkbox syntax
    if trimmed.startsWith "- [ ]" then
      items := items.push { text := trimmed.drop 5 |>.trim, done := false }
    else if trimmed.startsWith "- [x]" || trimmed.startsWith "- [X]" then
      items := items.push { text := trimmed.drop 5 |>.trim, done := true }
    else if hasActionKeyword trimmed then
      -- Extract action from keyword-containing lines
      items := items.push { text := trimmed, done := false }
  items

/-- Extract code references (backtick-enclosed paths) from text -/
def extractCodeRefs (text : String) : Array String := Id.run do
  let mut refs : Array String := #[]
  let mut inBacktick := false
  let mut current := ""
  for c in text.toList do
    if c == '`' then
      if inBacktick then
        -- End of backtick section
        if containsSubstr current "/" || containsSubstr current "." then
          -- Looks like a path or file reference
          if !refs.contains current then
            refs := refs.push current
        current := ""
      inBacktick := !inBacktick
    else if inBacktick then
      current := current.push c
  refs

/-- Build heuristic summary from messages -/
def buildHeuristicSummary (messages : Array Storage.Database.MessageWithSender) : ThreadSummary := Id.run do
  let mut participants : Array String := #[]
  let mut allKeyPoints : Array String := #[]
  let mut allActionItems : Array ActionItem := #[]
  let mut allMentions : Array MentionEntry := #[]
  let mut allCodeRefs : Array String := #[]

  for msg in messages do
    -- Collect participants
    if !participants.contains msg.senderName then
      participants := participants.push msg.senderName

    -- Extract from body
    let keyPoints := extractKeyPoints msg.bodyMd
    allKeyPoints := allKeyPoints ++ keyPoints

    let actions := extractActionItems msg.bodyMd
    allActionItems := allActionItems ++ actions

    let mentions := extractMentions msg.bodyMd
    for m in mentions do
      match allMentions.findIdx? (fun e => e.name == m.name) with
      | some idx =>
        let existing := allMentions[idx]!
        allMentions := allMentions.set! idx { name := m.name, count := existing.count + m.count }
      | none =>
        allMentions := allMentions.push m

    let codeRefs := extractCodeRefs msg.bodyMd
    for r in codeRefs do
      if !allCodeRefs.contains r then
        allCodeRefs := allCodeRefs.push r

  -- Count open/done actions
  let openActions := allActionItems.filter (fun a => !a.done) |>.size
  let doneActions := allActionItems.filter (fun a => a.done) |>.size

  {
    participants
    keyPoints := allKeyPoints.toList.take 10 |>.toArray  -- Limit to 10
    actionItems := allActionItems
    mentions := allMentions
    codeReferences := allCodeRefs
    totalMessages := messages.size
    openActions
    doneActions
  }

-- =============================================================================
-- LLM Refinement
-- =============================================================================

/-- System prompt for thread summarization -/
def summarizationSystemPrompt : String :=
  "You are a thread summarization assistant. Analyze the provided message excerpts and return a JSON object with these fields:
- \"key_points\": array of 3-5 main discussion points
- \"action_items\": array of objects with \"text\" (string) and \"done\" (boolean) fields
- \"summary_sentence\": a single sentence summarizing the thread

Respond ONLY with valid JSON, no markdown or explanation."

/-- Build message excerpts for LLM -/
def buildExcerpts (messages : Array Storage.Database.MessageWithSender) (maxChars : Nat) (maxMessages : Nat) : String := Id.run do
  let mut result := ""
  let limited := messages.toList.take maxMessages
  for msg in limited do
    let excerpt := if msg.bodyMd.length > maxChars then
      msg.bodyMd.take maxChars ++ "..."
    else
      msg.bodyMd
    result := result ++ s!"From {msg.senderName}:\n{excerpt}\n\n"
  result

/-- Try to refine summary using LLM -/
def refineSummaryWithLLM (apiKey : String) (modelOpt : Option String) (maxTokensOpt : Option Nat) (heuristic : ThreadSummary) (excerpts : String) : IO ThreadSummary := do
  if apiKey.isEmpty then
    return heuristic

  let client := match modelOpt with
    | some model => Oracle.Client.withModel apiKey model
    | none => Oracle.Client.withApiKey apiKey
  let userPrompt := s!"Here are message excerpts from a thread:\n\n{excerpts}\n\nPlease summarize."
  let messages := #[Oracle.Message.system summarizationSystemPrompt, Oracle.Message.user userPrompt]
  let opts : Oracle.ChatOptions := { maxTokens := maxTokensOpt }

  match ← client.complete messages opts with
  | .error _ => pure heuristic  -- Graceful fallback
  | .ok response =>
    -- Try to parse the JSON response
    match Lean.Json.parse response with
    | .error _ => pure heuristic
    | .ok json =>
      -- Extract key_points if present
      let keyPoints := match json.getObjValAs? (Array String) "key_points" with
        | .ok arr => arr
        | .error _ => heuristic.keyPoints

      -- Extract action_items if present
      let actionItems := match json.getObjVal? "action_items" with
        | .ok actionsJson =>
          match actionsJson.getArr? with
          | .ok arr =>
            arr.filterMap fun j => do
              let text ← j.getObjValAs? String "text" |>.toOption
              let done ← j.getObjValAs? Bool "done" |>.toOption
              some { text, done : ActionItem }
          | .error _ => heuristic.actionItems
        | .error _ => heuristic.actionItems

      let openActions := actionItems.filter (fun a => !a.done) |>.size
      let doneActions := actionItems.filter (fun a => a.done) |>.size

      pure { heuristic with
        keyPoints
        actionItems
        openActions
        doneActions
      }

-- =============================================================================
-- Handlers
-- =============================================================================

/-- Handle search_messages request -/
def handleSearchMessages (db : Storage.Database) (_cfg : Config) (req : JsonRpc.Request) : IO Response := do
  let params := req.params.getD Lean.Json.null

  -- Extract required params
  let projectKey ← match params.getObjValAs? String "project_key" with
    | Except.ok k => pure k
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: project_key")
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

  let agentNameOpt := match params.getObjValAs? String "agent_name" with
    | Except.ok n => some n
    | Except.error _ => none

  -- Resolve project
  let project ← match ← Identity.resolveProject db projectKey with
    | some p => pure p
    | none =>
      let err := JsonRpc.Error.invalidParams (some s!"project not found: {projectKey}")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  let agentIdOpt ← match agentNameOpt with
    | some name =>
      match ← db.queryAgentByName project.id name with
      | some agent => pure (some agent.id)
      | none =>
        let err := JsonRpc.Error.invalidParams (some s!"agent not found: {name}")
        let resp := JsonRpc.Response.failure req.id err
        return Response.json (Lean.Json.compress (Lean.toJson resp))
    | none => pure none

  -- Search messages
  let results ← db.searchMessages project.id query limit threadIdOpt agentIdOpt

  -- Convert results to JSON
  let resultsJson := results.map fun r =>
    Lean.Json.mkObj [
      ("id", Lean.Json.num r.id),
      ("subject", Lean.Json.str r.subject),
      ("importance", Lean.toJson r.importance),
      ("ack_required", Lean.Json.bool r.ackRequired),
      ("created_ts", Lean.Json.num r.createdTs.seconds),
      ("thread_id", match r.threadId with | some t => Lean.Json.str t | none => Lean.Json.null),
      ("from", Lean.Json.str r.senderName)
    ]

  let resp := JsonRpc.Response.success req.id (Lean.Json.arr resultsJson)
  pure (Response.json (Lean.Json.compress (Lean.toJson resp)))

/-- Handle summarize_thread request -/
def handleSummarizeThread (db : Storage.Database) (_cfg : Config) (req : JsonRpc.Request) : IO Response := do
  let params := req.params.getD Lean.Json.null

  -- Extract required params
  let projectKey ← match params.getObjValAs? String "project_key" with
    | Except.ok k => pure k
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: project_key")
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

  let includeExamples := match params.getObjValAs? Bool "include_examples" with
    | Except.ok b => b
    | Except.error _ => false

  let perThreadLimit := match params.getObjValAs? Nat "per_thread_limit" with
    | Except.ok n => n
    | Except.error _ => 50

  let modelOpt := match params.getObjValAs? String "model" with
    | Except.ok m => some m
    | Except.error _ => none

  let maxTokensOpt := match params.getObjValAs? Nat "max_tokens" with
    | Except.ok n => some n
    | Except.error _ => none

  -- Resolve project
  let project ← match ← Identity.resolveProject db projectKey with
    | some p => pure p
    | none =>
      let err := JsonRpc.Error.invalidParams (some s!"project not found: {projectKey}")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

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

  -- Process each thread
  let mut threadResults : Array Lean.Json := #[]
  let mut aggregateKeyPoints : Array String := #[]
  let mut aggregateActionItems : Array ActionItem := #[]
  let mut aggregateMentions : Array MentionEntry := #[]
  let mut anyMessages : Bool := false

  for threadId in threadIds do
    let messages ← db.queryMessagesByThread project.id threadId perThreadLimit

    if messages.isEmpty then
      continue
    anyMessages := true

    -- Build heuristic summary
    let heuristic := buildHeuristicSummary messages

    -- Optionally refine with LLM
    let summary ← if llmMode && !apiKey.isEmpty then
      let excerpts := buildExcerpts messages 800 15
      refineSummaryWithLLM apiKey modelOpt maxTokensOpt heuristic excerpts
    else
      pure heuristic

    -- Build thread result JSON
    let baseFields : List (String × Lean.Json) := [
      ("thread_id", Lean.Json.str threadId),
      ("summary", Lean.toJson summary)
    ]

    let fields := if includeExamples then
      let examples := messages.toList.take 3 |>.map fun m =>
        Lean.Json.mkObj [
          ("from", Lean.Json.str m.senderName),
          ("subject", Lean.Json.str m.subject),
          ("excerpt", Lean.Json.str (if m.bodyMd.length > 200 then m.bodyMd.take 200 ++ "..." else m.bodyMd))
        ]
      baseFields ++ [("examples", Lean.Json.arr examples.toArray)]
    else
      baseFields

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

end AgentMail.Tools.Search
