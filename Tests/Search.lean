import Crucible
import AgentMail

open Crucible
open AgentMail

namespace Tests.Search

testSuite "Search"

open Citadel

def parseJsonRpcResponse (resp : Response) : IO JsonRpc.Response := do
  let body := String.fromUTF8! resp.body
  let json := Lean.Json.parse body
  match json with
  | Except.ok j =>
      match (Lean.FromJson.fromJson? j : Except String JsonRpc.Response) with
      | Except.ok r => pure r
      | Except.error e => throw (IO.userError s!"Failed to decode JSON-RPC response: {e}")
  | Except.error e => throw (IO.userError s!"Failed to parse JSON: {e}")

def mkAgent (projectId : Nat) (name : String) (now : Chronos.Timestamp) : Agent := {
  id := 0
  projectId := projectId
  name := name
  program := "test"
  model := "test"
  taskDescription := ""
  contactPolicy := ContactPolicy.auto
  attachmentsPolicy := AttachmentsPolicy.auto
  inceptionTs := now
  lastActiveTs := now
}

-- =============================================================================
-- Helper Extraction Tests
-- =============================================================================

test "extractMentions finds @mentions" := do
  let text := "Hello @alice and @bob, please review. Thanks @alice!"
  let mentions := Tools.Search.extractMentions text
  mentions.size ≡ (2 : Nat)
  -- alice mentioned twice
  match mentions.find? (fun m => m.name == "alice") with
  | some m => m.count ≡ (2 : Nat)
  | none => throw (IO.userError "alice not found")
  -- bob mentioned once
  match mentions.find? (fun m => m.name == "bob") with
  | some m => m.count ≡ (1 : Nat)
  | none => throw (IO.userError "bob not found")

test "extractMentions handles empty text" := do
  let mentions := Tools.Search.extractMentions ""
  mentions.size ≡ (0 : Nat)

test "extractMentions ignores standalone @" := do
  let mentions := Tools.Search.extractMentions "Email me @ example.com"
  mentions.size ≡ (0 : Nat)

test "extractKeyPoints finds bullet items" := do
  let text := "Some intro text\n- First point\n- Second point\n* Third point\n+ Fourth point"
  let points := Tools.Search.extractKeyPoints text
  points.size ≡ (4 : Nat)
  (points.getD 0 "") ≡ "First point"
  (points.getD 1 "") ≡ "Second point"
  (points.getD 2 "") ≡ "Third point"
  (points.getD 3 "") ≡ "Fourth point"

test "extractKeyPoints finds numbered items" := do
  let text := "1. First item\n2. Second item\n3) Third item"
  let points := Tools.Search.extractKeyPoints text
  points.size ≡ (3 : Nat)

test "extractActionItems finds checkboxes" := do
  let text := "- [ ] Todo item\n- [x] Done item\n- [X] Also done"
  let items := Tools.Search.extractActionItems text
  items.size ≡ (3 : Nat)
  -- First should be unchecked
  (items.getD 0 { text := "", done := false }).done ≡ false
  -- Second and third should be checked
  (items.getD 1 { text := "", done := false }).done ≡ true
  (items.getD 2 { text := "", done := false }).done ≡ true

test "extractActionItems finds TODO keywords" := do
  let text := "Some context\nTODO: fix this bug\nMore text\nFIXME: broken code"
  let items := Tools.Search.extractActionItems text
  items.size ≡ (2 : Nat)

test "extractCodeRefs finds backtick paths" := do
  let text := "Check `src/main.lean` and `lib/utils.ts` for details"
  let refs := Tools.Search.extractCodeRefs text
  refs.size ≡ (2 : Nat)
  shouldSatisfy (refs.contains "src/main.lean") "should contain src/main.lean"
  shouldSatisfy (refs.contains "lib/utils.ts") "should contain lib/utils.ts"

test "extractCodeRefs ignores non-paths" := do
  let text := "Use `map` and `filter` functions"
  let refs := Tools.Search.extractCodeRefs text
  refs.size ≡ (0 : Nat)

-- =============================================================================
-- Database Search Tests
-- =============================================================================

test "searchMessages finds by subject" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "test" "/test" now
  -- Create sender
  let senderId ← db.insertAgent (mkAgent projectId "Sender" now)
  -- Create messages
  for (subj, body) in [("Bug report", "Found issue"), ("Feature request", "Please add"), ("Another bug", "Also broken")] do
    let msg : Message := {
      id := 0, projectId := projectId, senderId := senderId
      subject := subj, bodyMd := body
      attachments := #[]
      importance := Importance.normal, ackRequired := false
      threadId := none, createdTs := now
    }
    let _ ← db.insertMessage msg
  -- Search for "bug"
  let results ← db.searchMessages projectId "bug" 10 none none
  results.size ≡ (2 : Nat)
  db.close

test "searchMessages finds by body" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "test" "/test" now
  let senderId ← db.insertAgent (mkAgent projectId "Sender" now)
  let msg : Message := {
    id := 0, projectId := projectId, senderId := senderId
    subject := "General update", bodyMd := "The authentication module has a critical error"
    attachments := #[]
    importance := Importance.normal, ackRequired := false
    threadId := none, createdTs := now
  }
  let _ ← db.insertMessage msg
  -- Search for "authentication"
  let results ← db.searchMessages projectId "authentication" 10 none none
  results.size ≡ (1 : Nat)
  (results.getD 0 default).subject ≡ "General update"
  db.close

test "searchMessages filters by thread" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "test" "/test" now
  let senderId ← db.insertAgent (mkAgent projectId "Sender" now)
  -- Create messages in different threads
  for (threadId, subj) in [("thread-1", "Error in thread 1"), ("thread-2", "Error in thread 2"), ("thread-1", "More errors")] do
    let msg : Message := {
      id := 0, projectId := projectId, senderId := senderId
      subject := subj, bodyMd := "Details"
      attachments := #[]
      importance := Importance.normal, ackRequired := false
      threadId := some threadId, createdTs := now
    }
    let _ ← db.insertMessage msg
  -- Search within thread-1 only
  let results ← db.searchMessages projectId "error" 10 (some "thread-1") none
  results.size ≡ (2 : Nat)
  db.close

test "searchMessages filters by participant agent" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "test" "/test" now
  let senderId ← db.insertAgent (mkAgent projectId "Sender" now)
  let bobId ← db.insertAgent (mkAgent projectId "Bob" now)
  let carolId ← db.insertAgent (mkAgent projectId "Carol" now)
  let msg1 : Message := {
    id := 0, projectId := projectId, senderId := senderId
    subject := "Hello Bob", bodyMd := "Hi Bob"
    attachments := #[]
    importance := Importance.normal, ackRequired := false
    threadId := none, createdTs := now
  }
  let msg1Id ← db.insertMessage msg1
  let msg2 : Message := {
    id := 0, projectId := projectId, senderId := senderId
    subject := "Hello Carol", bodyMd := "Hi Carol"
    attachments := #[]
    importance := Importance.normal, ackRequired := false
    threadId := none, createdTs := now
  }
  let msg2Id ← db.insertMessage msg2
  db.insertMessageRecipient {
    messageId := msg1Id, agentId := bobId
    recipientType := RecipientType.toRecipient
    readAt := none, ackedAt := none
  }
  db.insertMessageRecipient {
    messageId := msg2Id, agentId := carolId
    recipientType := RecipientType.toRecipient
    readAt := none, ackedAt := none
  }
  let resultsBob ← db.searchMessages projectId "Hello" 10 none (some bobId)
  resultsBob.size ≡ (1 : Nat)
  (resultsBob.getD 0 default).subject ≡ "Hello Bob"
  let resultsSender ← db.searchMessages projectId "Hello" 10 none (some senderId)
  resultsSender.size ≡ (2 : Nat)
  db.close

test "searchMessages handles backslash queries" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "test" "/test" now
  let senderId ← db.insertAgent (mkAgent projectId "Sender" now)
  let msg : Message := {
    id := 0, projectId := projectId, senderId := senderId
    subject := "Path C:\\temp", bodyMd := "See C:\\temp"
    attachments := #[]
    importance := Importance.normal, ackRequired := false
    threadId := none, createdTs := now
  }
  let _ ← db.insertMessage msg
  let results ← db.searchMessages projectId "C:\\temp" 10 none none
  results.size ≡ (1 : Nat)
  db.close

test "queryMessagesByThread returns messages in order" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "test" "/test" now
  let senderAgent : Agent := {
    id := 0, projectId := projectId, name := "Alice"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let senderId ← db.insertAgent senderAgent
  -- Create messages in a thread
  for i in [1, 2, 3] do
    let msg : Message := {
      id := 0, projectId := projectId, senderId := senderId
      subject := s!"Message {i}", bodyMd := s!"Content {i}"
      attachments := #[]
      importance := Importance.normal, ackRequired := false
      threadId := some "thread-123", createdTs := Chronos.Timestamp.fromSeconds (1700000000 + i)
    }
    let _ ← db.insertMessage msg
  let messages ← db.queryMessagesByThread projectId "thread-123" 50
  messages.size ≡ (3 : Nat)
  -- Should be in ASC order (oldest first)
  (messages.getD 0 default).subject ≡ "Message 1"
  (messages.getD 2 default).subject ≡ "Message 3"
  db.close

test "queryMessagesByThread returns most recent messages in order" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "test" "/test" now
  let senderId ← db.insertAgent (mkAgent projectId "Alice" now)
  for i in [1, 2, 3, 4, 5] do
    let msg : Message := {
      id := 0, projectId := projectId, senderId := senderId
      subject := s!"Message {i}", bodyMd := s!"Content {i}"
      attachments := #[]
      importance := Importance.normal, ackRequired := false
      threadId := some "thread-xyz", createdTs := Chronos.Timestamp.fromSeconds (1700000000 + i)
    }
    let _ ← db.insertMessage msg
  let messages ← db.queryMessagesByThread projectId "thread-xyz" 2
  messages.size ≡ (2 : Nat)
  (messages.getD 0 default).subject ≡ "Message 4"
  (messages.getD 1 default).subject ≡ "Message 5"
  db.close

-- =============================================================================
-- Heuristic Summary Tests
-- =============================================================================

test "buildHeuristicSummary extracts participants" := do
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let messages : Array Storage.Database.MessageWithSender := #[
    { id := 1, subject := "Hi", bodyMd := "Hello", importance := .normal, ackRequired := false, threadId := some "t1", createdTs := now, senderName := "Alice" },
    { id := 2, subject := "Re: Hi", bodyMd := "Hi back", importance := .normal, ackRequired := false, threadId := some "t1", createdTs := now, senderName := "Bob" },
    { id := 3, subject := "Re: Re: Hi", bodyMd := "Thanks", importance := .normal, ackRequired := false, threadId := some "t1", createdTs := now, senderName := "Alice" }
  ]
  let summary := Tools.Search.buildHeuristicSummary messages
  summary.participants.size ≡ (2 : Nat)
  shouldSatisfy (summary.participants.contains "Alice") "should contain Alice"
  shouldSatisfy (summary.participants.contains "Bob") "should contain Bob"
  summary.totalMessages ≡ (3 : Nat)

test "buildHeuristicSummary counts actions" := do
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let messages : Array Storage.Database.MessageWithSender := #[
    { id := 1, subject := "Tasks", bodyMd := "- [ ] Do this\n- [x] Already done\n- [ ] Do that", importance := .normal, ackRequired := false, threadId := some "t1", createdTs := now, senderName := "Alice" }
  ]
  let summary := Tools.Search.buildHeuristicSummary messages
  summary.openActions ≡ (2 : Nat)
  summary.doneActions ≡ (1 : Nat)
  summary.actionItems.size ≡ (3 : Nat)

test "buildHeuristicSummary extracts code references" := do
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let messages : Array Storage.Database.MessageWithSender := #[
    { id := 1, subject := "Code review", bodyMd := "Check `src/main.lean` and `lib/utils.lean`", importance := .normal, ackRequired := false, threadId := some "t1", createdTs := now, senderName := "Alice" }
  ]
  let summary := Tools.Search.buildHeuristicSummary messages
  summary.codeReferences.size ≡ (2 : Nat)

-- =============================================================================
-- Handler Tests
-- =============================================================================

test "search_messages returns array payload and respects agent_name" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "test" "/test" now
  let senderId ← db.insertAgent (mkAgent projectId "Sender" now)
  let bobId ← db.insertAgent (mkAgent projectId "Bob" now)
  let carolId ← db.insertAgent (mkAgent projectId "Carol" now)
  let msg1 : Message := {
    id := 0, projectId := projectId, senderId := senderId
    subject := "Hello Bob", bodyMd := "Hi Bob"
    attachments := #[]
    importance := Importance.normal, ackRequired := false
    threadId := none, createdTs := now
  }
  let msg1Id ← db.insertMessage msg1
  let msg2 : Message := {
    id := 0, projectId := projectId, senderId := senderId
    subject := "Hello Carol", bodyMd := "Hi Carol"
    attachments := #[]
    importance := Importance.normal, ackRequired := false
    threadId := none, createdTs := now
  }
  let msg2Id ← db.insertMessage msg2
  db.insertMessageRecipient {
    messageId := msg1Id, agentId := bobId
    recipientType := RecipientType.toRecipient
    readAt := none, ackedAt := none
  }
  db.insertMessageRecipient {
    messageId := msg2Id, agentId := carolId
    recipientType := RecipientType.toRecipient
    readAt := none, ackedAt := none
  }
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/test"),
    ("query", Lean.Json.str "Hello"),
    ("agent_name", Lean.Json.str "Bob")
  ]
  let req : JsonRpc.Request := {
    method := "search_messages"
    params := some params
    id := some (JsonRpc.RequestId.num 1)
  }
  let resp ← Tools.Search.handleSearchMessages db Config.default req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some (Lean.Json.arr items) =>
      items.size ≡ (1 : Nat)
  | _ => throw (IO.userError "Expected search_messages result to be a JSON array")
  db.close

test "summarize_thread treats thread_id string literally" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "test" "/test" now
  let senderId ← db.insertAgent (mkAgent projectId "Sender" now)
  let msg : Message := {
    id := 0, projectId := projectId, senderId := senderId
    subject := "Comma Thread", bodyMd := "Discussion"
    attachments := #[]
    importance := Importance.normal, ackRequired := false
    threadId := some "thread,with,comma", createdTs := now
  }
  let _ ← db.insertMessage msg
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/test"),
    ("thread_id", Lean.Json.str "thread,with,comma"),
    ("llm_mode", Lean.Json.bool false)
  ]
  let req : JsonRpc.Request := {
    method := "summarize_thread"
    params := some params
    id := some (JsonRpc.RequestId.num 2)
  }
  let resp ← Tools.Search.handleSummarizeThread db Config.default req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some result =>
      match result.getObjValAs? String "thread_id" with
      | Except.ok threadId => threadId ≡ "thread,with,comma"
      | Except.error e => throw (IO.userError s!"Failed to read thread_id: {e}")
  | none => throw (IO.userError "Expected summarize_thread result")
  db.close

test "summarize_thread returns error for missing thread" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let _ ← db.insertProject "test" "/test" now
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/test"),
    ("thread_id", Lean.Json.str "missing-thread"),
    ("llm_mode", Lean.Json.bool false)
  ]
  let req : JsonRpc.Request := {
    method := "summarize_thread"
    params := some params
    id := some (JsonRpc.RequestId.num 3)
  }
  let resp ← Tools.Search.handleSummarizeThread db Config.default req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.error with
  | some _ => pure ()
  | none => throw (IO.userError "Expected summarize_thread to return error for missing thread")
  db.close

end Tests.Search
