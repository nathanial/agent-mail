import Crucible
import AgentMail

open Crucible
open AgentMail

namespace Tests.Products

testSuite "Products"

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
-- Database Tests
-- =============================================================================

test "insertProduct creates product" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let id ← db.insertProduct "my-product" now
  id ≡ (1 : Nat)
  db.close

test "queryProductByProductId finds existing product" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let _ ← db.insertProduct "my-product" now
  match ← db.queryProductByProductId "my-product" with
  | some product =>
    product.productId ≡ "my-product"
    product.createdAt.seconds ≡ now.seconds
  | none => throw (IO.userError "Product not found")
  db.close

test "queryProductByProductId returns none for missing product" := do
  let db ← Storage.Database.openMemory
  match ← db.queryProductByProductId "nonexistent" with
  | some _ => throw (IO.userError "Expected none for missing product")
  | none => pure ()
  db.close

test "insertProductProject creates link" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let productId ← db.insertProduct "my-product" now
  let projectId ← db.insertProject "test" "/test" now
  let linkId ← db.insertProductProject productId projectId now
  linkId ≡ (1 : Nat)
  db.close

test "queryProductProjectLink finds existing link" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let productId ← db.insertProduct "my-product" now
  let projectId ← db.insertProject "test" "/test" now
  let _ ← db.insertProductProject productId projectId now
  match ← db.queryProductProjectLink productId projectId with
  | some link =>
    link.productDbId ≡ productId
    link.projectId ≡ projectId
  | none => throw (IO.userError "Link not found")
  db.close

test "queryProjectsByProduct returns all linked projects" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let productId ← db.insertProduct "my-product" now
  let proj1Id ← db.insertProject "proj1" "/proj1" now
  let proj2Id ← db.insertProject "proj2" "/proj2" now
  let _ ← db.insertProductProject productId proj1Id now
  let _ ← db.insertProductProject productId proj2Id now
  let projects ← db.queryProjectsByProduct productId
  projects.size ≡ (2 : Nat)
  db.close

test "queryProductsByProject returns all products a project belongs to" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let prod1Id ← db.insertProduct "product-1" now
  let prod2Id ← db.insertProduct "product-2" now
  let projectId ← db.insertProject "test" "/test" now
  let _ ← db.insertProductProject prod1Id projectId now
  let _ ← db.insertProductProject prod2Id projectId now
  let products ← db.queryProductsByProject projectId
  products.size ≡ (2 : Nat)
  db.close

-- =============================================================================
-- Handler Tests
-- =============================================================================

test "ensure_product creates new product" := do
  let db ← Storage.Database.openMemory
  let params := Lean.Json.mkObj [
    ("product_id", Lean.Json.str "new-product")
  ]
  let req : JsonRpc.Request := {
    method := "ensure_product"
    params := some params
    id := some (JsonRpc.RequestId.num 1)
  }
  let resp ← Tools.Products.handleEnsureProduct db Config.default req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some result =>
    match result.getObjValAs? String "product_id" with
    | Except.ok pid => pid ≡ "new-product"
    | Except.error e => throw (IO.userError s!"Failed to read product_id: {e}")
    match result.getObjValAs? Nat "id" with
    | Except.ok id => id ≡ (1 : Nat)
    | Except.error e => throw (IO.userError s!"Failed to read id: {e}")
    match result.getObjValAs? Int "created_at" with
    | Except.ok ts => shouldSatisfy (ts > 0) "created_at should be numeric seconds"
    | Except.error e => throw (IO.userError s!"Failed to read created_at: {e}")
  | none => throw (IO.userError "Expected ensure_product result")
  db.close

test "ensure_product is idempotent" := do
  let db ← Storage.Database.openMemory
  let params := Lean.Json.mkObj [
    ("product_id", Lean.Json.str "my-product")
  ]
  let req : JsonRpc.Request := {
    method := "ensure_product"
    params := some params
    id := some (JsonRpc.RequestId.num 1)
  }
  -- First call
  let resp1 ← Tools.Products.handleEnsureProduct db Config.default req
  let rpcResp1 ← parseJsonRpcResponse resp1
  -- Second call
  let resp2 ← Tools.Products.handleEnsureProduct db Config.default req
  let rpcResp2 ← parseJsonRpcResponse resp2
  -- Both should return same ID
  match rpcResp1.result, rpcResp2.result with
  | some r1, some r2 =>
    match r1.getObjValAs? Nat "id", r2.getObjValAs? Nat "id" with
    | Except.ok id1, Except.ok id2 => id1 ≡ id2
    | _, _ => throw (IO.userError "Failed to read IDs")
  | _, _ => throw (IO.userError "Expected results from both calls")
  db.close

test "products_link links project to product" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  -- Create product and project
  let _ ← db.insertProduct "my-product" now
  let _ ← db.insertProject "test" "/test" now
  let params := Lean.Json.mkObj [
    ("product_id", Lean.Json.str "my-product"),
    ("project_key", Lean.Json.str "/test")
  ]
  let req : JsonRpc.Request := {
    method := "products_link"
    params := some params
    id := some (JsonRpc.RequestId.num 1)
  }
  let resp ← Tools.Products.handleProductsLink db Config.default req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some result =>
    match result.getObjValAs? Bool "linked" with
    | Except.ok linked => linked ≡ true
    | Except.error e => throw (IO.userError s!"Failed to read linked: {e}")
    match result.getObjValAs? String "project_slug" with
    | Except.ok slug => slug ≡ "test"
    | Except.error e => throw (IO.userError s!"Failed to read project_slug: {e}")
    match result.getObjValAs? Int "linked_at" with
    | Except.ok ts => shouldSatisfy (ts > 0) "linked_at should be numeric seconds"
    | Except.error e => throw (IO.userError s!"Failed to read linked_at: {e}")
  | none => throw (IO.userError "Expected products_link result")
  db.close

test "products_link is idempotent" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let _ ← db.insertProduct "my-product" now
  let _ ← db.insertProject "test" "/test" now
  let params := Lean.Json.mkObj [
    ("product_id", Lean.Json.str "my-product"),
    ("project_key", Lean.Json.str "/test")
  ]
  let req : JsonRpc.Request := {
    method := "products_link"
    params := some params
    id := some (JsonRpc.RequestId.num 1)
  }
  -- Link twice
  let resp1 ← Tools.Products.handleProductsLink db Config.default req
  let rpcResp1 ← parseJsonRpcResponse resp1
  let resp2 ← Tools.Products.handleProductsLink db Config.default req
  let rpcResp2 ← parseJsonRpcResponse resp2
  -- Both should succeed
  match rpcResp1.result, rpcResp2.result with
  | some r1, some r2 =>
    match r1.getObjValAs? Bool "linked", r2.getObjValAs? Bool "linked" with
    | Except.ok l1, Except.ok l2 =>
      l1 ≡ true
      l2 ≡ true
    | _, _ => throw (IO.userError "Failed to read linked fields")
  | _, _ => throw (IO.userError "Expected results from both calls")
  db.close

test "products_link fails for missing product" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let _ ← db.insertProject "test" "/test" now
  let params := Lean.Json.mkObj [
    ("product_id", Lean.Json.str "nonexistent"),
    ("project_key", Lean.Json.str "/test")
  ]
  let req : JsonRpc.Request := {
    method := "products_link"
    params := some params
    id := some (JsonRpc.RequestId.num 1)
  }
  let resp ← Tools.Products.handleProductsLink db Config.default req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.error with
  | some _ => pure ()
  | none => throw (IO.userError "Expected error for missing product")
  db.close

test "products_link fails for missing project" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let _ ← db.insertProduct "my-product" now
  let params := Lean.Json.mkObj [
    ("product_id", Lean.Json.str "my-product"),
    ("project_key", Lean.Json.str "/nonexistent")
  ]
  let req : JsonRpc.Request := {
    method := "products_link"
    params := some params
    id := some (JsonRpc.RequestId.num 1)
  }
  let resp ← Tools.Products.handleProductsLink db Config.default req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.error with
  | some _ => pure ()
  | none => throw (IO.userError "Expected error for missing project")
  db.close

test "search_messages_product searches across projects" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  -- Create product
  let productId ← db.insertProduct "my-product" now
  -- Create two projects
  let proj1Id ← db.insertProject "proj1" "/proj1" now
  let proj2Id ← db.insertProject "proj2" "/proj2" now
  -- Link projects to product
  let _ ← db.insertProductProject productId proj1Id now
  let _ ← db.insertProductProject productId proj2Id now
  -- Create agents and messages in each project
  let sender1Id ← db.insertAgent (mkAgent proj1Id "Alice" now)
  let sender2Id ← db.insertAgent (mkAgent proj2Id "Bob" now)
  let msg1 : Message := {
    id := 0, projectId := proj1Id, senderId := sender1Id
    subject := "Test message from proj1", bodyMd := "Content from project 1"
    attachments := #[]
    importance := Importance.normal, ackRequired := false
    threadId := none, createdTs := Chronos.Timestamp.fromSeconds 1700000001
  }
  let msg2 : Message := {
    id := 0, projectId := proj2Id, senderId := sender2Id
    subject := "Test message from proj2", bodyMd := "Content from project 2"
    attachments := #[]
    importance := Importance.normal, ackRequired := false
    threadId := none, createdTs := Chronos.Timestamp.fromSeconds 1700000002
  }
  let _ ← db.insertMessage msg1
  let _ ← db.insertMessage msg2
  -- Search across product
  let params := Lean.Json.mkObj [
    ("product_id", Lean.Json.str "my-product"),
    ("query", Lean.Json.str "Test message")
  ]
  let req : JsonRpc.Request := {
    method := "search_messages_product"
    params := some params
    id := some (JsonRpc.RequestId.num 1)
  }
  let resp ← Tools.Products.handleSearchMessagesProduct db Config.default req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some (Lean.Json.arr arr) =>
    arr.size ≡ (2 : Nat)
  | some _ => throw (IO.userError "Expected search_messages_product array result")
  | none => throw (IO.userError "Expected search_messages_product result")
  db.close

test "search_messages_product returns empty for unlinked projects" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  -- Create product with no linked projects
  let _ ← db.insertProduct "empty-product" now
  -- Create a project with messages but don't link it
  let projId ← db.insertProject "proj1" "/proj1" now
  let senderId ← db.insertAgent (mkAgent projId "Alice" now)
  let msg : Message := {
    id := 0, projectId := projId, senderId := senderId
    subject := "Test message", bodyMd := "Content"
    attachments := #[]
    importance := Importance.normal, ackRequired := false
    threadId := none, createdTs := now
  }
  let _ ← db.insertMessage msg
  -- Search should find nothing
  let params := Lean.Json.mkObj [
    ("product_id", Lean.Json.str "empty-product"),
    ("query", Lean.Json.str "Test")
  ]
  let req : JsonRpc.Request := {
    method := "search_messages_product"
    params := some params
    id := some (JsonRpc.RequestId.num 1)
  }
  let resp ← Tools.Products.handleSearchMessagesProduct db Config.default req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some (Lean.Json.arr arr) =>
    arr.size ≡ (0 : Nat)
  | some _ => throw (IO.userError "Expected search_messages_product array result")
  | none => throw (IO.userError "Expected search_messages_product result")
  db.close

test "fetch_inbox_product fetches across projects" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  -- Create product
  let productId ← db.insertProduct "my-product" now
  -- Create two projects
  let proj1Id ← db.insertProject "proj1" "/proj1" now
  let proj2Id ← db.insertProject "proj2" "/proj2" now
  -- Link projects to product
  let _ ← db.insertProductProject productId proj1Id now
  let _ ← db.insertProductProject productId proj2Id now
  -- Create agents with same name in each project
  let alice1Id ← db.insertAgent (mkAgent proj1Id "Alice" now)
  let alice2Id ← db.insertAgent (mkAgent proj2Id "Alice" now)
  let bob1Id ← db.insertAgent (mkAgent proj1Id "Bob" now)
  let bob2Id ← db.insertAgent (mkAgent proj2Id "Bob" now)
  -- Create messages from Bob to Alice in each project
  let msg1 : Message := {
    id := 0, projectId := proj1Id, senderId := bob1Id
    subject := "Message in proj1", bodyMd := "Content 1"
    attachments := #[]
    importance := Importance.normal, ackRequired := false
    threadId := none, createdTs := Chronos.Timestamp.fromSeconds 1700000001
  }
  let msg2 : Message := {
    id := 0, projectId := proj2Id, senderId := bob2Id
    subject := "Message in proj2", bodyMd := "Content 2"
    attachments := #[]
    importance := Importance.normal, ackRequired := false
    threadId := none, createdTs := Chronos.Timestamp.fromSeconds 1700000002
  }
  let msg1Id ← db.insertMessage msg1
  let msg2Id ← db.insertMessage msg2
  -- Add Alice as recipient
  db.insertMessageRecipient {
    messageId := msg1Id, agentId := alice1Id
    recipientType := RecipientType.toRecipient
    readAt := none, ackedAt := none
  }
  db.insertMessageRecipient {
    messageId := msg2Id, agentId := alice2Id
    recipientType := RecipientType.toRecipient
    readAt := none, ackedAt := none
  }
  -- Fetch inbox for Alice across product
  let params := Lean.Json.mkObj [
    ("product_id", Lean.Json.str "my-product"),
    ("agent_name", Lean.Json.str "Alice")
  ]
  let req : JsonRpc.Request := {
    method := "fetch_inbox_product"
    params := some params
    id := some (JsonRpc.RequestId.num 1)
  }
  let resp ← Tools.Products.handleFetchInboxProduct db Config.default req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some (Lean.Json.arr arr) =>
    arr.size ≡ (2 : Nat)
    match arr.toList with
    | entry :: _ =>
      match entry.getObjValAs? String "project_key" with
      | Except.ok key => shouldSatisfy (key.length > 0) "project_key should be present"
      | Except.error e => throw (IO.userError s!"Failed to read project_key: {e}")
    | [] => throw (IO.userError "Expected message entries")
  | some _ => throw (IO.userError "Expected fetch_inbox_product array result")
  | none => throw (IO.userError "Expected fetch_inbox_product result")
  db.close

test "fetch_inbox_product only finds agents with matching name" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  -- Create product with one project
  let productId ← db.insertProduct "my-product" now
  let projId ← db.insertProject "proj1" "/proj1" now
  let _ ← db.insertProductProject productId projId now
  -- Create agents with different names
  let aliceId ← db.insertAgent (mkAgent projId "Alice" now)
  let bobId ← db.insertAgent (mkAgent projId "Bob" now)
  -- Create message to Bob only
  let msg : Message := {
    id := 0, projectId := projId, senderId := aliceId
    subject := "Message to Bob", bodyMd := "Content"
    attachments := #[]
    importance := Importance.normal, ackRequired := false
    threadId := none, createdTs := now
  }
  let msgId ← db.insertMessage msg
  db.insertMessageRecipient {
    messageId := msgId, agentId := bobId
    recipientType := RecipientType.toRecipient
    readAt := none, ackedAt := none
  }
  -- Fetch inbox for Carol (doesn't exist)
  let params := Lean.Json.mkObj [
    ("product_id", Lean.Json.str "my-product"),
    ("agent_name", Lean.Json.str "Carol")
  ]
  let req : JsonRpc.Request := {
    method := "fetch_inbox_product"
    params := some params
    id := some (JsonRpc.RequestId.num 1)
  }
  let resp ← Tools.Products.handleFetchInboxProduct db Config.default req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some (Lean.Json.arr arr) =>
    arr.size ≡ (0 : Nat)
  | some _ => throw (IO.userError "Expected fetch_inbox_product array result")
  | none => throw (IO.userError "Expected fetch_inbox_product result")
  db.close

test "summarize_thread_product summarizes across projects" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  -- Create product
  let productId ← db.insertProduct "my-product" now
  -- Create two projects
  let proj1Id ← db.insertProject "proj1" "/proj1" now
  let proj2Id ← db.insertProject "proj2" "/proj2" now
  -- Link projects to product
  let _ ← db.insertProductProject productId proj1Id now
  let _ ← db.insertProductProject productId proj2Id now
  -- Create messages with same thread_id in each project
  let sender1Id ← db.insertAgent (mkAgent proj1Id "Alice" now)
  let sender2Id ← db.insertAgent (mkAgent proj2Id "Bob" now)
  let msg1 : Message := {
    id := 0, projectId := proj1Id, senderId := sender1Id
    subject := "Thread topic", bodyMd := "Discussion point 1"
    attachments := #[]
    importance := Importance.normal, ackRequired := false
    threadId := some "shared-thread", createdTs := Chronos.Timestamp.fromSeconds 1700000001
  }
  let msg2 : Message := {
    id := 0, projectId := proj2Id, senderId := sender2Id
    subject := "Thread topic", bodyMd := "Discussion point 2"
    attachments := #[]
    importance := Importance.normal, ackRequired := false
    threadId := some "shared-thread", createdTs := Chronos.Timestamp.fromSeconds 1700000002
  }
  let _ ← db.insertMessage msg1
  let _ ← db.insertMessage msg2
  -- Summarize thread across product
  let params := Lean.Json.mkObj [
    ("product_id", Lean.Json.str "my-product"),
    ("thread_id", Lean.Json.str "shared-thread"),
    ("llm_mode", Lean.Json.bool false)
  ]
  let req : JsonRpc.Request := {
    method := "summarize_thread_product"
    params := some params
    id := some (JsonRpc.RequestId.num 1)
  }
  let resp ← Tools.Products.handleSummarizeThreadProduct db Config.default req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some result =>
    match result.getObjValAs? String "thread_id" with
    | Except.ok tid => tid ≡ "shared-thread"
    | Except.error e => throw (IO.userError s!"Failed to read thread_id: {e}")
    match result.getObjVal? "summary" with
    | Except.ok summary =>
      match summary.getObjVal? "participants" with
      | Except.ok (Lean.Json.arr parts) => parts.size ≡ (2 : Nat)
      | _ => throw (IO.userError "Expected participants array")
      match summary.getObjValAs? Nat "total_messages" with
      | Except.ok total => total ≡ (2 : Nat)
      | Except.error e => throw (IO.userError s!"Failed to read total_messages: {e}")
    | Except.error e => throw (IO.userError s!"Failed to read summary: {e}")
  | none => throw (IO.userError "Expected summarize_thread_product result")
  db.close

test "summarize_thread_product returns error for missing thread" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let _ ← db.insertProduct "my-product" now
  let params := Lean.Json.mkObj [
    ("product_id", Lean.Json.str "my-product"),
    ("thread_id", Lean.Json.str "nonexistent"),
    ("llm_mode", Lean.Json.bool false)
  ]
  let req : JsonRpc.Request := {
    method := "summarize_thread_product"
    params := some params
    id := some (JsonRpc.RequestId.num 1)
  }
  let resp ← Tools.Products.handleSummarizeThreadProduct db Config.default req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.error with
  | some _ => pure ()
  | none => throw (IO.userError "Expected error for missing thread")
  db.close

end Tests.Products
