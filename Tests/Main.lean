import Crucible
import Chronos
import Citadel
import AgentMail

open Crucible
open AgentMail

namespace Tests.Types

testSuite "Types"

test "ContactPolicy roundtrip" := do
  let policies := #[ContactPolicy.openPolicy, ContactPolicy.auto, ContactPolicy.contactsOnly, ContactPolicy.blockAll]
  for p in policies do
    let str := p.toString
    let parsed := ContactPolicy.fromString? str
    parsed ≡ some p

test "ContactPolicy JSON roundtrip" := do
  let p := ContactPolicy.auto
  let json := Lean.toJson p
  let parsed : Except String ContactPolicy := Lean.FromJson.fromJson? json
  match parsed with
  | Except.ok q => q ≡ p
  | Except.error e => throw (IO.userError s!"Failed to parse: {e}")

test "AttachmentsPolicy roundtrip" := do
  let policies := #[AttachmentsPolicy.auto, AttachmentsPolicy.inline, AttachmentsPolicy.file]
  for p in policies do
    let str := p.toString
    let parsed := AttachmentsPolicy.fromString? str
    parsed ≡ some p

test "Importance roundtrip" := do
  let levels := #[Importance.low, Importance.normal, Importance.high, Importance.urgent]
  for i in levels do
    let str := i.toString
    let parsed := Importance.fromString? str
    parsed ≡ some i

test "RecipientType roundtrip" := do
  let types := #[RecipientType.toRecipient, RecipientType.cc, RecipientType.bcc]
  for t in types do
    let str := t.toString
    let parsed := RecipientType.fromString? str
    parsed ≡ some t

end Tests.Types

namespace Tests.Project

testSuite "Project"

test "JSON roundtrip" := do
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let project : Project := {
    id := 1
    slug := "test-project"
    humanKey := "Test Project"
    createdAt := now
  }
  let json := Lean.toJson project
  let parsed : Except String Project := Lean.FromJson.fromJson? json
  match parsed with
  | Except.ok p =>
    p.id ≡ project.id
    p.slug ≡ project.slug
    p.humanKey ≡ project.humanKey
    p.createdAt.seconds ≡ project.createdAt.seconds
  | Except.error e => throw (IO.userError s!"Failed to parse: {e}")

end Tests.Project

namespace Tests.Agent

testSuite "Agent"

test "JSON roundtrip" := do
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let agent : Agent := {
    id := 1
    projectId := 1
    name := "builder"
    program := "claude-code"
    model := "opus-4.5"
    taskDescription := "Building features"
    contactPolicy := ContactPolicy.openPolicy
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now
    lastActiveTs := now
  }
  let json := Lean.toJson agent
  let parsed : Except String Agent := Lean.FromJson.fromJson? json
  match parsed with
  | Except.ok a =>
    a.id ≡ agent.id
    a.name ≡ agent.name
    a.contactPolicy ≡ agent.contactPolicy
  | Except.error e => throw (IO.userError s!"Failed to parse: {e}")

end Tests.Agent

namespace Tests.Message

testSuite "Message"

test "JSON roundtrip" := do
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let msg : Message := {
    id := 1
    projectId := 1
    senderId := 1
    subject := "Test subject"
    bodyMd := "Test body"
    attachments := #[]
    importance := Importance.normal
    ackRequired := false
    threadId := some "thread-123"
    createdTs := now
  }
  let json := Lean.toJson msg
  let parsed : Except String Message := Lean.FromJson.fromJson? json
  match parsed with
  | Except.ok m =>
    m.id ≡ msg.id
    m.subject ≡ msg.subject
    m.threadId ≡ msg.threadId
  | Except.error e => throw (IO.userError s!"Failed to parse: {e}")

test "Message without threadId" := do
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let msg : Message := {
    id := 1
    projectId := 1
    senderId := 1
    subject := "Test"
    bodyMd := "Body"
    attachments := #[]
    importance := Importance.low
    ackRequired := true
    threadId := none
    createdTs := now
  }
  let json := Lean.toJson msg
  let parsed : Except String Message := Lean.FromJson.fromJson? json
  match parsed with
  | Except.ok m => shouldSatisfy m.threadId.isNone "threadId should be none"
  | Except.error e => throw (IO.userError s!"Failed to parse: {e}")

end Tests.Message

namespace Tests.FileReservation

testSuite "FileReservation"

test "JSON roundtrip" := do
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let later := Chronos.Timestamp.fromSeconds 1700003600
  let res : FileReservation := {
    id := 1
    projectId := 1
    agentId := 1
    pathPattern := "src/**/*.lean"
    exclusive := true
    reason := "Refactoring module"
    createdTs := now
    expiresTs := later
    releasedTs := none
  }
  let json := Lean.toJson res
  let parsed : Except String FileReservation := Lean.FromJson.fromJson? json
  match parsed with
  | Except.ok r =>
    r.id ≡ res.id
    r.pathPattern ≡ res.pathPattern
    r.exclusive ≡ res.exclusive
  | Except.error e => throw (IO.userError s!"Failed to parse: {e}")

test "isActive check" := do
  let now := Chronos.Timestamp.fromSeconds 1700001000
  let res : FileReservation := {
    id := 1
    projectId := 1
    agentId := 1
    pathPattern := "*.lean"
    exclusive := true
    reason := ""
    createdTs := Chronos.Timestamp.fromSeconds 1700000000
    expiresTs := Chronos.Timestamp.fromSeconds 1700002000
    releasedTs := none
  }
  shouldSatisfy (res.isActive now) "should be active"
  -- After expiry
  let later := Chronos.Timestamp.fromSeconds 1700003000
  shouldSatisfy (not (res.isActive later)) "should be expired"
  -- If released
  let released := { res with releasedTs := some now }
  shouldSatisfy (not (released.isActive now)) "should be released"

end Tests.FileReservation

namespace Tests.JsonRpc

testSuite "JsonRpc"

test "Request parsing" := do
  let json := Lean.Json.parse "{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"id\":1}"
  match json with
  | Except.ok j =>
    let req : Except String JsonRpc.Request := Lean.FromJson.fromJson? j
    match req with
    | Except.ok r =>
      r.method ≡ "test"
      r.id ≡ some (JsonRpc.RequestId.num 1)
    | Except.error e => throw (IO.userError s!"Failed to parse request: {e}")
  | Except.error e => throw (IO.userError s!"Failed to parse JSON: {e}")

test "Request with params" := do
  let json := Lean.Json.parse "{\"jsonrpc\":\"2.0\",\"method\":\"add\",\"params\":{\"a\":1,\"b\":2},\"id\":\"req-1\"}"
  match json with
  | Except.ok j =>
    let req : Except String JsonRpc.Request := Lean.FromJson.fromJson? j
    match req with
    | Except.ok r =>
      r.method ≡ "add"
      shouldSatisfy r.params.isSome "params should be present"
      r.id ≡ some (JsonRpc.RequestId.str "req-1")
    | Except.error e => throw (IO.userError s!"Failed to parse request: {e}")
  | Except.error e => throw (IO.userError s!"Failed to parse JSON: {e}")

test "Notification (no id)" := do
  let json := Lean.Json.parse "{\"jsonrpc\":\"2.0\",\"method\":\"notify\"}"
  match json with
  | Except.ok j =>
    let req : Except String JsonRpc.Request := Lean.FromJson.fromJson? j
    match req with
    | Except.ok r =>
      r.method ≡ "notify"
      shouldSatisfy r.isNotification "should be notification"
    | Except.error e => throw (IO.userError s!"Failed to parse request: {e}")
  | Except.error e => throw (IO.userError s!"Failed to parse JSON: {e}")

test "Error creation" := do
  let err := JsonRpc.Error.methodNotFound "unknown_method"
  err.code ≡ JsonRpc.errorMethodNotFound
  err.message ≡ "Method not found: unknown_method"

test "Response success" := do
  let resp := JsonRpc.Response.success (some (JsonRpc.RequestId.num 1)) (Lean.Json.str "ok")
  let json := Lean.toJson resp
  let str := Lean.Json.compress json
  shouldSatisfy (str.find? "result" |>.isSome) "should contain result"
  shouldSatisfy (str.find? "error" |>.isNone) "should not contain error"

test "Response failure" := do
  let err := JsonRpc.Error.invalidParams (some "missing field")
  let resp := JsonRpc.Response.failure (some (JsonRpc.RequestId.num 1)) err
  let json := Lean.toJson resp
  let str := Lean.Json.compress json
  shouldSatisfy (str.find? "error" |>.isSome) "should contain error"
  shouldSatisfy (str.find? "-32602" |>.isSome) "should contain error code"

end Tests.JsonRpc

namespace Tests.Config

testSuite "Config"

test "Default config" := do
  let cfg := Config.default
  cfg.environment ≡ "development"
  cfg.port ≡ 8765
  cfg.host ≡ "127.0.0.1"
  cfg.databasePath ≡ "agent_mail.db"
  shouldSatisfy cfg.authToken.isNone "authToken should be none"

test "Display hides token" := do
  let cfg := { Config.default with authToken := some "secret" }
  let display := cfg.display
  shouldSatisfy (display.find? "(set)" |>.isSome) "should show (set)"
  shouldSatisfy (display.find? "secret" |>.isNone) "should hide secret"

end Tests.Config

namespace Tests.Database

testSuite "Database"

test "Schema initialization" := do
  let db ← Storage.Database.openMemory
  -- Verify tables exist by querying sqlite_master
  let rows ← db.query "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name"
  let tableNames := rows.map fun row =>
    match row.get? 0 with
    | some (Quarry.Value.text name) => name
    | _ => ""
  shouldSatisfy (tableNames.contains "projects") "should have projects table"
  shouldSatisfy (tableNames.contains "agents") "should have agents table"
  shouldSatisfy (tableNames.contains "messages") "should have messages table"
  shouldSatisfy (tableNames.contains "message_recipients") "should have message_recipients table"
  shouldSatisfy (tableNames.contains "file_reservations") "should have file_reservations table"
  db.close

test "Insert and query project" := do
  let db ← Storage.Database.openMemory
  let now := 1700000000
  let id ← db.insert s!"INSERT INTO projects (slug, human_key, created_at) VALUES ('test', 'Test', {now})"
  id ≡ (1 : Int)
  let row ← db.queryOne "SELECT slug, human_key FROM projects WHERE id = 1"
  match row with
  | some r =>
    match (r.get? 0, r.get? 1) with
    | (some (Quarry.Value.text slug), some (Quarry.Value.text humanKey)) =>
      slug ≡ "test"
      humanKey ≡ "Test"
    | _ => throw (IO.userError "Unexpected column types")
  | none => throw (IO.userError "Project not found")
  db.close

end Tests.Database

namespace Tests.NameGenerator

testSuite "NameGenerator"

open AgentMail.Utils.NameGenerator in
test "Deterministic name generation" := do
  let name1 := generateNameDeterministic 12345
  let name2 := generateNameDeterministic 12345
  name1 ≡ name2
  -- Verify it follows AdjectiveNoun pattern
  shouldSatisfy (name1.length > 0) "name should not be empty"

open AgentMail.Utils.NameGenerator in
test "Different seeds produce different names" := do
  let name1 := generateNameDeterministic 1
  let name2 := generateNameDeterministic 1000000
  shouldSatisfy (name1 != name2) "different seeds should produce different names"

end Tests.NameGenerator

namespace Tests.Identity

testSuite "Identity"

open AgentMail.Tools.Identity in
test "generateSlug sanitizes paths" := do
  let slug1 := generateSlug "/Users/test/my-project"
  shouldSatisfy (slug1.find? "/" |>.isNone) "slug should not contain slashes"
  shouldSatisfy (not (slug1.startsWith "-")) "slug should not start with dash"
  shouldSatisfy (not (slug1.endsWith "-")) "slug should not end with dash"

open AgentMail.Tools.Identity in
test "generateSlug handles various inputs" := do
  let slug1 := generateSlug "/foo/bar/baz"
  slug1 ≡ "foo-bar-baz"
  let slug2 := generateSlug "simple"
  slug2 ≡ "simple"

end Tests.Identity

namespace Tests.DatabaseQueries

testSuite "DatabaseQueries"

test "Insert and query project" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let id ← db.insertProject "test-slug" "/Users/test/project" now
  id ≡ (1 : Nat)
  let project ← db.queryProjectByHumanKey "/Users/test/project"
  match project with
  | some p =>
    p.slug ≡ "test-slug"
    p.humanKey ≡ "/Users/test/project"
  | none => throw (IO.userError "Project not found")
  db.close

test "Query project by ID" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let id ← db.insertProject "slug1" "/path/1" now
  let project ← db.queryProjectById id
  match project with
  | some p => p.id ≡ id
  | none => throw (IO.userError "Project not found by ID")
  db.close

test "Insert and query agent" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  -- First create a project
  let projectId ← db.insertProject "test" "/test" now
  -- Create an agent
  let agent : Agent := {
    id := 0
    projectId := projectId
    name := "TestAgent"
    program := "claude-code"
    model := "opus-4.5"
    taskDescription := "Testing"
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now
    lastActiveTs := now
  }
  let _agentId ← db.insertAgent agent
  -- Query by name
  let found ← db.queryAgentByName projectId "TestAgent"
  match found with
  | some a =>
    a.name ≡ "TestAgent"
    a.program ≡ "claude-code"
    a.model ≡ "opus-4.5"
  | none => throw (IO.userError "Agent not found")
  db.close

test "Update agent last active" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let later := Chronos.Timestamp.fromSeconds 1700001000
  let projectId ← db.insertProject "test" "/test" now
  let agent : Agent := {
    id := 0
    projectId := projectId
    name := "UpdateTest"
    program := "test"
    model := "test"
    taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now
    lastActiveTs := now
  }
  let agentId ← db.insertAgent agent
  db.updateAgentLastActive agentId later
  let found ← db.queryAgentById agentId
  match found with
  | some a => a.lastActiveTs.seconds ≡ later.seconds
  | none => throw (IO.userError "Agent not found after update")
  db.close

end Tests.DatabaseQueries

namespace Tests.Messaging

testSuite "Messaging"

test "Insert and query message" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  -- Create project
  let projectId ← db.insertProject "test" "/test" now
  -- Create sender agent
  let senderAgent : Agent := {
    id := 0, projectId := projectId, name := "Sender"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let senderId ← db.insertAgent senderAgent
  -- Create recipient agent
  let recipientAgent : Agent := {
    id := 0, projectId := projectId, name := "Recipient"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let _recipientId ← db.insertAgent recipientAgent
  -- Create message
  let msg : Message := {
    id := 0, projectId := projectId, senderId := senderId
    subject := "Test Subject", bodyMd := "Test body"
    attachments := #[]
    importance := Importance.normal, ackRequired := false
    threadId := some "thread-123", createdTs := now
  }
  let messageId ← db.insertMessage msg
  messageId ≡ (1 : Nat)
  -- Query message
  let found ← db.queryMessageById messageId
  match found with
  | some m =>
    m.subject ≡ "Test Subject"
    m.bodyMd ≡ "Test body"
    m.threadId ≡ some "thread-123"
  | none => throw (IO.userError "Message not found")
  db.close

test "Insert and query recipients" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "test" "/test" now
  -- Create agents
  let senderAgent : Agent := {
    id := 0, projectId := projectId, name := "Sender"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let senderId ← db.insertAgent senderAgent
  let recipientAgent : Agent := {
    id := 0, projectId := projectId, name := "Recipient"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let recipientId ← db.insertAgent recipientAgent
  -- Create message
  let msg : Message := {
    id := 0, projectId := projectId, senderId := senderId
    subject := "Test", bodyMd := "Body"
    attachments := #[]
    importance := Importance.high, ackRequired := true
    threadId := none, createdTs := now
  }
  let messageId ← db.insertMessage msg
  -- Add recipient
  let msgRecipient : MessageRecipient := {
    messageId := messageId, agentId := recipientId
    recipientType := RecipientType.toRecipient
    readAt := none, ackedAt := none
  }
  db.insertMessageRecipient msgRecipient
  -- Query recipient status
  let status ← db.queryRecipientStatus messageId recipientId
  match status with
  | some s =>
    s.recipientType ≡ RecipientType.toRecipient
    shouldSatisfy s.readAt.isNone "readAt should be none"
    shouldSatisfy s.ackedAt.isNone "ackedAt should be none"
  | none => throw (IO.userError "Recipient not found")
  db.close

test "Query inbox" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "test" "/test" now
  -- Create agents
  let senderAgent : Agent := {
    id := 0, projectId := projectId, name := "Sender"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let senderId ← db.insertAgent senderAgent
  let recipientAgent : Agent := {
    id := 0, projectId := projectId, name := "Recipient"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let recipientId ← db.insertAgent recipientAgent
  -- Create 3 messages
  for i in [1, 2, 3] do
    let msg : Message := {
      id := 0, projectId := projectId, senderId := senderId
      subject := s!"Message {i}", bodyMd := s!"Body {i}"
      attachments := #[]
      importance := if i == 3 then Importance.urgent else Importance.normal
      ackRequired := false
      threadId := some s!"thread-{i}", createdTs := Chronos.Timestamp.fromSeconds (1700000000 + i)
    }
    let messageId ← db.insertMessage msg
    let msgRecipient : MessageRecipient := {
      messageId := messageId, agentId := recipientId
      recipientType := RecipientType.toRecipient
      readAt := none, ackedAt := none
    }
    db.insertMessageRecipient msgRecipient
  -- Query inbox
  let entries ← db.queryInbox projectId recipientId 10 false none
  entries.size ≡ (3 : Nat)
  -- First entry should be most recent (Message 3)
  (entries.getD 0 default).subject ≡ "Message 3"
  -- Query urgent only
  let urgentEntries ← db.queryInbox projectId recipientId 10 true none
  urgentEntries.size ≡ (1 : Nat)
  (urgentEntries.getD 0 default).subject ≡ "Message 3"
  db.close

test "Update read and ack status" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let later := Chronos.Timestamp.fromSeconds 1700001000
  let projectId ← db.insertProject "test" "/test" now
  -- Create agents
  let senderAgent : Agent := {
    id := 0, projectId := projectId, name := "Sender"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let senderId ← db.insertAgent senderAgent
  let recipientAgent : Agent := {
    id := 0, projectId := projectId, name := "Recipient"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let recipientId ← db.insertAgent recipientAgent
  -- Create message
  let msg : Message := {
    id := 0, projectId := projectId, senderId := senderId
    subject := "Test", bodyMd := "Body"
    attachments := #[]
    importance := Importance.normal, ackRequired := true
    threadId := none, createdTs := now
  }
  let messageId ← db.insertMessage msg
  let msgRecipient : MessageRecipient := {
    messageId := messageId, agentId := recipientId
    recipientType := RecipientType.toRecipient
    readAt := none, ackedAt := none
  }
  db.insertMessageRecipient msgRecipient
  -- Mark as read
  let readUpdated ← db.updateMessageReadAt messageId recipientId later
  shouldSatisfy readUpdated "should have updated read status"
  -- Mark as read again (should not update since already set)
  let readUpdated2 ← db.updateMessageReadAt messageId recipientId (Chronos.Timestamp.fromSeconds 1700002000)
  shouldSatisfy (not readUpdated2) "should not update already read message"
  -- Verify read_at
  let status ← db.queryRecipientStatus messageId recipientId
  match status with
  | some s =>
    match s.readAt with
    | some t => t.seconds ≡ later.seconds
    | none => throw (IO.userError "readAt should be set")
  | none => throw (IO.userError "Recipient not found")
  -- Acknowledge
  let ackUpdated ← db.updateMessageAckedAt messageId recipientId later
  shouldSatisfy ackUpdated "should have updated ack status"
  let statusAfterAck ← db.queryRecipientStatus messageId recipientId
  match statusAfterAck with
  | some s =>
    shouldSatisfy s.ackedAt.isSome "ackedAt should be set"
  | none => throw (IO.userError "Recipient not found after ack")
  db.close

test "Count inbox" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "test" "/test" now
  let senderAgent : Agent := {
    id := 0, projectId := projectId, name := "Sender"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let senderId ← db.insertAgent senderAgent
  let recipientAgent : Agent := {
    id := 0, projectId := projectId, name := "Recipient"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let recipientId ← db.insertAgent recipientAgent
  -- Initially empty
  let count0 ← db.countInbox projectId recipientId
  count0 ≡ (0 : Nat)
  -- Add messages
  for i in [1, 2, 3, 4, 5] do
    let msg : Message := {
      id := 0, projectId := projectId, senderId := senderId
      subject := s!"Msg {i}", bodyMd := ""
      attachments := #[]
      importance := Importance.normal, ackRequired := false
      threadId := none, createdTs := now
    }
    let messageId ← db.insertMessage msg
    let msgRecipient : MessageRecipient := {
      messageId := messageId, agentId := recipientId
      recipientType := RecipientType.toRecipient
      readAt := none, ackedAt := none
    }
    db.insertMessageRecipient msgRecipient
  let count5 ← db.countInbox projectId recipientId
  count5 ≡ (5 : Nat)
  db.close

end Tests.Messaging

namespace Tests.ContactRequest

testSuite "ContactRequest"

test "ContactRequestStatus roundtrip" := do
  let statuses := #[ContactRequestStatus.pending, ContactRequestStatus.accepted, ContactRequestStatus.rejected]
  for s in statuses do
    let str := s.toString
    let parsed := ContactRequestStatus.fromString? str
    parsed ≡ some s

test "ContactRequest JSON roundtrip" := do
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let req : ContactRequest := {
    id := 1
    projectId := 1
    fromAgentId := 1
    toAgentId := 2
    message := "Hello, let's connect"
    status := ContactRequestStatus.pending
    createdTs := now
    respondedAt := none
  }
  let json := Lean.toJson req
  let parsed : Except String ContactRequest := Lean.FromJson.fromJson? json
  match parsed with
  | Except.ok r =>
    r.id ≡ req.id
    r.fromAgentId ≡ req.fromAgentId
    r.toAgentId ≡ req.toAgentId
    r.status ≡ req.status
  | Except.error e => throw (IO.userError s!"Failed to parse: {e}")

test "ContactRequest with respondedAt" := do
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let later := Chronos.Timestamp.fromSeconds 1700001000
  let req : ContactRequest := {
    id := 1
    projectId := 1
    fromAgentId := 1
    toAgentId := 2
    message := ""
    status := ContactRequestStatus.accepted
    createdTs := now
    respondedAt := some later
  }
  let json := Lean.toJson req
  let parsed : Except String ContactRequest := Lean.FromJson.fromJson? json
  match parsed with
  | Except.ok r =>
    r.status ≡ ContactRequestStatus.accepted
    match r.respondedAt with
    | some ts => ts.seconds ≡ later.seconds
    | none => throw (IO.userError "respondedAt should be set")
  | Except.error e => throw (IO.userError s!"Failed to parse: {e}")

end Tests.ContactRequest

namespace Tests.Contact

testSuite "Contact"

test "Contact JSON roundtrip" := do
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let contact : Contact := {
    id := 1
    projectId := 1
    agentId1 := 1
    agentId2 := 2
    createdTs := now
  }
  let json := Lean.toJson contact
  let parsed : Except String Contact := Lean.FromJson.fromJson? json
  match parsed with
  | Except.ok c =>
    c.id ≡ contact.id
    c.agentId1 ≡ contact.agentId1
    c.agentId2 ≡ contact.agentId2
  | Except.error e => throw (IO.userError s!"Failed to parse: {e}")

test "Contact.involvesAgent" := do
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let contact : Contact := {
    id := 1, projectId := 1, agentId1 := 1, agentId2 := 2, createdTs := now
  }
  shouldSatisfy (contact.involvesAgent 1) "should involve agent 1"
  shouldSatisfy (contact.involvesAgent 2) "should involve agent 2"
  shouldSatisfy (not (contact.involvesAgent 3)) "should not involve agent 3"

test "Contact.otherAgent" := do
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let contact : Contact := {
    id := 1, projectId := 1, agentId1 := 1, agentId2 := 2, createdTs := now
  }
  contact.otherAgent 1 ≡ some 2
  contact.otherAgent 2 ≡ some 1
  contact.otherAgent 3 ≡ none

end Tests.Contact

namespace Tests.ContactDatabase

testSuite "ContactDatabase"

test "Insert and query contact request" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  -- Create project
  let projectId ← db.insertProject "test" "/test" now
  -- Create agents
  let agent1 : Agent := {
    id := 0, projectId := projectId, name := "Agent1"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let agent1Id ← db.insertAgent agent1
  let agent2 : Agent := {
    id := 0, projectId := projectId, name := "Agent2"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let agent2Id ← db.insertAgent agent2
  -- Insert contact request
  let requestId ← db.insertContactRequest projectId agent1Id agent2Id "Hello!" now
  requestId ≡ (1 : Nat)
  -- Query by ID
  let found ← db.queryContactRequestById requestId
  match found with
  | some r =>
    r.fromAgentId ≡ agent1Id
    r.toAgentId ≡ agent2Id
    r.message ≡ "Hello!"
    r.status ≡ ContactRequestStatus.pending
  | none => throw (IO.userError "Contact request not found")
  db.close

test "Query contact request between agents" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "test" "/test" now
  let agent1 : Agent := {
    id := 0, projectId := projectId, name := "Agent1"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let agent1Id ← db.insertAgent agent1
  let agent2 : Agent := {
    id := 0, projectId := projectId, name := "Agent2"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let agent2Id ← db.insertAgent agent2
  -- No request yet
  let notFound ← db.queryContactRequestBetween projectId agent1Id agent2Id
  shouldSatisfy notFound.isNone "should not find request before insert"
  -- Insert request
  let _ ← db.insertContactRequest projectId agent1Id agent2Id "" now
  -- Now found
  let found ← db.queryContactRequestBetween projectId agent1Id agent2Id
  shouldSatisfy found.isSome "should find request after insert"
  db.close

test "Query pending contact requests" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "test" "/test" now
  let agent1 : Agent := {
    id := 0, projectId := projectId, name := "Agent1"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let agent1Id ← db.insertAgent agent1
  let agent2 : Agent := {
    id := 0, projectId := projectId, name := "Agent2"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let agent2Id ← db.insertAgent agent2
  let agent3 : Agent := {
    id := 0, projectId := projectId, name := "Agent3"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let agent3Id ← db.insertAgent agent3
  -- Insert requests to agent2
  let _ ← db.insertContactRequest projectId agent1Id agent2Id "From 1" now
  let _ ← db.insertContactRequest projectId agent3Id agent2Id "From 3" now
  -- Query pending for agent2
  let pending ← db.queryPendingContactRequests projectId agent2Id
  pending.size ≡ (2 : Nat)
  db.close

test "Update contact request status" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let later := Chronos.Timestamp.fromSeconds 1700001000
  let projectId ← db.insertProject "test" "/test" now
  let agent1 : Agent := {
    id := 0, projectId := projectId, name := "Agent1"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let agent1Id ← db.insertAgent agent1
  let agent2 : Agent := {
    id := 0, projectId := projectId, name := "Agent2"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let agent2Id ← db.insertAgent agent2
  let requestId ← db.insertContactRequest projectId agent1Id agent2Id "" now
  -- Update to accepted
  db.updateContactRequestStatus requestId ContactRequestStatus.accepted later
  let found ← db.queryContactRequestById requestId
  match found with
  | some r =>
    r.status ≡ ContactRequestStatus.accepted
    match r.respondedAt with
    | some ts => ts.seconds ≡ later.seconds
    | none => throw (IO.userError "respondedAt should be set")
  | none => throw (IO.userError "Contact request not found")
  db.close

test "Insert and query contact" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "test" "/test" now
  let agent1 : Agent := {
    id := 0, projectId := projectId, name := "Agent1"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let agent1Id ← db.insertAgent agent1
  let agent2 : Agent := {
    id := 0, projectId := projectId, name := "Agent2"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let agent2Id ← db.insertAgent agent2
  -- Insert contact (order shouldn't matter)
  let contactId ← db.insertContact projectId agent2Id agent1Id now
  contactId ≡ (1 : Nat)
  -- Query both directions
  let found1 ← db.queryContactBetween projectId agent1Id agent2Id
  shouldSatisfy found1.isSome "should find contact (1,2)"
  let found2 ← db.queryContactBetween projectId agent2Id agent1Id
  shouldSatisfy found2.isSome "should find contact (2,1)"
  db.close

test "Query contacts for agent" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "test" "/test" now
  let agent1 : Agent := {
    id := 0, projectId := projectId, name := "Agent1"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let agent1Id ← db.insertAgent agent1
  let agent2 : Agent := {
    id := 0, projectId := projectId, name := "Agent2"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let agent2Id ← db.insertAgent agent2
  let agent3 : Agent := {
    id := 0, projectId := projectId, name := "Agent3"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let agent3Id ← db.insertAgent agent3
  -- Agent1 has contacts with Agent2 and Agent3
  let _ ← db.insertContact projectId agent1Id agent2Id now
  let _ ← db.insertContact projectId agent1Id agent3Id now
  -- Query contacts for Agent1
  let contacts ← db.queryContacts projectId agent1Id
  contacts.size ≡ (2 : Nat)
  -- Verify names
  let names := contacts.map (·.agentName)
  shouldSatisfy (names.contains "Agent2") "should include Agent2"
  shouldSatisfy (names.contains "Agent3") "should include Agent3"
  db.close

test "Update agent contact policy" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "test" "/test" now
  let agent : Agent := {
    id := 0, projectId := projectId, name := "Agent1"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let agentId ← db.insertAgent agent
  -- Update to blockAll
  db.updateAgentContactPolicy agentId ContactPolicy.blockAll
  let found ← db.queryAgentById agentId
  match found with
  | some a => a.contactPolicy ≡ ContactPolicy.blockAll
  | none => throw (IO.userError "Agent not found")
  db.close

end Tests.ContactDatabase

namespace Tests.ContactTools

testSuite "ContactTools"

open Citadel

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

def mkAgentWithPolicy (projectId : Nat) (name : String) (policy : ContactPolicy) (now : Chronos.Timestamp) : Agent := {
  id := 0
  projectId := projectId
  name := name
  program := "test"
  model := "test"
  taskDescription := ""
  contactPolicy := policy
  attachmentsPolicy := AttachmentsPolicy.auto
  inceptionTs := now
  lastActiveTs := now
}

def parseJsonRpcResponse (resp : Response) : IO JsonRpc.Response := do
  let body := String.fromUTF8! resp.body
  let json := Lean.Json.parse body
  match json with
  | Except.ok j =>
      match (Lean.FromJson.fromJson? j : Except String JsonRpc.Response) with
      | Except.ok r => pure r
      | Except.error e => throw (IO.userError s!"Failed to decode JSON-RPC response: {e}")
  | Except.error e => throw (IO.userError s!"Failed to parse JSON: {e}")

test "list_contacts returns array payload" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "p1" "/p1" now
  let agent1Id ← db.insertAgent (mkAgent projectId "Agent1" now)
  let agent2Id ← db.insertAgent (mkAgent projectId "Agent2" now)
  let _ ← db.insertContact projectId agent1Id agent2Id now
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("agent_name", Lean.Json.str "Agent1")
  ]
  let req : JsonRpc.Request := {
    method := "list_contacts"
    params := some params
    id := some (JsonRpc.RequestId.num 1)
  }
  let resp ← Tools.Contacts.handleListContacts db req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some (Lean.Json.arr items) =>
      items.size ≡ (1 : Nat)
      match items.toList with
      | entry :: _ =>
          match entry.getObjValAs? String "agent_name" with
          | Except.ok name => name ≡ "Agent2"
          | Except.error e => throw (IO.userError s!"Failed to read agent_name: {e}")
      | [] => throw (IO.userError "Expected contact entry")
  | _ => throw (IO.userError "Expected list_contacts result to be a JSON array")
  db.close

test "request_contact refreshes non-pending requests" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let later := Chronos.Timestamp.fromSeconds 1700001000
  let projectId ← db.insertProject "p1" "/p1" now
  let agent1Id ← db.insertAgent (mkAgent projectId "Agent1" now)
  let agent2Id ← db.insertAgent (mkAgent projectId "Agent2" now)
  let requestId ← db.insertContactRequest projectId agent1Id agent2Id "old message" now
  db.updateContactRequestStatus requestId ContactRequestStatus.accepted later
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("from_agent", Lean.Json.str "Agent1"),
    ("to_agent", Lean.Json.str "Agent2"),
    ("message", Lean.Json.str "new message")
  ]
  let req : JsonRpc.Request := {
    method := "request_contact"
    params := some params
    id := some (JsonRpc.RequestId.num 2)
  }
  let resp ← Tools.Contacts.handleRequestContact db req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some result =>
      match result.getObjValAs? String "status" with
      | Except.ok status => status ≡ "pending"
      | Except.error e => throw (IO.userError s!"Failed to read status: {e}")
  | none => throw (IO.userError "Expected request_contact result")
  let refreshed ← db.queryContactRequestById requestId
  match refreshed with
  | some r =>
      r.status ≡ ContactRequestStatus.pending
      r.message ≡ "new message"
      shouldSatisfy r.respondedAt.isNone "responded_at should be cleared"
  | none => throw (IO.userError "Contact request not found after refresh")
  db.close

test "respond_contact rejects requests from other projects" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let project1Id ← db.insertProject "p1" "/p1" now
  let project2Id ← db.insertProject "p2" "/p2" now
  let agentA1Id ← db.insertAgent (mkAgent project1Id "AgentA" now)
  let agentB1Id ← db.insertAgent (mkAgent project1Id "AgentB" now)
  let _agentB2Id ← db.insertAgent (mkAgent project2Id "AgentB" now)
  let requestId ← db.insertContactRequest project1Id agentA1Id agentB1Id "hello" now
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p2"),
    ("agent_name", Lean.Json.str "AgentB"),
    ("request_id", Lean.Json.num requestId),
    ("accept", Lean.Json.bool true)
  ]
  let req : JsonRpc.Request := {
    method := "respond_contact"
    params := some params
    id := some (JsonRpc.RequestId.num 3)
  }
  let resp ← Tools.Contacts.handleRespondContact db req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.error with
  | some err =>
      err.message ≡ "Invalid params"
      match err.data with
      | some (Lean.Json.str details) =>
          details ≡ "contact request does not belong to this project"
      | _ => throw (IO.userError "Expected error details string")
  | none => throw (IO.userError "Expected error response for project mismatch")
  db.close

test "respond_contact reuses existing contact" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "p1" "/p1" now
  let agent1Id ← db.insertAgent (mkAgent projectId "Agent1" now)
  let agent2Id ← db.insertAgent (mkAgent projectId "Agent2" now)
  let requestId ← db.insertContactRequest projectId agent1Id agent2Id "hello" now
  let existingContactId ← db.insertContact projectId agent1Id agent2Id now
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("agent_name", Lean.Json.str "Agent2"),
    ("request_id", Lean.Json.num requestId),
    ("accept", Lean.Json.bool true)
  ]
  let req : JsonRpc.Request := {
    method := "respond_contact"
    params := some params
    id := some (JsonRpc.RequestId.num 4)
  }
  let resp ← Tools.Contacts.handleRespondContact db req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some result =>
      match result.getObjValAs? Int "contact_id" with
      | Except.ok contactId => contactId ≡ Int.ofNat existingContactId
      | Except.error e => throw (IO.userError s!"Failed to read contact_id: {e}")
  | none => throw (IO.userError "Expected respond_contact result")
  let contacts ← db.queryContacts projectId agent1Id
  contacts.size ≡ (1 : Nat)
  db.close

test "request_contact respects block_all policy" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "p1" "/p1" now
  let _agent1Id ← db.insertAgent (mkAgent projectId "Agent1" now)
  let _agent2Id ← db.insertAgent (mkAgentWithPolicy projectId "Agent2" ContactPolicy.blockAll now)
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("from_agent", Lean.Json.str "Agent1"),
    ("to_agent", Lean.Json.str "Agent2"),
    ("message", Lean.Json.str "hello")
  ]
  let req : JsonRpc.Request := {
    method := "request_contact"
    params := some params
    id := some (JsonRpc.RequestId.num 5)
  }
  let resp ← Tools.Contacts.handleRequestContact db req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.error with
  | some err =>
      err.message ≡ "Invalid params"
      match err.data with
      | some (Lean.Json.str details) =>
          details ≡ "Agent2 is not accepting contact requests"
      | _ => throw (IO.userError "Expected error details string")
  | none => throw (IO.userError "Expected error response for block_all policy")
  db.close

test "respond_contact only allows recipient agent" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "p1" "/p1" now
  let agent1Id ← db.insertAgent (mkAgent projectId "Agent1" now)
  let agent2Id ← db.insertAgent (mkAgent projectId "Agent2" now)
  let _agent3Id ← db.insertAgent (mkAgent projectId "Agent3" now)
  let requestId ← db.insertContactRequest projectId agent1Id agent2Id "hello" now
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("agent_name", Lean.Json.str "Agent3"),
    ("request_id", Lean.Json.num requestId),
    ("accept", Lean.Json.bool true)
  ]
  let req : JsonRpc.Request := {
    method := "respond_contact"
    params := some params
    id := some (JsonRpc.RequestId.num 6)
  }
  let resp ← Tools.Contacts.handleRespondContact db req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.error with
  | some err =>
      err.message ≡ "Invalid params"
      match err.data with
      | some (Lean.Json.str details) =>
          details ≡ "you can only respond to requests sent to you"
      | _ => throw (IO.userError "Expected error details string")
  | none => throw (IO.userError "Expected error response for wrong agent")
  db.close

end Tests.ContactTools

namespace Tests.FileReservationDatabase

testSuite "FileReservationDatabase"

test "Insert and query file reservation" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let expires := Chronos.Timestamp.fromSeconds 1700003600
  -- Create project
  let projectId ← db.insertProject "test" "/test" now
  -- Create agent
  let agent : Agent := {
    id := 0, projectId := projectId, name := "Agent1"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let agentId ← db.insertAgent agent
  -- Insert file reservation
  let reservationId ← db.insertFileReservation projectId agentId "src/**/*.lean" true "Refactoring" now expires
  reservationId ≡ (1 : Nat)
  -- Query by ID
  let found ← db.queryFileReservationById reservationId
  match found with
  | some r =>
    r.pathPattern ≡ "src/**/*.lean"
    r.exclusive ≡ true
    r.reason ≡ "Refactoring"
    shouldSatisfy r.releasedTs.isNone "releasedTs should be none"
  | none => throw (IO.userError "File reservation not found")
  db.close

test "Query active file reservations" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let expires := Chronos.Timestamp.fromSeconds 1700003600
  let projectId ← db.insertProject "test" "/test" now
  let agent : Agent := {
    id := 0, projectId := projectId, name := "Agent1"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let agentId ← db.insertAgent agent
  -- Insert active reservation
  let _ ← db.insertFileReservation projectId agentId "src/*.lean" true "" now expires
  -- Insert expired reservation
  let expiredTs := Chronos.Timestamp.fromSeconds 1699999000
  let _ ← db.insertFileReservation projectId agentId "docs/*.md" true "" expiredTs expiredTs
  -- Query active at 'now'
  let active ← db.queryActiveFileReservations projectId now
  active.size ≡ (1 : Nat)
  (active.getD 0 default).pathPattern ≡ "src/*.lean"
  db.close

test "Update file reservation released" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let expires := Chronos.Timestamp.fromSeconds 1700003600
  let projectId ← db.insertProject "test" "/test" now
  let agent : Agent := {
    id := 0, projectId := projectId, name := "Agent1"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let agentId ← db.insertAgent agent
  let reservationId ← db.insertFileReservation projectId agentId "*.lean" true "" now expires
  -- Release
  let released ← db.updateFileReservationReleased reservationId now
  shouldSatisfy released "should have released"
  -- Verify
  let found ← db.queryFileReservationById reservationId
  match found with
  | some r => shouldSatisfy r.releasedTs.isSome "releasedTs should be set"
  | none => throw (IO.userError "Reservation not found")
  -- Try to release again (should fail)
  let releasedAgain ← db.updateFileReservationReleased reservationId now
  shouldSatisfy (not releasedAgain) "should not release already released"
  db.close

test "Update file reservation expires" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let expires := Chronos.Timestamp.fromSeconds 1700003600
  let newExpires := Chronos.Timestamp.fromSeconds 2000000000
  let projectId ← db.insertProject "test" "/test" now
  let agent : Agent := {
    id := 0, projectId := projectId, name := "Agent1"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let agentId ← db.insertAgent agent
  let reservationId ← db.insertFileReservation projectId agentId "*.lean" true "" now expires
  -- Extend
  let updated ← db.updateFileReservationExpires reservationId newExpires
  shouldSatisfy updated "should have extended"
  -- Verify
  let found ← db.queryFileReservationById reservationId
  match found with
  | some r => r.expiresTs.seconds ≡ newExpires.seconds
  | none => throw (IO.userError "Reservation not found")
  db.close

test "Query reservations by agent" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let expires := Chronos.Timestamp.fromSeconds 1700003600
  let projectId ← db.insertProject "test" "/test" now
  let agent1 : Agent := {
    id := 0, projectId := projectId, name := "Agent1"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let agent1Id ← db.insertAgent agent1
  let agent2 : Agent := {
    id := 0, projectId := projectId, name := "Agent2"
    program := "test", model := "test", taskDescription := ""
    contactPolicy := ContactPolicy.auto
    attachmentsPolicy := AttachmentsPolicy.auto
    inceptionTs := now, lastActiveTs := now
  }
  let agent2Id ← db.insertAgent agent2
  -- Insert reservations
  let _ ← db.insertFileReservation projectId agent1Id "src/*.lean" true "" now expires
  let _ ← db.insertFileReservation projectId agent1Id "test/*.lean" false "" now expires
  let _ ← db.insertFileReservation projectId agent2Id "docs/*.md" true "" now expires
  -- Query by agent
  let agent1Reservations ← db.queryFileReservationsByAgent projectId agent1Id
  agent1Reservations.size ≡ (2 : Nat)
  let agent2Reservations ← db.queryFileReservationsByAgent projectId agent2Id
  agent2Reservations.size ≡ (1 : Nat)
  db.close

end Tests.FileReservationDatabase

namespace Tests.FileReservationTools

testSuite "FileReservationTools"

open Citadel
open AgentMail.Tools.FileReservations

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

def testConfig : Config := {
  Config.default with
  storageRoot := "/tmp/agent-mail-test-archive"
}

def parseJsonRpcResponse (resp : Response) : IO JsonRpc.Response := do
  let body := String.fromUTF8! resp.body
  let json := Lean.Json.parse body
  match json with
  | Except.ok j =>
      match (Lean.FromJson.fromJson? j : Except String JsonRpc.Response) with
      | Except.ok r => pure r
      | Except.error e => throw (IO.userError s!"Failed to decode JSON-RPC response: {e}")
  | Except.error e => throw (IO.userError s!"Failed to parse JSON: {e}")

test "patternsOverlap detects identical patterns" := do
  shouldSatisfy (patternsOverlap "src/*.lean" "src/*.lean") "identical patterns should overlap"

test "patternsOverlap detects prefix patterns" := do
  shouldSatisfy (patternsOverlap "src/" "src/foo.lean") "prefix should overlap"
  shouldSatisfy (patternsOverlap "src/foo.lean" "src/") "prefix should overlap (reverse)"

test "patternsOverlap with wildcards" := do
  shouldSatisfy (patternsOverlap "*.lean" "foo.lean") "wildcard at start overlaps"
  shouldSatisfy (patternsOverlap "src/*" "src/foo") "directory wildcard overlaps"

test "patternsOverlap non-overlapping" := do
  shouldSatisfy (not (patternsOverlap "src/foo.lean" "docs/bar.md")) "different dirs don't overlap"

test "file_reservation_paths grants reservations" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let projectId ← db.insertProject "p1" "/p1" now
  let _agent1Id ← db.insertAgent (mkAgent projectId "Agent1" now)
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("agent_name", Lean.Json.str "Agent1"),
    ("paths", Lean.toJson #["src/*.lean", "docs/*.md"]),
    ("ttl_seconds", Lean.Json.num 7200),
    ("exclusive", Lean.Json.bool true)
  ]
  let req : JsonRpc.Request := {
    method := "file_reservation_paths"
    params := some params
    id := some (JsonRpc.RequestId.num 1)
  }
  let resp ← handleFileReservationPaths db testConfig req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some result =>
      match result.getObjVal? "granted" with
      | Except.ok (Lean.Json.arr granted) =>
          granted.size ≡ (2 : Nat)
          match granted.toList with
          | entry :: _ =>
              match entry.getObjValAs? Bool "exclusive" with
              | Except.ok isExclusive => shouldSatisfy isExclusive "exclusive should be true"
              | Except.error e => throw (IO.userError s!"Expected exclusive: {e}")
          | [] => throw (IO.userError "Expected granted entry")
      | _ => throw (IO.userError "Expected granted array")
      match result.getObjVal? "conflicts" with
      | Except.ok (Lean.Json.arr conflicts) =>
          conflicts.size ≡ (0 : Nat)
      | _ => throw (IO.userError "Expected conflicts array")
  | none => throw (IO.userError "Expected result")
  db.close

test "file_reservation_paths detects conflicts" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  -- Use a far-future expiration to ensure reservation is still active
  let expires := Chronos.Timestamp.fromSeconds 2000000000  -- Year 2033
  let projectId ← db.insertProject "p1" "/p1" now
  let agent1Id ← db.insertAgent (mkAgent projectId "Agent1" now)
  let _agent2Id ← db.insertAgent (mkAgent projectId "Agent2" now)
  -- Agent1 reserves src/*.lean
  let _ ← db.insertFileReservation projectId agent1Id "src/*.lean" true "" now expires
  -- Agent2 tries to reserve overlapping pattern
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("agent_name", Lean.Json.str "Agent2"),
    ("paths", Lean.toJson #["src/*.lean", "docs/*.md"]),
    ("exclusive", Lean.Json.bool true)
  ]
  let req : JsonRpc.Request := {
    method := "file_reservation_paths"
    params := some params
    id := some (JsonRpc.RequestId.num 2)
  }
  let resp ← handleFileReservationPaths db testConfig req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some result =>
      match result.getObjVal? "granted" with
      | Except.ok (Lean.Json.arr granted) =>
          granted.size ≡ (2 : Nat)
      | _ => throw (IO.userError "Expected granted array")
      match result.getObjVal? "conflicts" with
      | Except.ok (Lean.Json.arr conflicts) =>
          conflicts.size ≡ (1 : Nat)
          match conflicts.toList with
          | conflictEntry :: _ =>
              match conflictEntry.getObjValAs? String "path" with
              | Except.ok path => path ≡ "src/*.lean"
              | Except.error e => throw (IO.userError s!"Expected conflict path: {e}")
              match conflictEntry.getObjVal? "holders" with
              | Except.ok (Lean.Json.arr holders) =>
                  holders.size ≡ (1 : Nat)
              | _ => throw (IO.userError "Expected holders array")
          | [] => throw (IO.userError "Expected conflict entry")
      | _ => throw (IO.userError "Expected conflicts array")
  | none => throw (IO.userError "Expected result")
  db.close

test "file_reservation_paths allows shared reservations" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let expires := Chronos.Timestamp.fromSeconds 2000000000
  let projectId ← db.insertProject "p1" "/p1" now
  let agent1Id ← db.insertAgent (mkAgent projectId "Agent1" now)
  let _agent2Id ← db.insertAgent (mkAgent projectId "Agent2" now)
  -- Agent1 reserves src/*.lean as NON-exclusive
  let _ ← db.insertFileReservation projectId agent1Id "src/*.lean" false "" now expires
  -- Agent2 tries to reserve same pattern as NON-exclusive
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("agent_name", Lean.Json.str "Agent2"),
    ("paths", Lean.toJson #["src/*.lean"]),
    ("exclusive", Lean.Json.bool false)
  ]
  let req : JsonRpc.Request := {
    method := "file_reservation_paths"
    params := some params
    id := some (JsonRpc.RequestId.num 3)
  }
  let resp ← handleFileReservationPaths db testConfig req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some result =>
      match result.getObjVal? "granted" with
      | Except.ok (Lean.Json.arr granted) =>
          -- Should be granted since both are non-exclusive
          granted.size ≡ (1 : Nat)
      | _ => throw (IO.userError "Expected granted array")
      match result.getObjVal? "conflicts" with
      | Except.ok (Lean.Json.arr conflicts) =>
          conflicts.size ≡ (0 : Nat)
      | _ => throw (IO.userError "Expected conflicts array")
  | none => throw (IO.userError "Expected result")
  db.close

test "release_file_reservations by IDs" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let expires := Chronos.Timestamp.fromSeconds 2000000000
  let projectId ← db.insertProject "p1" "/p1" now
  let agentId ← db.insertAgent (mkAgent projectId "Agent1" now)
  let resId ← db.insertFileReservation projectId agentId "src/*.lean" true "" now expires
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("agent_name", Lean.Json.str "Agent1"),
    ("file_reservation_ids", Lean.toJson #[resId])
  ]
  let req : JsonRpc.Request := {
    method := "release_file_reservations"
    params := some params
    id := some (JsonRpc.RequestId.num 4)
  }
  let resp ← handleReleaseFileReservations db testConfig req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some result =>
      match result.getObjValAs? Nat "released" with
      | Except.ok released => released ≡ (1 : Nat)
      | Except.error e => throw (IO.userError s!"Expected released count: {e}")
  | none => throw (IO.userError "Expected result")
  -- Verify released
  let found ← db.queryFileReservationById resId
  match found with
  | some r => shouldSatisfy r.releasedTs.isSome "should be released"
  | none => throw (IO.userError "Reservation not found")
  db.close

test "release_file_reservations by paths" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let expires := Chronos.Timestamp.fromSeconds 2000000000
  let projectId ← db.insertProject "p1" "/p1" now
  let agentId ← db.insertAgent (mkAgent projectId "Agent1" now)
  let _ ← db.insertFileReservation projectId agentId "src/*.lean" true "" now expires
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("agent_name", Lean.Json.str "Agent1"),
    ("paths", Lean.toJson #["src/*.lean"])
  ]
  let req : JsonRpc.Request := {
    method := "release_file_reservations"
    params := some params
    id := some (JsonRpc.RequestId.num 5)
  }
  let resp ← handleReleaseFileReservations db testConfig req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some result =>
      match result.getObjValAs? Nat "released" with
      | Except.ok released => released ≡ (1 : Nat)
      | Except.error e => throw (IO.userError s!"Expected released count: {e}")
  | none => throw (IO.userError "Expected result")
  db.close

test "renew_file_reservations extends TTL" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let expires := Chronos.Timestamp.fromSeconds 2000000000
  let projectId ← db.insertProject "p1" "/p1" now
  let agentId ← db.insertAgent (mkAgent projectId "Agent1" now)
  let resId ← db.insertFileReservation projectId agentId "src/*.lean" true "" now expires
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("agent_name", Lean.Json.str "Agent1"),
    ("file_reservation_ids", Lean.toJson #[resId]),
    ("extend_seconds", Lean.Json.num 3600)
  ]
  let req : JsonRpc.Request := {
    method := "renew_file_reservations"
    params := some params
    id := some (JsonRpc.RequestId.num 6)
  }
  let resp ← handleRenewFileReservations db testConfig req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some result =>
      match result.getObjValAs? Nat "renewed" with
      | Except.ok renewed => renewed ≡ (1 : Nat)
      | Except.error e => throw (IO.userError s!"Expected renewed count: {e}")
      match result.getObjVal? "file_reservations" with
      | Except.ok (Lean.Json.arr updated) =>
          updated.size ≡ (1 : Nat)
      | _ => throw (IO.userError "Expected file_reservations array")
  | none => throw (IO.userError "Expected result")
  -- Verify extended
  let found ← db.queryFileReservationById resId
  match found with
  | some r => r.expiresTs.seconds ≡ (2000000000 + 3600 : Int)
  | none => throw (IO.userError "Reservation not found")
  db.close

test "renew_file_reservations fails for other agent's reservations" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let expires := Chronos.Timestamp.fromSeconds 2000000000
  let projectId ← db.insertProject "p1" "/p1" now
  let agent1Id ← db.insertAgent (mkAgent projectId "Agent1" now)
  let _agent2Id ← db.insertAgent (mkAgent projectId "Agent2" now)
  let resId ← db.insertFileReservation projectId agent1Id "src/*.lean" true "" now expires
  -- Agent2 tries to renew Agent1's reservation
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("agent_name", Lean.Json.str "Agent2"),
    ("file_reservation_ids", Lean.toJson #[resId])
  ]
  let req : JsonRpc.Request := {
    method := "renew_file_reservations"
    params := some params
    id := some (JsonRpc.RequestId.num 7)
  }
  let resp ← handleRenewFileReservations db testConfig req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some result =>
      match result.getObjValAs? Nat "renewed" with
      | Except.ok renewed => renewed ≡ (0 : Nat)
      | Except.error e => throw (IO.userError s!"Expected renewed count: {e}")
      match result.getObjVal? "file_reservations" with
      | Except.ok (Lean.Json.arr updated) =>
          updated.size ≡ (0 : Nat)
      | _ => throw (IO.userError "Expected file_reservations array")
  | none => throw (IO.userError "Expected result")
  db.close

test "force_release_file_reservation releases any reservation" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let expires := Chronos.Timestamp.fromSeconds 1700003600
  let projectId ← db.insertProject "p1" "/p1" now
  let agentId ← db.insertAgent (mkAgent projectId "Agent1" now)
  let resId ← db.insertFileReservation projectId agentId "src/*.lean" true "" now expires
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("agent_name", Lean.Json.str "Agent1"),
    ("file_reservation_id", Lean.Json.num resId),
    ("note", Lean.Json.str "Admin override")
  ]
  let req : JsonRpc.Request := {
    method := "force_release_file_reservation"
    params := some params
    id := some (JsonRpc.RequestId.num 8)
  }
  let resp ← handleForceReleaseFileReservation db testConfig req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some result =>
      match result.getObjValAs? Nat "released" with
      | Except.ok released => released ≡ (1 : Nat)
      | Except.error e => throw (IO.userError s!"Expected released count: {e}")
      match result.getObjVal? "reservation" with
      | Except.ok reservation =>
          match reservation.getObjValAs? Nat "id" with
          | Except.ok rid => rid ≡ resId
          | Except.error e => throw (IO.userError s!"Expected reservation id: {e}")
      | _ => throw (IO.userError "Expected reservation summary")
  | none => throw (IO.userError "Expected result")
  -- Verify released
  let found ← db.queryFileReservationById resId
  match found with
  | some r => shouldSatisfy r.releasedTs.isSome "should be released"
  | none => throw (IO.userError "Reservation not found")
  db.close

test "force_release_file_reservation rejects wrong project" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let expires := Chronos.Timestamp.fromSeconds 1700003600
  let project1Id ← db.insertProject "p1" "/p1" now
  let project2Id ← db.insertProject "p2" "/p2" now
  let agent1Id ← db.insertAgent (mkAgent project1Id "Agent1" now)
  let _agent2Id ← db.insertAgent (mkAgent project2Id "Agent2" now)
  let resId ← db.insertFileReservation project1Id agent1Id "src/*.lean" true "" now expires
  -- Try to force release from wrong project
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p2"),
    ("agent_name", Lean.Json.str "Agent2"),
    ("file_reservation_id", Lean.Json.num resId),
    ("note", Lean.Json.str "Wrong project test")
  ]
  let req : JsonRpc.Request := {
    method := "force_release_file_reservation"
    params := some params
    id := some (JsonRpc.RequestId.num 9)
  }
  let resp ← handleForceReleaseFileReservation db testConfig req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.error with
  | some err =>
      err.message ≡ "Invalid params"
      match err.data with
      | some (Lean.Json.str details) =>
          details ≡ "reservation does not belong to this project"
      | _ => throw (IO.userError "Expected error details string")
  | none => throw (IO.userError "Expected error response")
  db.close

test "same agent can re-reserve own patterns" := do
  let db ← Storage.Database.openMemory
  let now := Chronos.Timestamp.fromSeconds 1700000000
  let expires := Chronos.Timestamp.fromSeconds 2000000000
  let projectId ← db.insertProject "p1" "/p1" now
  let agentId ← db.insertAgent (mkAgent projectId "Agent1" now)
  -- Agent1 reserves src/*.lean
  let _ ← db.insertFileReservation projectId agentId "src/*.lean" true "" now expires
  -- Agent1 tries to reserve overlapping pattern (should succeed - same agent)
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/p1"),
    ("agent_name", Lean.Json.str "Agent1"),
    ("paths", Lean.toJson #["src/*.lean"]),
    ("exclusive", Lean.Json.bool true)
  ]
  let req : JsonRpc.Request := {
    method := "file_reservation_paths"
    params := some params
    id := some (JsonRpc.RequestId.num 10)
  }
  let resp ← handleFileReservationPaths db testConfig req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.result with
  | some result =>
      match result.getObjVal? "granted" with
      | Except.ok (Lean.Json.arr granted) =>
          -- Should be granted since it's the same agent
          granted.size ≡ (1 : Nat)
      | _ => throw (IO.userError "Expected granted array")
      match result.getObjVal? "conflicts" with
      | Except.ok (Lean.Json.arr conflicts) =>
          conflicts.size ≡ (0 : Nat)
      | _ => throw (IO.userError "Expected conflicts array")
  | none => throw (IO.userError "Expected result")
  db.close

end Tests.FileReservationTools

namespace Tests.GitGuard

testSuite "GitGuard"

open AgentMail.Git.Guard

test "renderChainRunner contains marker" := do
  let script := renderChainRunner "pre-commit"
  shouldSatisfy (script.find? chainRunnerMarker |>.isSome) "should contain chain runner marker"
  shouldSatisfy (script.find? "pre-commit" |>.isSome) "should contain hook name"

test "renderChainRunner is valid python" := do
  let script := renderChainRunner "pre-push"
  shouldSatisfy (script.find? "#!/usr/bin/env python3" |>.isSome) "should have python shebang"
  shouldSatisfy (script.find? "hooks.d" |>.isSome) "should reference hooks.d directory"

test "renderPrecommitGuard contains marker" := do
  let script := renderPrecommitGuard "/tmp/archive" "/tmp/archive/file_reservations"
  shouldSatisfy (script.find? guardPluginMarker |>.isSome) "should contain guard plugin marker"
  shouldSatisfy (script.find? "#!/usr/bin/env python3" |>.isSome) "should have python shebang"

test "renderPrecommitGuard embeds config" := do
  let script := renderPrecommitGuard "/tmp/archive" "/tmp/archive/file_reservations"
  shouldSatisfy (script.find? "/tmp/archive" |>.isSome) "should contain storage root"
  shouldSatisfy (script.find? "/tmp/archive/file_reservations" |>.isSome) "should contain file reservations dir"

test "isChainRunnerContent detects chain runner" := do
  let script := renderChainRunner "pre-commit"
  shouldSatisfy (isChainRunnerContent script) "should detect chain runner content"
  shouldSatisfy (not (isChainRunnerContent "#!/bin/bash\nsome other script")) "should not detect non-chain-runner"
  shouldSatisfy (not (isChainRunnerContent "")) "should not detect empty content"

test "isGuardPluginContent detects guard plugin" := do
  let script := renderPrecommitGuard "/tmp/archive" "/tmp/archive/file_reservations"
  shouldSatisfy (isGuardPluginContent script) "should detect guard plugin content"
  shouldSatisfy (not (isGuardPluginContent "#!/usr/bin/env python3\nsome other script")) "should not detect non-guard"

test "InstallResult JSON serialization" := do
  let result : InstallResult := {
    hook := "/path/to/hooks/pre-commit"
  }
  let json := Lean.toJson result
  let str := Lean.Json.compress json
  shouldSatisfy (str.find? "\"hook\"" |>.isSome) "should contain hook"

test "UninstallResult JSON serialization" := do
  let result : UninstallResult := {
    removed := true
  }
  let json := Lean.toJson result
  let str := Lean.Json.compress json
  shouldSatisfy (str.find? "\"removed\":true" |>.isSome) "should contain removed"

end Tests.GitGuard

namespace Tests.GitGuardTools

testSuite "GitGuardTools"

open Citadel
open AgentMail.Tools.GitGuard

def testConfig : Config := {
  Config.default with
  storageRoot := "/tmp/agent-mail-test-archive"
  worktreesEnabled := true
}

def parseJsonRpcResponse (resp : Response) : IO JsonRpc.Response := do
  let body := String.fromUTF8! resp.body
  let json := Lean.Json.parse body
  match json with
  | Except.ok j =>
      match (Lean.FromJson.fromJson? j : Except String JsonRpc.Response) with
      | Except.ok r => pure r
      | Except.error e => throw (IO.userError s!"Failed to decode JSON-RPC response: {e}")
  | Except.error e => throw (IO.userError s!"Failed to parse JSON: {e}")

test "install_precommit_guard requires project_key" := do
  let db ← Storage.Database.openMemory
  let params := Lean.Json.mkObj [
    ("code_repo_path", Lean.Json.str "/tmp/test")
  ]
  let req : JsonRpc.Request := {
    method := "install_precommit_guard"
    params := some params
    id := some (JsonRpc.RequestId.num 1)
  }
  let resp ← handleInstallPrecommitGuard db testConfig req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.error with
  | some err =>
      err.message ≡ "Invalid params"
      match err.data with
      | some (Lean.Json.str details) =>
          details ≡ "missing required param: project_key"
      | _ => throw (IO.userError "Expected error details string")
  | none => throw (IO.userError "Expected error response")
  db.close

test "install_precommit_guard requires code_repo_path" := do
  let db ← Storage.Database.openMemory
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/test")
  ]
  let req : JsonRpc.Request := {
    method := "install_precommit_guard"
    params := some params
    id := some (JsonRpc.RequestId.num 2)
  }
  let resp ← handleInstallPrecommitGuard db testConfig req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.error with
  | some err =>
      err.message ≡ "Invalid params"
      match err.data with
      | some (Lean.Json.str details) =>
          details ≡ "missing required param: code_repo_path"
      | _ => throw (IO.userError "Expected error details string")
  | none => throw (IO.userError "Expected error response")
  db.close

test "install_precommit_guard validates project exists" := do
  let db ← Storage.Database.openMemory
  let params := Lean.Json.mkObj [
    ("project_key", Lean.Json.str "/nonexistent"),
    ("code_repo_path", Lean.Json.str "/tmp/test")
  ]
  let req : JsonRpc.Request := {
    method := "install_precommit_guard"
    params := some params
    id := some (JsonRpc.RequestId.num 3)
  }
  let resp ← handleInstallPrecommitGuard db testConfig req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.error with
  | some err =>
      err.message ≡ "Invalid params"
      match err.data with
      | some (Lean.Json.str details) =>
          details ≡ "project not found: /nonexistent"
      | _ => throw (IO.userError "Expected error details string")
  | none => throw (IO.userError "Expected error response")
  db.close

test "uninstall_precommit_guard requires code_repo_path" := do
  let db ← Storage.Database.openMemory
  let params := Lean.Json.mkObj [
  ]
  let req : JsonRpc.Request := {
    method := "uninstall_precommit_guard"
    params := some params
    id := some (JsonRpc.RequestId.num 4)
  }
  let resp ← handleUninstallPrecommitGuard db testConfig req
  let rpcResp ← parseJsonRpcResponse resp
  match rpcResp.error with
  | some err =>
      err.message ≡ "Invalid params"
      match err.data with
      | some (Lean.Json.str details) =>
          details ≡ "missing required param: code_repo_path"
      | _ => throw (IO.userError "Expected error details string")
  | none => throw (IO.userError "Expected error response")
  db.close

end Tests.GitGuardTools

def main : IO UInt32 := runAllSuites
