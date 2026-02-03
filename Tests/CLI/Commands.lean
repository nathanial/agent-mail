/-
  Tests.CLI.Commands - Tests for CLI command parsing and output formatting.
-/
import Crucible
import AgentMail.CLI.Commands
import AgentMail.CLI.Output
import Chronos
import Staple

open Crucible
open AgentMail.CLI
open AgentMail.CLI.Output
open Parlance
open Staple (String.containsSubstr)

namespace Tests.CLI.Commands

testSuite "CLI.Commands"

test "agentMailCommand parses list-projects" := do
  match Parlance.parse agentMailCommand ["list-projects"] with
  | .ok result =>
    result.commandPath ≡ ["list-projects"]
  | .error msg => throw (IO.userError s!"Parse failed: {msg}")

test "agentMailCommand parses list-projects with json flag" := do
  match Parlance.parse agentMailCommand ["-j", "list-projects"] with
  | .ok result =>
    result.commandPath ≡ ["list-projects"]
    (result.getBool "json") ≡ true
  | .error msg => throw (IO.userError s!"Parse failed: {msg}")

test "agentMailCommand parses list-acks" := do
  match Parlance.parse agentMailCommand ["list-acks", "--project", "my-project", "--agent", "alice"] with
  | .ok result =>
    result.commandPath ≡ ["list-acks"]
    (result.get (α := String) "project") ≡ some "my-project"
    (result.get (α := String) "agent") ≡ some "alice"
  | .error msg => throw (IO.userError s!"Parse failed: {msg}")

test "agentMailCommand list-acks requires project and agent" := do
  match Parlance.parse agentMailCommand ["list-acks", "--project", "my-project"] with
  | .ok _ => throw (IO.userError "Expected parse error for missing --agent")
  | .error _ => pure ()

test "agentMailCommand parses list-acks with limit" := do
  match Parlance.parse agentMailCommand ["list-acks", "--project", "my-project", "--agent", "alice", "--limit", "15"] with
  | .ok result =>
    result.commandPath ≡ ["list-acks"]
    (result.getNat "limit") ≡ some 15
  | .error msg => throw (IO.userError s!"Parse failed: {msg}")

test "agentMailCommand parses config show-port" := do
  match Parlance.parse agentMailCommand ["config", "show-port"] with
  | .ok result =>
    result.commandPath ≡ ["config", "show-port"]
  | .error msg => throw (IO.userError s!"Parse failed: {msg}")

test "agentMailCommand parses config set-port" := do
  match Parlance.parse agentMailCommand ["config", "set-port", "9000"] with
  | .ok result =>
    result.commandPath ≡ ["config", "set-port"]
    (result.getNat "port") ≡ some 9000
  | .error msg => throw (IO.userError s!"Parse failed: {msg}")

test "agentMailCommand parses doctor check" := do
  match Parlance.parse agentMailCommand ["doctor", "check"] with
  | .ok result =>
    result.commandPath ≡ ["doctor", "check"]
  | .error msg => throw (IO.userError s!"Parse failed: {msg}")

test "agentMailCommand parses doctor repair" := do
  match Parlance.parse agentMailCommand ["doctor", "repair"] with
  | .ok result =>
    result.commandPath ≡ ["doctor", "repair"]
  | .error msg => throw (IO.userError s!"Parse failed: {msg}")

test "agentMailCommand parses clear-and-reset with force" := do
  match Parlance.parse agentMailCommand ["clear-and-reset", "--force"] with
  | .ok result =>
    result.commandPath ≡ ["clear-and-reset"]
    (result.getBool "force") ≡ true
  | .error msg => throw (IO.userError s!"Parse failed: {msg}")

test "agentMailCommand parses serve" := do
  match Parlance.parse agentMailCommand ["serve"] with
  | .ok result =>
    result.commandPath ≡ ["serve"]
  | .error msg => throw (IO.userError s!"Parse failed: {msg}")

test "agentMailCommand empty args parses as root" := do
  match Parlance.parse agentMailCommand [] with
  | .ok result =>
    result.commandPath ≡ ([] : List String)
  | .error msg => throw (IO.userError s!"Parse failed: {msg}")

testSuite "CLI.Output"

test "formatProjects text mode with empty list" := do
  let result := formatProjects #[] .text
  result ≡ "No projects found."

test "formatProjects text mode with projects" := do
  let ts := Chronos.Timestamp.fromSeconds 1700000000
  let projects := #[
    { id := 1, slug := "proj1", humanKey := "/path/to/proj1", createdAt := ts },
    { id := 2, slug := "proj2", humanKey := "/path/to/proj2", createdAt := ts }
  ]
  let result := formatProjects projects .text
  shouldSatisfy (result.containsSubstr "Found 2 project(s)") "contains count"
  shouldSatisfy (result.containsSubstr "proj1") "contains proj1"
  shouldSatisfy (result.containsSubstr "proj2") "contains proj2"

test "formatProjects json mode" := do
  let ts := Chronos.Timestamp.fromSeconds 1700000000
  let projects := #[
    { id := 1, slug := "proj1", humanKey := "/path/to/proj1", createdAt := ts }
  ]
  let result := formatProjects projects .json
  shouldSatisfy (result.containsSubstr "\"slug\":\"proj1\"") "contains slug"
  shouldSatisfy (result.containsSubstr "\"id\":1") "contains id"

test "formatAcks text mode with empty list" := do
  let result := formatAcks #[] .text
  result ≡ "No pending acknowledgements."

test "formatAcks text mode with acks" := do
  let ts := Chronos.Timestamp.fromSeconds 1700000000
  let acks := #[{
    messageId := 42,
    projectSlug := "myproj",
    senderName := "alice",
    recipientName := "bob",
    subject := "Test message",
    createdTs := ts
  }]
  let result := formatAcks acks .text
  shouldSatisfy (result.containsSubstr "Found 1 pending acknowledgement(s)") "contains count"
  shouldSatisfy (result.containsSubstr "myproj") "contains project"
  shouldSatisfy (result.containsSubstr "alice") "contains sender"
  shouldSatisfy (result.containsSubstr "bob") "contains recipient"

test "formatPort text mode" := do
  let result := formatPort 8765 .text
  result ≡ "Server port: 8765"

test "formatPort json mode" := do
  let result := formatPort 8765 .json
  result ≡ "{\"port\": 8765}"

test "formatSuccess text mode" := do
  let result := formatSuccess "Operation completed" .text
  result ≡ "Operation completed"

test "formatSuccess json mode" := do
  let result := formatSuccess "Operation completed" .json
  shouldSatisfy (result.containsSubstr "\"status\": \"success\"") "contains status"
  shouldSatisfy (result.containsSubstr "\"message\": \"Operation completed\"") "contains message"

test "formatError text mode without suggestion" := do
  let result := formatError "Something failed" .text
  result ≡ "Error: Something failed"

test "formatError text mode with suggestion" := do
  let result := formatError "Something failed" .text (some "Try again")
  shouldSatisfy (result.containsSubstr "Error: Something failed") "contains error"
  shouldSatisfy (result.containsSubstr "Hint: Try again") "contains hint"

test "formatError json mode" := do
  let result := formatError "Something failed" .json (some "Try again")
  shouldSatisfy (result.containsSubstr "\"status\": \"error\"") "contains status"
  shouldSatisfy (result.containsSubstr "\"message\": \"Something failed\"") "contains message"
  shouldSatisfy (result.containsSubstr "\"suggestion\": \"Try again\"") "contains suggestion"

test "formatDoctorResult text mode healthy" := do
  let result := formatDoctorResult {
    integrityOk := true,
    orphanAgents := 0,
    orphanMessages := 0,
    orphanRecipients := 0
  } .text
  shouldSatisfy (result.containsSubstr "[OK]") "contains OK"
  shouldSatisfy (result.containsSubstr "Database is healthy") "contains healthy"

test "formatDoctorResult text mode with issues" := do
  let result := formatDoctorResult {
    integrityOk := true,
    orphanAgents := 2,
    orphanMessages := 0,
    orphanRecipients := 1
  } .text
  shouldSatisfy (result.containsSubstr "Orphan agents: 2") "contains orphan agents"
  shouldSatisfy (result.containsSubstr "Orphan recipients: 1") "contains orphan recipients"
  shouldSatisfy (result.containsSubstr "doctor repair") "contains repair hint"

test "formatDoctorResult json mode" := do
  let result := formatDoctorResult {
    integrityOk := true,
    orphanAgents := 0,
    orphanMessages := 0,
    orphanRecipients := 0
  } .json
  shouldSatisfy (result.containsSubstr "\"integrity_ok\":true") "contains integrity"
  shouldSatisfy (result.containsSubstr "\"orphan_agents\":0") "contains orphan_agents"

end Tests.CLI.Commands
