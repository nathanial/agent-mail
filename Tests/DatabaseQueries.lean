import Crucible
import AgentMail

open Crucible
open AgentMail

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
