import Crucible
import AgentMail

open Crucible
open AgentMail

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
