import Crucible
import AgentMail

open Crucible
open AgentMail

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
