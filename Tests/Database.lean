import Crucible
import AgentMail

open Crucible
open AgentMail

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
