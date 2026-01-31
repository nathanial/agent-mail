import Crucible
import AgentMail

open Crucible
open AgentMail

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
