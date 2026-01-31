import Crucible
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
