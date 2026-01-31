import Crucible
import AgentMail

open Crucible
open AgentMail

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
