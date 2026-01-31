import Crucible
import AgentMail

open Crucible
open AgentMail

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
