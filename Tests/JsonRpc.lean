import Crucible
import AgentMail

open Crucible
open AgentMail

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
