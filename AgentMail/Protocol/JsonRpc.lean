/-
  AgentMail.Protocol.JsonRpc - JSON-RPC 2.0 protocol types
-/
import Lean.Data.Json

namespace AgentMail.JsonRpc

/-- JSON-RPC request ID - can be a number or string -/
inductive RequestId where
  | num : Int → RequestId
  | str : String → RequestId
  deriving Repr, DecidableEq

namespace RequestId

instance : Lean.ToJson RequestId where
  toJson
    | num n => Lean.Json.num n
    | str s => Lean.Json.str s

instance : Lean.FromJson RequestId where
  fromJson? j :=
    match j.getInt? with
    | Except.ok n => pure (num n)
    | Except.error _ =>
      match j.getStr? with
      | Except.ok s => pure (str s)
      | Except.error e => throw e

end RequestId

/-- Standard JSON-RPC 2.0 error codes -/
def errorParseError : Int := -32700
def errorInvalidRequest : Int := -32600
def errorMethodNotFound : Int := -32601
def errorInvalidParams : Int := -32602
def errorInternalError : Int := -32603

/-- JSON-RPC 2.0 request -/
structure Request where
  jsonrpc : String := "2.0"
  method : String
  params : Option Lean.Json := none
  id : Option RequestId := none

namespace Request

instance : Lean.FromJson Request where
  fromJson? j := do
    let jsonrpc ← j.getObjValAs? String "jsonrpc"
    if jsonrpc != "2.0" then
      throw "jsonrpc must be \"2.0\""
    let method ← j.getObjValAs? String "method"
    let params : Option Lean.Json := match j.getObjVal? "params" with
      | Except.ok v => if v.isNull then none else some v
      | Except.error _ => none
    let id : Option RequestId := match j.getObjVal? "id" with
      | Except.ok v =>
        if v.isNull then none
        else match Lean.FromJson.fromJson? v with
          | Except.ok rid => some rid
          | Except.error _ => none
      | Except.error _ => none
    pure { jsonrpc, method, params, id }

instance : Lean.ToJson Request where
  toJson r := Lean.Json.mkObj <|
    [("jsonrpc", Lean.Json.str r.jsonrpc),
     ("method", Lean.Json.str r.method)] ++
    (match r.params with
      | some p => [("params", p)]
      | none => []) ++
    (match r.id with
      | some id => [("id", Lean.toJson id)]
      | none => [])

/-- Check if this is a notification (no id) -/
def isNotification (r : Request) : Bool := r.id.isNone

end Request

/-- JSON-RPC 2.0 error object -/
structure Error where
  code : Int
  message : String
  data : Option Lean.Json := none

namespace Error

instance : Lean.ToJson Error where
  toJson e := Lean.Json.mkObj <|
    [("code", Lean.Json.num e.code),
     ("message", Lean.Json.str e.message)] ++
    (match e.data with
      | some d => [("data", d)]
      | none => [])

instance : Lean.FromJson Error where
  fromJson? j := do
    let code ← j.getObjValAs? Int "code"
    let message ← j.getObjValAs? String "message"
    let data : Option Lean.Json := match j.getObjVal? "data" with
      | Except.ok v => if v.isNull then none else some v
      | Except.error _ => none
    pure { code, message, data }

/-- Create a parse error -/
def parseError (details : Option String := none) : Error :=
  { code := errorParseError
  , message := "Parse error"
  , data := details.map Lean.Json.str }

/-- Create an invalid request error -/
def invalidRequest (details : Option String := none) : Error :=
  { code := errorInvalidRequest
  , message := "Invalid Request"
  , data := details.map Lean.Json.str }

/-- Create a method not found error -/
def methodNotFound (method : String) : Error :=
  { code := errorMethodNotFound
  , message := s!"Method not found: {method}"
  , data := none }

/-- Create an invalid params error -/
def invalidParams (details : Option String := none) : Error :=
  { code := errorInvalidParams
  , message := "Invalid params"
  , data := details.map Lean.Json.str }

/-- Create an internal error -/
def internalError (details : Option String := none) : Error :=
  { code := errorInternalError
  , message := "Internal error"
  , data := details.map Lean.Json.str }

end Error

/-- JSON-RPC 2.0 response -/
structure Response where
  jsonrpc : String := "2.0"
  result : Option Lean.Json := none
  error : Option Error := none
  id : Option RequestId

namespace Response

instance : Lean.ToJson Response where
  toJson r := Lean.Json.mkObj <|
    [("jsonrpc", Lean.Json.str r.jsonrpc)] ++
    (match r.result with
      | some res => [("result", res)]
      | none => []) ++
    (match r.error with
      | some err => [("error", Lean.toJson err)]
      | none => []) ++
    [("id", match r.id with
      | some id => Lean.toJson id
      | none => Lean.Json.null)]

instance : Lean.FromJson Response where
  fromJson? j := do
    let jsonrpc ← j.getObjValAs? String "jsonrpc"
    let result : Option Lean.Json := match j.getObjVal? "result" with
      | Except.ok v => if v.isNull then none else some v
      | Except.error _ => none
    let error : Option Error := match j.getObjVal? "error" with
      | Except.ok v =>
        if v.isNull then none
        else match Lean.FromJson.fromJson? v with
          | Except.ok e => some e
          | Except.error _ => none
      | Except.error _ => none
    let id : Option RequestId := match j.getObjVal? "id" with
      | Except.ok v =>
        if v.isNull then none
        else match Lean.FromJson.fromJson? v with
          | Except.ok rid => some rid
          | Except.error _ => none
      | Except.error _ => none
    pure { jsonrpc, result, error, id }

/-- Create a success response -/
def success (id : Option RequestId) (result : Lean.Json) : Response :=
  { result := some result, id := id }

/-- Create an error response -/
def failure (id : Option RequestId) (error : Error) : Response :=
  { error := some error, id := id }

end Response

end AgentMail.JsonRpc
