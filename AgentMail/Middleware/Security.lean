/-
  AgentMail.Middleware.Security - JWT authentication and RBAC enforcement
-/
import Citadel
import Chronos
import Wisp.HTTP.Client
import AgentMail.Config

open Citadel

namespace AgentMail.Middleware.Security

private def base64Chars : String :=
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

private def charAtIndex (s : String) (i : Nat) : Char :=
  s.data[i]!

private def base64Encode (data : ByteArray) : String := Id.run do
  if data.size == 0 then return ""
  let mut result : String := ""
  let mut i := 0
  while i + 2 < data.size do
    let b0 := data.data[i]!.toNat
    let b1 := data.data[i + 1]!.toNat
    let b2 := data.data[i + 2]!.toNat
    let c0 := b0 >>> 2
    let c1 := ((b0 &&& 0x03) <<< 4) ||| (b1 >>> 4)
    let c2 := ((b1 &&& 0x0F) <<< 2) ||| (b2 >>> 6)
    let c3 := b2 &&& 0x3F
    result := result.push (charAtIndex base64Chars c0)
    result := result.push (charAtIndex base64Chars c1)
    result := result.push (charAtIndex base64Chars c2)
    result := result.push (charAtIndex base64Chars c3)
    i := i + 3
  let remaining := data.size - i
  if remaining == 1 then
    let b0 := data.data[i]!.toNat
    let c0 := b0 >>> 2
    let c1 := (b0 &&& 0x03) <<< 4
    result := result.push (charAtIndex base64Chars c0)
    result := result.push (charAtIndex base64Chars c1)
    result := result ++ "=="
  else if remaining == 2 then
    let b0 := data.data[i]!.toNat
    let b1 := data.data[i + 1]!.toNat
    let c0 := b0 >>> 2
    let c1 := ((b0 &&& 0x03) <<< 4) ||| (b1 >>> 4)
    let c2 := (b1 &&& 0x0F) <<< 2
    result := result.push (charAtIndex base64Chars c0)
    result := result.push (charAtIndex base64Chars c1)
    result := result.push (charAtIndex base64Chars c2)
    result := result ++ "="
  return result

private def base64DecodeTable : Array UInt8 := Id.run do
  let mut table : Array UInt8 := Array.replicate 256 255
  for i in [:26] do
    table := table.set! ('A'.toNat + i) i.toUInt8
  for i in [:26] do
    table := table.set! ('a'.toNat + i) (i + 26).toUInt8
  for i in [:10] do
    table := table.set! ('0'.toNat + i) (i + 52).toUInt8
  table := table.set! '+'.toNat 62
  table := table.set! '/'.toNat 63
  return table

private def base64Decode (s : String) : Option ByteArray := Id.run do
  let chars := s.toList.filter fun c => c != '\n' && c != '\r' && c != ' '
  let mut result := ByteArray.empty
  let mut buffer : UInt32 := 0
  let mut bits : Nat := 0
  for c in chars do
    if c == '=' then
      continue
    let idx := c.toNat
    if idx >= 256 then return none
    let val := base64DecodeTable[idx]!
    if val == 255 then return none
    buffer := (buffer <<< 6) ||| val.toUInt32
    bits := bits + 6
    if bits >= 8 then
      bits := bits - 8
      let byte := ((buffer >>> bits.toUInt32) &&& 0xFF).toUInt8
      result := result.push byte
  return some result

private def base64UrlDecode (s : String) : Option ByteArray :=
  let normalized := s.replace "-" "+" |>.replace "_" "/"
  let padded :=
    match normalized.length % 4 with
    | 0 => normalized
    | 2 => normalized ++ "=="
    | 3 => normalized ++ "="
    | _ => normalized
  base64Decode padded

private def base64UrlEncode (data : ByteArray) : String :=
  let b64 := base64Encode data
  let noPad := b64.replace "=" ""
  noPad.replace "+" "-" |>.replace "/" "_"

private def hexVal (c : Char) : Option UInt8 :=
  if '0' ≤ c && c ≤ '9' then
    some (c.toNat.toUInt8 - '0'.toNat.toUInt8)
  else if 'a' ≤ c && c ≤ 'f' then
    some (c.toNat.toUInt8 - 'a'.toNat.toUInt8 + 10)
  else if 'A' ≤ c && c ≤ 'F' then
    some (c.toNat.toUInt8 - 'A'.toNat.toUInt8 + 10)
  else
    none

private def hexToBytes (hex : String) : Option ByteArray := Id.run do
  let chars := hex.trim.toList
  if chars.length % 2 != 0 then return none
  let mut out := ByteArray.empty
  let mut i := 0
  while i < chars.length do
    let c1 := chars[i]!
    let c2 := chars[i + 1]!
    match hexVal c1, hexVal c2 with
    | some h1, some h2 =>
      let byte := (h1 <<< 4) + h2
      out := out.push byte
    | _, _ => return none
    i := i + 2
  return some out

private def bytesToHex (bytes : ByteArray) : String := Id.run do
  let hexChars := "0123456789abcdef"
  let mut out := ""
  for b in bytes.toList do
    let hi := (b.toNat >>> 4) &&& 0xF
    let lo := b.toNat &&& 0xF
    out := out.push (charAtIndex hexChars hi)
    out := out.push (charAtIndex hexChars lo)
  return out

private def parseDigestHex (output : String) : Option String :=
  let trimmed := output.trim
  if trimmed.contains '=' then
    trimmed.splitOn "=" |>.getLast?
  else if trimmed.contains ' ' then
    trimmed.splitOn " " |>.getLast?
  else
    some trimmed

private def hmacSha256Base64Url (keyBytes : ByteArray) (message : String) : IO (Option String) := do
  let keyHex := bytesToHex keyBytes
  let out ← IO.Process.output
    { cmd := "openssl"
    , args := #["dgst", "-sha256", "-mac", "HMAC", "-macopt", s!"hexkey:{keyHex}"]
    } (some message)
  if out.exitCode != 0 then
    return none
  let hex := (parseDigestHex out.stdout).getD ""
  match hexToBytes hex with
  | some bytes => return some (base64UrlEncode bytes)
  | none => return none

private def decodeJsonSegment (segment : String) : Option Lean.Json := do
  let bytes ← base64UrlDecode segment
  let str ← String.fromUTF8? bytes
  match Lean.Json.parse str with
  | Except.ok json => some json
  | Except.error _ => none

private def getStringField (json : Lean.Json) (key : String) : Option String :=
  match json.getObjVal? key with
  | Except.ok (Lean.Json.str s) => some s
  | _ => none

private def getArrayField (json : Lean.Json) (key : String) : Option (Array Lean.Json) :=
  match json.getObjVal? key with
  | Except.ok (Lean.Json.arr arr) => some arr
  | _ => none

private def matchesAudience (claims : Lean.Json) (audience : String) : Bool :=
  match claims.getObjVal? "aud" with
  | Except.ok (Lean.Json.str s) => s == audience
  | Except.ok (Lean.Json.arr arr) =>
    arr.toList.any fun j =>
      match j with
      | Lean.Json.str s => s == audience
      | _ => false
  | _ => false

private def validateClaims (claims : Lean.Json) (cfg : AgentMail.Config) : IO Bool := do
  let now ← Chronos.Timestamp.now
  let nowSecs : Int := now.seconds
  let expOk := match claims.getObjValAs? Int "exp" with
    | Except.ok exp => exp > nowSecs
    | Except.error _ => true
  let nbfOk := match claims.getObjValAs? Int "nbf" with
    | Except.ok nbf => nbf <= nowSecs
    | Except.error _ => true
  let audOk := match cfg.http.jwtAudience with
    | some aud => matchesAudience claims aud
    | none => true
  let issOk := match cfg.http.jwtIssuer with
    | some iss =>
      match getStringField claims "iss" with
      | some s => s == iss
      | none => false
    | none => true
  pure (expOk && nbfOk && audOk && issOk)

private def fetchJwks (url : String) : IO (Option Lean.Json) := do
  let client := Wisp.HTTP.Client.new
  let task ← Wisp.HTTP.Client.get client url
  let result := task.get
  match result with
  | .error _ => pure none
  | .ok resp =>
    if !Wisp.Response.isSuccess resp then
      pure none
    else
      let body := resp.bodyTextLossy
      match Lean.Json.parse body with
      | Except.ok json => pure (some json)
      | Except.error _ => pure none

private def selectJwk (jwks : Lean.Json) (kid : Option String) : Option Lean.Json := do
  let keys ← getArrayField jwks "keys"
  match kid with
  | some k =>
    keys.find? fun j =>
      match getStringField j "kid" with
      | some kidVal => kidVal == k
      | none => false
  | none =>
    keys.toList.head?

private def jwkSecret (jwk : Lean.Json) : Option ByteArray := do
  let kty ← getStringField jwk "kty"
  if kty != "oct" then
    none
  else
    let k ← getStringField jwk "k"
    base64UrlDecode k

private def decodeJwt (token : String) (cfg : AgentMail.Config) : IO (Option Lean.Json) := do
  let parts := (token.splitOn ".").toArray
  if parts.size != 3 then
    return none
  let headerSeg := parts[0]!
  let payloadSeg := parts[1]!
  let sigSeg := parts[2]!
  let headerJson := decodeJsonSegment headerSeg
  let payloadJson := decodeJsonSegment payloadSeg
  match headerJson, payloadJson with
  | some headerJson, some payloadJson =>
    let alg := getStringField headerJson "alg"
    let kid := getStringField headerJson "kid"
    let allowed := cfg.http.jwtAlgorithms.map String.toUpper
    let algUpper := alg.map String.toUpper
    if algUpper.isNone || !allowed.contains algUpper.get! then
      return none
    let keyBytes? ←
      match cfg.http.jwtJwksUrl with
      | some url => do
          match ← fetchJwks url with
          | some jwks =>
            pure (selectJwk jwks kid >>= jwkSecret)
          | none => pure none
      | none =>
        pure (cfg.http.jwtSecret.map String.toUTF8)
    match keyBytes? with
    | none => return none
    | some keyBytes =>
      let message := s!"{headerSeg}.{payloadSeg}"
      let sigExpected ← hmacSha256Base64Url keyBytes message
      if sigExpected.isNone || sigExpected.get! != sigSeg then
        return none
      if !(← validateClaims payloadJson cfg) then
        return none
      return some payloadJson
  | _, _ => return none

private def extractRoles (claims : Lean.Json) (claimName : String) : List String :=
  match claims.getObjVal? claimName with
  | Except.ok (Lean.Json.str s) => [s]
  | Except.ok (Lean.Json.arr arr) =>
    arr.toList.filterMap fun j =>
      match j with
      | Lean.Json.str s => some s
      | _ => none
  | _ => []

private def isLocalhost (req : ServerRequest) : Bool :=
  match req.header "Host" with
  | some host =>
    host.startsWith "localhost" ||
    host.startsWith "127.0.0.1" ||
    host.startsWith "[::1]"
  | none => false

private def classify (req : ServerRequest) : IO (String × Option String) := do
  if req.path.startsWith "/resource" then
    return ("resources", none)
  if req.path == "/rpc" then
    let body := req.bodyString
    match Lean.Json.parse body with
    | Except.ok json =>
      match json.getObjValAs? String "method" with
      | Except.ok method => return ("tools", some method)
      | Except.error _ => return ("tools", none)
    | Except.error _ => return ("tools", none)
  return ("other", none)

/-- JWT authentication + RBAC middleware. -/
def jwtRbac (cfg : AgentMail.Config) : Citadel.Middleware :=
  fun handler req => do
    if req.method == .OPTIONS || req.path == "/health" || req.path.startsWith "/health/" then
      return ← handler req
    let (kind, toolName) ← classify req
    let mut roles : List String := []
    if cfg.http.jwtEnabled then
      match req.header "Authorization" with
      | some auth =>
        if !auth.startsWith "Bearer " then
          return Response.unauthorized "Unauthorized"
        let token := auth.drop 7
        match ← decodeJwt token cfg with
        | some claims =>
          roles := extractRoles claims cfg.http.jwtRoleClaim
          if roles.isEmpty then
            roles := [cfg.http.rbacDefaultRole]
        | none =>
          return Response.unauthorized "Unauthorized"
      | none =>
        return Response.unauthorized "Unauthorized"
    else
      roles := [cfg.http.rbacDefaultRole]
      if cfg.http.allowLocalhostUnauthenticated && isLocalhost req then
        roles := roles ++ ["writer"]

    let isLocalOk := cfg.http.allowLocalhostUnauthenticated && isLocalhost req
    if cfg.http.rbacEnabled && !isLocalOk && (kind == "tools" || kind == "resources") then
      let isReader := roles.any (fun r => cfg.http.rbacReaderRoles.contains r)
      let isWriter := roles.any (fun r => cfg.http.rbacWriterRoles.contains r) || roles.isEmpty
      if kind == "tools" then
        match toolName with
        | some t =>
          if cfg.http.rbacReadonlyTools.contains t then
            if !(isReader || isWriter) then
              return Response.forbidden "Forbidden"
          else
            if !isWriter then
              return Response.forbidden "Forbidden"
        | none =>
          if !isWriter then
            return Response.forbidden "Forbidden"
      else
        if !(isReader || isWriter) then
          return Response.forbidden "Forbidden"

    handler req

end AgentMail.Middleware.Security
