/-
  AgentMail.Config - Server configuration
-/
import AgentMail.Middleware.RateLimit
import AgentMail.Middleware.CORS
import AgentMail.ToolFilter
import AgentMail.Notifications

namespace AgentMail

/-- HTTP authentication and rate limiting configuration -/
structure HttpConfig where
  /-- Bearer token for authentication (none = no auth required) -/
  bearerToken : Option String := none
  /-- Allow localhost requests without authentication -/
  allowLocalhostUnauthenticated : Bool := true
  /-- Rate limiting configuration -/
  rateLimit : Middleware.RateLimit.RateLimitConfig := {}
  /-- Enable JWT authentication -/
  jwtEnabled : Bool := false
  /-- Allowed JWT algorithms (e.g., HS256) -/
  jwtAlgorithms : List String := ["HS256"]
  /-- JWT shared secret for HS256 (optional) -/
  jwtSecret : Option String := none
  /-- JWT JWKS URL for key discovery (optional) -/
  jwtJwksUrl : Option String := none
  /-- JWT audience claim (optional) -/
  jwtAudience : Option String := none
  /-- JWT issuer claim (optional) -/
  jwtIssuer : Option String := none
  /-- JWT role claim name -/
  jwtRoleClaim : String := "roles"
  /-- Enable RBAC enforcement -/
  rbacEnabled : Bool := true
  /-- Roles allowed to read resources and readonly tools -/
  rbacReaderRoles : List String := []
  /-- Roles allowed to call write tools -/
  rbacWriterRoles : List String := []
  /-- Default role when no roles are present -/
  rbacDefaultRole : String := "tools"
  /-- Tools that are allowed for read-only roles -/
  rbacReadonlyTools : List String := []
  deriving Repr, Inhabited

namespace HttpConfig

/-- Default HTTP configuration -/
def default : HttpConfig := {}

end HttpConfig

/-- Server configuration -/
structure Config where
  environment : String := "development"
  port : UInt16 := 8765
  host : String := "127.0.0.1"
  databasePath : String := "agent_mail.db"
  storageRoot : String := "~/.mcp_agent_mail_git_mailbox_repo"
  gitAuthorName : String := "mcp-agent"
  gitAuthorEmail : String := "mcp-agent@example.com"
  worktreesEnabled : Bool := false
  authToken : Option String := none  -- Deprecated, use http.bearerToken
  /-- HTTP authentication and rate limiting -/
  http : HttpConfig := {}
  /-- CORS configuration -/
  cors : Middleware.CORS.CorsConfig := {}
  /-- Tool filtering configuration -/
  toolFilter : ToolFilter.ToolFilterConfig := {}
  /-- Notifications configuration -/
  notifications : Notifications.NotificationConfig := {}
  /-- Default output format ("json" or "toon") -/
  outputFormatDefault : String := ""
  /-- Default toon output format ("json" or "toon") -/
  toonDefaultFormat : String := ""
  /-- Whether toon encoder stats are enabled -/
  toonStatsEnabled : Bool := false
  /-- Toon encoder binary (defaults to "tru") -/
  toonBin : String := ""
  /-- Log level (DEBUG, INFO, WARN, ERROR) -/
  logLevel : String := "INFO"
  /-- Whether request logging is enabled -/
  requestLogEnabled : Bool := false
  deriving Repr

namespace Config

/-- Default configuration -/
def default : Config := {}

/-- Parse boolean from environment variable -/
private def parseBool (s : Option String) (default : Bool := false) : Bool :=
  match s with
  | some v => v.toLower == "1" || v.toLower == "true" || v.toLower == "yes"
  | none => default

/-- Parse Nat from environment variable -/
private def parseNat (s : Option String) (default : Nat) : Nat :=
  match s with
  | some v => v.toNat?.getD default
  | none => default

/-- Parse comma-separated list from environment variable -/
private def parseList (s : Option String) : List String :=
  match s with
  | some v => v.splitOn "," |>.map String.trim |>.filter (!·.isEmpty)
  | none => []

/-- Load configuration from environment variables -/
def fromEnv : IO Config := do
  -- Core settings
  let env ← IO.getEnv "AGENT_MAIL_ENV"
  let port ← IO.getEnv "AGENT_MAIL_PORT"
  let host ← IO.getEnv "AGENT_MAIL_HOST"
  let dbPath ← IO.getEnv "AGENT_MAIL_DB"
  let token ← IO.getEnv "AGENT_MAIL_TOKEN"
  let storageRoot ← IO.getEnv "STORAGE_ROOT"
  let gitAuthorName ← IO.getEnv "GIT_AUTHOR_NAME"
  let gitAuthorEmail ← IO.getEnv "GIT_AUTHOR_EMAIL"
  let worktreesEnabled ← IO.getEnv "WORKTREES_ENABLED"
  let gitIdentityEnabled ← IO.getEnv "GIT_IDENTITY_ENABLED"

  -- HTTP settings
  let httpBearerToken ← IO.getEnv "HTTP_BEARER_TOKEN"
  let httpAllowLocalhost ← IO.getEnv "HTTP_ALLOW_LOCALHOST_UNAUTHENTICATED"
  let httpRateLimitEnabled ← IO.getEnv "HTTP_RATE_LIMIT_ENABLED"
  let httpRateLimitToolsPerMin ← IO.getEnv "HTTP_RATE_LIMIT_TOOLS_PER_MINUTE"
  let httpRateLimitResourcesPerMin ← IO.getEnv "HTTP_RATE_LIMIT_RESOURCES_PER_MINUTE"
  let httpRateLimitToolsBurst ← IO.getEnv "HTTP_RATE_LIMIT_TOOLS_BURST"
  let httpRateLimitResourcesBurst ← IO.getEnv "HTTP_RATE_LIMIT_RESOURCES_BURST"
  let httpJwtEnabled ← IO.getEnv "HTTP_JWT_ENABLED"
  let httpJwtAlgorithms ← IO.getEnv "HTTP_JWT_ALGORITHMS"
  let httpJwtSecret ← IO.getEnv "HTTP_JWT_SECRET"
  let httpJwtJwksUrl ← IO.getEnv "HTTP_JWT_JWKS_URL"
  let httpJwtAudience ← IO.getEnv "HTTP_JWT_AUDIENCE"
  let httpJwtIssuer ← IO.getEnv "HTTP_JWT_ISSUER"
  let httpJwtRoleClaim ← IO.getEnv "HTTP_JWT_ROLE_CLAIM"
  let httpRbacEnabled ← IO.getEnv "HTTP_RBAC_ENABLED"
  let httpRbacReaderRoles ← IO.getEnv "HTTP_RBAC_READER_ROLES"
  let httpRbacWriterRoles ← IO.getEnv "HTTP_RBAC_WRITER_ROLES"
  let httpRbacDefaultRole ← IO.getEnv "HTTP_RBAC_DEFAULT_ROLE"
  let httpRbacReadonlyTools ← IO.getEnv "HTTP_RBAC_READONLY_TOOLS"

  -- CORS settings
  let corsEnabled ← IO.getEnv "HTTP_CORS_ENABLED"
  let corsOrigins ← IO.getEnv "HTTP_CORS_ORIGINS"
  let corsCredentials ← IO.getEnv "HTTP_CORS_ALLOW_CREDENTIALS"
  let corsMethods ← IO.getEnv "HTTP_CORS_ALLOW_METHODS"
  let corsHeaders ← IO.getEnv "HTTP_CORS_ALLOW_HEADERS"

  -- Tool filter settings
  let toolsFilterEnabled ← IO.getEnv "TOOLS_FILTER_ENABLED"
  let toolsFilterProfile ← IO.getEnv "TOOLS_FILTER_PROFILE"
  let toolsFilterMode ← IO.getEnv "TOOLS_FILTER_MODE"
  let toolsFilterClusters ← IO.getEnv "TOOLS_FILTER_CLUSTERS"
  let toolsFilterTools ← IO.getEnv "TOOLS_FILTER_TOOLS"

  -- Notification settings
  let notificationsEnabled ← IO.getEnv "NOTIFICATIONS_ENABLED"
  let notificationsSignalsDir ← IO.getEnv "NOTIFICATIONS_SIGNALS_DIR"
  let notificationsIncludeMetadata ← IO.getEnv "NOTIFICATIONS_INCLUDE_METADATA"
  let notificationsDebounceMs ← IO.getEnv "NOTIFICATIONS_DEBOUNCE_MS"

  -- Logging settings
  let logLevel ← IO.getEnv "LOG_LEVEL"
  let requestLogEnabled ← IO.getEnv "HTTP_REQUEST_LOG_ENABLED"

  -- Output formatting settings
  let outputFormatDefault ← IO.getEnv "MCP_AGENT_MAIL_OUTPUT_FORMAT"
  let toonDefaultFormat ← IO.getEnv "TOON_DEFAULT_FORMAT"
  let toonStatsEnabled ← IO.getEnv "TOON_STATS"
  let toonTruBin ← IO.getEnv "TOON_TRU_BIN"
  let toonBin ← IO.getEnv "TOON_BIN"

  -- Parse port
  let portVal : UInt16 := match port with
    | some p => match p.toNat? with
      | some n => if n > 0 && n < 65536 then n.toUInt16 else 8765
      | none => 8765
    | none => 8765

  -- Build rate limit config
  let rateLimitConfig : Middleware.RateLimit.RateLimitConfig := {
    enabled := parseBool httpRateLimitEnabled
    toolsPerMinute := parseNat httpRateLimitToolsPerMin 60
    resourcesPerMinute := parseNat httpRateLimitResourcesPerMin 120
    toolsBurst := parseNat httpRateLimitToolsBurst 10
    resourcesBurst := parseNat httpRateLimitResourcesBurst 20
  }

  -- Build HTTP config
  let httpConfig : HttpConfig := {
    bearerToken := httpBearerToken
    allowLocalhostUnauthenticated := parseBool httpAllowLocalhost true
    rateLimit := rateLimitConfig
    jwtEnabled := parseBool httpJwtEnabled
    jwtAlgorithms := if httpJwtAlgorithms.isSome then parseList httpJwtAlgorithms else ["HS256"]
    jwtSecret := httpJwtSecret
    jwtJwksUrl := httpJwtJwksUrl
    jwtAudience := httpJwtAudience
    jwtIssuer := httpJwtIssuer
    jwtRoleClaim := httpJwtRoleClaim.getD "roles"
    rbacEnabled := parseBool httpRbacEnabled true
    rbacReaderRoles := parseList httpRbacReaderRoles
    rbacWriterRoles := parseList httpRbacWriterRoles
    rbacDefaultRole := httpRbacDefaultRole.getD "tools"
    rbacReadonlyTools := parseList httpRbacReadonlyTools
  }

  -- Build CORS config
  let corsMethodsDefault := ["GET", "POST", "PUT", "DELETE", "PATCH", "OPTIONS"]
  let corsHeadersDefault := ["Content-Type", "Authorization", "Accept"]
  let corsConfig : Middleware.CORS.CorsConfig := {
    enabled := parseBool corsEnabled
    origins := parseList corsOrigins
    allowCredentials := parseBool corsCredentials
    allowMethods := if corsMethods.isSome then parseList corsMethods else corsMethodsDefault
    allowHeaders := if corsHeaders.isSome then parseList corsHeaders else corsHeadersDefault
  }

  -- Build tool filter config
  let toolFilterConfig : ToolFilter.ToolFilterConfig := {
    enabled := parseBool toolsFilterEnabled
    profile := ToolFilter.ToolProfile.fromString (toolsFilterProfile.getD "full")
    mode := toolsFilterMode.getD "include"
    clusters := parseList toolsFilterClusters
    tools := parseList toolsFilterTools
  }

  -- Build notifications config
  let notificationsConfig : Notifications.NotificationConfig := {
    enabled := parseBool notificationsEnabled
    signalsDir := notificationsSignalsDir.getD "~/.mcp_agent_mail/signals"
    includeMetadata := parseBool notificationsIncludeMetadata true
    debounceMs := parseNat notificationsDebounceMs 100
  }

  pure {
    environment := env.getD "development"
    port := portVal
    host := host.getD "127.0.0.1"
    databasePath := dbPath.getD "agent_mail.db"
    storageRoot := storageRoot.getD "~/.mcp_agent_mail_git_mailbox_repo"
    gitAuthorName := gitAuthorName.getD "mcp-agent"
    gitAuthorEmail := gitAuthorEmail.getD "mcp-agent@example.com"
    worktreesEnabled :=
      (parseBool worktreesEnabled) || (parseBool gitIdentityEnabled)
    authToken := token
    http := httpConfig
    cors := corsConfig
    toolFilter := toolFilterConfig
    notifications := notificationsConfig
    outputFormatDefault := outputFormatDefault.getD ""
    toonDefaultFormat := toonDefaultFormat.getD ""
    toonStatsEnabled := parseBool toonStatsEnabled
    toonBin := (toonTruBin.getD "" |> fun v => if v.isEmpty then toonBin.getD "" else v)
    logLevel := logLevel.getD "INFO"
    requestLogEnabled := parseBool requestLogEnabled
  }

/-- Display configuration (hiding auth tokens) -/
def display (cfg : Config) : String :=
  let tokenDisplay := match cfg.authToken with
    | some _ => "(set)"
    | none => "(none)"
  let httpTokenDisplay := match cfg.http.bearerToken with
    | some _ => "(set)"
    | none => "(none)"
  s!"Config \{ env: {cfg.environment}, host: {cfg.host}, port: {cfg.port}, " ++
  s!"database: {cfg.databasePath}, storage: {cfg.storageRoot}, token: {tokenDisplay}, " ++
  s!"http.bearerToken: {httpTokenDisplay}, http.rateLimit.enabled: {cfg.http.rateLimit.enabled}, " ++
  s!"cors.enabled: {cfg.cors.enabled}, toolFilter.enabled: {cfg.toolFilter.enabled}, " ++
  s!"notifications.enabled: {cfg.notifications.enabled}, outputFormat: {cfg.outputFormatDefault}, " ++
  s!"logLevel: {cfg.logLevel} }"

end Config

end AgentMail
