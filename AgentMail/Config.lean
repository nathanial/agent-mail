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
  s!"notifications.enabled: {cfg.notifications.enabled}, logLevel: {cfg.logLevel} }"

end Config

end AgentMail
