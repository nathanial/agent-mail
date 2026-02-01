/-
  AgentMail.Middleware - HTTP middleware for agent-mail server
-/
import AgentMail.Middleware.Auth
import AgentMail.Middleware.Security
import AgentMail.Middleware.RateLimit
import AgentMail.Middleware.CORS
import AgentMail.Middleware.RequestLog

namespace AgentMail.Middleware

-- Re-export for convenience
export Auth (bearerAuth optionalBearerAuth)
export Security (jwtRbac)
export RateLimit (RateLimitConfig RateLimitState rateLimit)
export CORS (CorsConfig cors)
export RequestLog (requestLog)

end AgentMail.Middleware
