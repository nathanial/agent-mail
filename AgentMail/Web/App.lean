/-
  AgentMail.Web.App - Loom app for the agent-mail web UI
-/
import Loom
import Loom.Stencil
import Loom.Stencil.Manager
import AgentMail.Web.Pages

namespace AgentMail.Web

open Loom
open AgentMail.Web.Pages

/-- Route prefix for the web UI. -/
def routePrefix : String := "/app"

/-- Application configuration -/
def config : AppConfig := {
  secretKey := "agent-mail-web-secret-key-min-32-chars!!".toUTF8
  sessionCookieName := "agent_mail_session"
  csrfFieldName := "_csrf"
  csrfEnabled := false
}

/-- Build the Loom application. -/
def buildApp : App :=
  Loom.app config
    |> registerPages
    |>.withStencil { templateDir := "templates", extension := ".html.hbs", hotReload := true }

private def stripPrefix (path : String) : String :=
  let parts := path.splitOn "?"
  let base := parts.getD 0 path
  let query := if parts.length > 1 then "?" ++ String.intercalate "?" (parts.drop 1) else ""
  let stripped :=
    if base == routePrefix || base == routePrefix ++ "/" then
      "/"
    else if base.startsWith (routePrefix ++ "/") then
      base.drop routePrefix.length
    else
      base
  stripped ++ query

/-- Build a handler that serves the UI under `/app`. -/
def buildHandler (stencilManagerRef : Option (IO.Ref Loom.Stencil.Manager)) : Citadel.Handler :=
  let app := buildApp
  let baseHandler := app.toHandler none stencilManagerRef
  fun req =>
    let newPath := stripPrefix req.request.path
    let req' : Citadel.ServerRequest := { req with request := { req.request with path := newPath } }
    baseHandler req'

end AgentMail.Web
