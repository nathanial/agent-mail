/-
  AgentMail.Web.Pages.Home - Agent Mail live web UI
-/
import Loom
import Loom.Stencil
import Stencil

namespace AgentMail.Web.Pages

open Loom
open Loom.Page
open Loom.ActionM

page home "/" GET do
  let data : Stencil.Value := .object #[("title", .string "Agent Mail")]
  Loom.Stencil.ActionM.renderWithLayout "app" "app/index" data

end AgentMail.Web.Pages
