/-
  Tests.Main - Test runner entry point
-/

import Crucible
import Tests.Types
import Tests.Project
import Tests.Agent
import Tests.Message
import Tests.FileReservation
import Tests.JsonRpc
import Tests.Config
import Tests.Database
import Tests.NameGenerator
import Tests.Identity
import Tests.DatabaseQueries
import Tests.Messaging
import Tests.ContactRequest
import Tests.Contact
import Tests.ContactDatabase
import Tests.ContactTools
import Tests.FileReservationDatabase
import Tests.FileReservationTools
import Tests.GitGuard
import Tests.GitGuardTools
import Tests.Search
import Tests.Macros
import Tests.BuildSlots
import Tests.Products
import Tests.Resources.Discovery
import Tests.Resources.Mail
import Tests.Resources.Threads
import Tests.Resources.Views
import Tests.Resources.FileReservations
import Tests.Middleware.Auth
import Tests.Middleware.RateLimit
import Tests.Middleware.CORS
import Tests.CLI.Commands

open Crucible

def main : IO UInt32 := runAllSuites
