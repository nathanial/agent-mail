/-
  AgentMail - MCP server for inter-agent communication
-/
import AgentMail.Config
import AgentMail.Middleware
import AgentMail.ToolFilter
import AgentMail.Notifications
import AgentMail.Models.Types
import AgentMail.Models.Project
import AgentMail.Models.Agent
import AgentMail.Models.Message
import AgentMail.Models.FileReservation
import AgentMail.Models.BuildSlot
import AgentMail.Models.ContactRequest
import AgentMail.Models.Contact
import AgentMail.Models.Product
import AgentMail.Protocol.JsonRpc
import AgentMail.Storage.Database
import AgentMail.Utils.NameGenerator
import AgentMail.Tools.Identity
import AgentMail.Tools.Messaging
import AgentMail.Tools.Contacts
import AgentMail.Tools.FileReservations
import AgentMail.Git.Guard
import AgentMail.Tools.GitGuard
import AgentMail.Tools.Search
import AgentMail.Tools.Macros
import AgentMail.Tools.BuildSlots
import AgentMail.Tools.Products
import AgentMail.Resources
import AgentMail.Server.Server
import AgentMail.CLI.Commands
import AgentMail.CLI.Output
import AgentMail.CLI.Handlers
