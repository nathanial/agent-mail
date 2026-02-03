/-
  AgentMail.CLI.Commands - CLI command definitions using Parlance.
-/
import Parlance

namespace AgentMail.CLI

open Parlance

/-- The main agent-mail command with all subcommands -/
def agentMailCommand : Command := command "agent-mail" do
  Cmd.version "0.1.0"
  Cmd.description "Agent mail server and CLI for inter-agent communication"

  -- Global flags
  Cmd.boolFlag "json" (short := some 'j') (description := "Output in JSON format (default is text)")

  -- serve subcommand (explicit server start)
  Cmd.subcommand "serve" do
    Cmd.description "Start the HTTP server"

  -- list-projects subcommand
  Cmd.subcommand "list-projects" do
    Cmd.description "List all known projects"

  -- list-acks subcommand
  Cmd.subcommand "list-acks" do
    Cmd.description "List pending acknowledgements"
    Cmd.flag "project" (short := some 'p')
      (description := "Project slug or human key") (required := true)
    Cmd.flag "agent" (short := some 'a')
      (description := "Agent name") (required := true)
    Cmd.flag "limit" (short := some 'l') (argType := .nat)
      (description := "Max messages to show") (defaultValue := some "20")

  -- config subcommand group
  Cmd.subcommand "config" do
    Cmd.description "Configuration management"

    Cmd.subcommand "show-port" do
      Cmd.description "Show the configured server port"

    Cmd.subcommand "set-port" do
      Cmd.description "Set the server port"
      Cmd.arg "port" (argType := .nat) (description := "Port number (1-65535)")

  -- doctor subcommand group
  Cmd.subcommand "doctor" do
    Cmd.description "Database diagnostics and repair"

    Cmd.subcommand "check" do
      Cmd.description "Run database integrity checks"

    Cmd.subcommand "repair" do
      Cmd.description "Repair database (VACUUM, reindex FTS)"

  -- clear-and-reset subcommand
  Cmd.subcommand "clear-and-reset" do
    Cmd.description "Drop all tables and reinitialize database schema (DESTRUCTIVE)"
    Cmd.boolFlag "force" (short := some 'f')
      (description := "Required to confirm destructive operation")

end AgentMail.CLI
