/-
  AgentMail.Tools.GitGuard - Git guard installation MCP tool handlers
-/
import Citadel
import AgentMail.Config
import AgentMail.Protocol.JsonRpc
import AgentMail.Storage.Archive
import AgentMail.Storage.Database
import AgentMail.Tools.Identity
import AgentMail.Git.Guard

open Citadel

namespace AgentMail.Tools.GitGuard

private def expandUser (path : String) : IO String := do
  if path.startsWith "~/" then
    match ← IO.getEnv "HOME" with
    | some home => pure (home ++ path.drop 1)
    | none => pure path
  else
    pure path

private def resolvePath (path : String) : IO String := do
  let expanded ← expandUser path
  try
    let resolved ← IO.FS.realPath expanded
    pure resolved.toString
  catch _ =>
    pure expanded

private def runGit (repoPath : String) (args : Array String) : IO (Option String) := do
  let result ← IO.Process.output {
    cmd := "git"
    args := #["-C", repoPath] ++ args
  }
  if result.exitCode == 0 then
    pure (some result.stdout.trim)
  else
    pure none

/-- Get the hooks directory for a git repository. -/
private def getHooksDir (repoPath : String) : IO String := do
  let hooksPath := (← runGit repoPath #["config", "--get", "core.hooksPath"]).getD ""
  if hooksPath != "" then
    let expanded ← expandUser hooksPath
    let hookPath := System.FilePath.mk expanded
    if hookPath.isAbsolute then
      pure expanded
    else
      let root := (← runGit repoPath #["rev-parse", "--show-toplevel"]).getD repoPath
      pure s!"{root}/{hooksPath}"
  else
    match ← runGit repoPath #["rev-parse", "--git-dir"] with
    | some gitDir =>
        let gitPath := System.FilePath.mk gitDir
        if gitPath.isAbsolute then
          pure s!"{gitDir}/hooks"
        else
          pure s!"{repoPath}/{gitDir}/hooks"
    | none =>
        pure s!"{repoPath}/.git/hooks"

/-- Read file contents, returning empty string on error. -/
private def readFileOrEmpty (path : String) : IO String := do
  try
    IO.FS.readFile path
  catch _ =>
    pure ""

/-- Check if a file exists. -/
private def fileExists (path : String) : IO Bool :=
  (System.FilePath.mk path).pathExists

/-- Write file with content and optionally make executable. -/
private def writeFileExecutable (path : String) (content : String) : IO Unit := do
  IO.FS.writeFile path content
  try
    let _ ← IO.Process.run {
      cmd := "chmod"
      args := #["+x", path]
    }
    pure ()
  catch _ =>
    pure ()

/-- Write a file only if it does not already exist. -/
private def writeFileIfMissing (path : String) (content : String) : IO Unit := do
  if !(← fileExists path) then
    IO.FS.writeFile path content

/-- Create directories recursively. -/
private def mkdirp (path : String) : IO Unit :=
  IO.FS.createDirAll path

/-- Rename a file. -/
private def renameFile (src dst : String) : IO Unit :=
  IO.FS.rename src dst

/-- Remove a file. -/
private def removeFile (path : String) : IO Unit :=
  IO.FS.removeFile path

/-- Check whether hooks.d/<hook> has other plugins besides ours. -/
private def hasOtherPlugins (runDir : String) : IO Bool := do
  let dirPath := System.FilePath.mk runDir
  if !(← dirPath.isDir) then
    pure false
  else
    let entries ← dirPath.readDir
    let mut found := false
    for entry in entries do
      if entry.fileName != "50-agent-mail.py" then
        found := true
    pure found

/-- Handle install_precommit_guard request. -/
def handleInstallPrecommitGuard (db : Storage.Database) (cfg : Config) (req : JsonRpc.Request) : IO Response := do
  let params := req.params.getD Lean.Json.null

  -- Extract required params
  let projectKey ← match params.getObjValAs? String "project_key" with
    | Except.ok k => pure k
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: project_key")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  let codeRepoPath ← match params.getObjValAs? String "code_repo_path" with
    | Except.ok p => pure p
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: code_repo_path")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  if !cfg.worktreesEnabled then
    let result : Git.Guard.InstallResult := { hook := "" }
    let resp := JsonRpc.Response.success req.id (Lean.toJson result)
    return Response.json (Lean.Json.compress (Lean.toJson resp))

  -- Resolve project (validates it exists)
  let project ← match ← Identity.resolveProject db projectKey with
    | some p => pure p
    | none =>
      let err := JsonRpc.Error.invalidParams (some s!"project not found: {projectKey}")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  let repoPath ← resolvePath codeRepoPath

  -- Get hooks directory
  let hooksDir ← getHooksDir repoPath
  mkdirp hooksDir

  -- Ensure hooks.d/pre-commit directory
  let hooksDDir := s!"{hooksDir}/hooks.d/pre-commit"
  mkdirp hooksDDir

  let hookPath := s!"{hooksDir}/pre-commit"
  let pluginPath := s!"{hooksDDir}/50-agent-mail.py"

  -- Check if existing pre-commit hook exists and is not our chain-runner
  let existingHookExists ← fileExists hookPath
  if existingHookExists then
    let existingContent ← readFileOrEmpty hookPath
    if !Git.Guard.isChainRunnerContent existingContent then
      let backup := s!"{hooksDir}/pre-commit.orig"
      let backupExists ← fileExists backup
      if !backupExists then
        renameFile hookPath backup

  -- Write chain-runner
  let chainRunnerContent := Git.Guard.renderChainRunner "pre-commit"
  writeFileExecutable hookPath chainRunnerContent

  -- Windows shims (.cmd / .ps1) to invoke the Python chain-runner
  let cmdPath := s!"{hooksDir}/pre-commit.cmd"
  let cmdBody :=
    "@echo off\r\n" ++
    "setlocal\r\n" ++
    "set \"DIR=%~dp0\"\r\n" ++
    "python \"%DIR%pre-commit\" %*\r\n" ++
    "exit /b %ERRORLEVEL%\r\n"
  writeFileIfMissing cmdPath cmdBody
  let ps1Path := s!"{hooksDir}/pre-commit.ps1"
  let ps1Body :=
    "$ErrorActionPreference = 'Stop'\n" ++
    "$hook = Join-Path $PSScriptRoot 'pre-commit'\n" ++
    "python $hook @args\n" ++
    "exit $LASTEXITCODE\n"
  writeFileIfMissing ps1Path ps1Body

  -- Write guard plugin
  let archive ← Storage.ensureProjectArchive cfg project.slug
  let storageRoot ← resolvePath archive.projectRoot
  let fileResDir ← resolvePath s!"{archive.projectRoot}/file_reservations"
  let guardContent := Git.Guard.renderPrecommitGuard storageRoot fileResDir
  writeFileExecutable pluginPath guardContent

  let installResult : Git.Guard.InstallResult := { hook := hookPath }
  let resp := JsonRpc.Response.success req.id (Lean.toJson installResult)
  pure (Response.json (Lean.Json.compress (Lean.toJson resp)))

/-- Handle uninstall_precommit_guard request. -/
def handleUninstallPrecommitGuard (_db : Storage.Database) (_cfg : Config) (req : JsonRpc.Request) : IO Response := do
  let params := req.params.getD Lean.Json.null

  -- Extract required params
  let codeRepoPath ← match params.getObjValAs? String "code_repo_path" with
    | Except.ok p => pure p
    | Except.error _ =>
      let err := JsonRpc.Error.invalidParams (some "missing required param: code_repo_path")
      let resp := JsonRpc.Response.failure req.id err
      return Response.json (Lean.Json.compress (Lean.toJson resp))

  let repoPath ← resolvePath codeRepoPath

  -- Get hooks directory
  let hooksDir ← getHooksDir repoPath

  let mut removed := false

  -- Remove guard plugins if they exist
  for sub in #["pre-commit", "pre-push"] do
    let pluginPath := s!"{hooksDir}/hooks.d/{sub}/50-agent-mail.py"
    let pluginExists ← fileExists pluginPath
    if pluginExists then
      removeFile pluginPath
      removed := true

  -- Legacy top-level single-file uninstall (pre-chain-runner installs)
  let preCommitPath := s!"{hooksDir}/pre-commit"
  let prePushPath := s!"{hooksDir}/pre-push"
  let sentinels := #["mcp-agent-mail guard hook", "AGENT_NAME environment variable is required."]

  for (hookName, hookPath) in #[("pre-commit", preCommitPath), ("pre-push", prePushPath)] do
    let hookExists ← fileExists hookPath
    if hookExists then
      let hookContent ← readFileOrEmpty hookPath
      let isChainRunner := (hookContent.find? "mcp-agent-mail chain-runner").isSome
      let isLegacy := sentinels.any (fun s => (hookContent.find? s).isSome)
      if isChainRunner then
        let runDir := s!"{hooksDir}/hooks.d/{hookName}"
        let origPath := s!"{hooksDir}/{hookName}.orig"
        let otherPlugins ← hasOtherPlugins runDir
        if otherPlugins then
          pure ()
        else
          let origExists ← fileExists origPath
          if origExists then
            removeFile hookPath
            renameFile origPath hookPath
            removed := true
          else
            removeFile hookPath
            removed := true
      else if isLegacy then
        removeFile hookPath
        removed := true

  let uninstallResult : Git.Guard.UninstallResult := { removed := removed }
  let resp := JsonRpc.Response.success req.id (Lean.toJson uninstallResult)
  pure (Response.json (Lean.Json.compress (Lean.toJson resp)))

end AgentMail.Tools.GitGuard
