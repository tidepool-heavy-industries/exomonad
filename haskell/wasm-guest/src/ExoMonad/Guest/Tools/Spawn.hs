-- | Hylo spawn primitives: spawn_subtree, spawn_workers.
module ExoMonad.Guest.Tools.Spawn
  ( SpawnSubtree,
    SpawnLeafSubtree,
    SpawnWorkers,
    SpawnSubtreeArgs (..),
    SpawnLeafSubtreeArgs (..),
    SpawnWorkersArgs (..),
    WorkerSpec (..),
    WorkerType (..),
    SpawnAcp,
    SpawnAcpArgs (..),
  )
where

import Control.Monad (forM, void)
import Control.Monad.Freer (Eff)
import Data.Aeson (FromJSON, object, withObject, withText, (.:), (.:?), (.=))
import Data.Maybe (fromMaybe)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Either (partitionEithers)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Effects.EffectError (Custom (..), EffectError (..), EffectErrorKind (..), InvalidInput (..), NetworkError (..), NotFound (..), PermissionDenied (..), Timeout (..))
import Effects.Log qualified as Log
import ExoMonad.Effects.Log (LogEmitEvent)
import ExoMonad.Guest.Effects.AgentControl qualified as AC
import ExoMonad.Guest.Tool.Class
import ExoMonad.Guest.Tool.Schema (JsonSchema (..), genericToolSchemaWith)
import ExoMonad.Guest.Tool.SuspendEffect (suspendEffect_)
import ExoMonad.Guest.Types.Permissions
import GHC.Generics (Generic)

-- ============================================================================
-- Helpers
-- ============================================================================

-- | Helper to convert EffectError to a human-readable message.
spawnErrorMessage :: EffectError -> Text
spawnErrorMessage (EffectError kind) = case kind of
  Just (EffectErrorKindCustom c) -> case customCode c of
    "worktree.branch_exists" -> "Branch already exists. Try a different branch_name suffix."
    "worktree.push_rejected" -> "Push rejected (non-fast-forward). Remote branch has diverged."
    "worktree.lock_conflict" -> "Git lock file conflict - another git operation may be in progress. Retry in a few seconds."
    _ -> TL.toStrict (customMessage c)
  Just (EffectErrorKindNotFound n) -> "Not found: " <> TL.toStrict (notFoundResource n)
  Just (EffectErrorKindInvalidInput i) -> "Invalid input: " <> TL.toStrict (invalidInputMessage i)
  Just (EffectErrorKindNetworkError n) -> "Network error: " <> TL.toStrict (networkErrorMessage n)
  Just (EffectErrorKindPermissionDenied p) -> "Permission denied: " <> TL.toStrict (permissionDeniedMessage p)
  Just (EffectErrorKindTimeout t) -> "Timeout: " <> TL.toStrict (timeoutMessage t)
  Nothing -> "Unknown effect error"

-- | Helper to check if an EffectError has a specific custom code.
hasCustomCode :: Text -> EffectError -> Bool
hasCustomCode code (EffectError (Just (EffectErrorKindCustom c))) = customCode c == TL.fromStrict code
hasCustomCode _ _ = False

-- ============================================================================
-- SpawnSubtree
-- ============================================================================

data SpawnSubtree

data SpawnSubtreeArgs = SpawnSubtreeArgs
  { ssTask :: Text,
    ssBranchName :: Text,
    ssForkSession :: Maybe Bool,
    ssPermissionMode :: Maybe Text,
    ssAllowedTools :: Maybe [Text],
    ssDisallowedTools :: Maybe [Text],
    ssWorkingDir :: Maybe Text,
    ssPermissions :: Maybe ClaudePermissions,
    ssStandaloneRepo :: Maybe Bool,
    ssAllowedDirs :: Maybe [Text]
  }
  deriving (Show, Eq, Generic)

instance FromJSON SpawnSubtreeArgs where
  parseJSON = withObject "SpawnSubtreeArgs" $ \v ->
    SpawnSubtreeArgs
      <$> v .: "task"
      <*> v .: "branch_name"
      <*> v .:? "fork_session"
      <*> v .:? "permission_mode"
      <*> v .:? "allowed_tools"
      <*> v .:? "disallowed_tools"
      <*> v .:? "working_dir"
      <*> v .:? "permissions"
      <*> v .:? "standalone_repo"
      <*> v .:? "allowed_dirs"

instance MCPTool SpawnSubtree where
  type ToolArgs SpawnSubtree = SpawnSubtreeArgs
  toolName = "spawn_subtree"
  toolDescription = "Fork a Claude agent into its own worktree and tmux window. The child gets TL role (can spawn its own children). IMPORTANT: You MUST create a team using TeamCreate BEFORE calling any spawn tool — without a team, child agent messages will not be delivered to you. After spawning, return immediately — you will be notified when the agent sends updates or when Copilot approves their PR. Do not poll or wait. Prefer spawn_leaf_subtree or spawn_workers for implementation work — Gemini agents are highly capable implementers and cost 10-30x less."
  toolSchema =
    genericToolSchemaWith @SpawnSubtreeArgs
      [ ("task", "Description of the sub-problem to solve"),
        ("branch_name", "Branch name suffix (will be prefixed with current branch)"),
        ("fork_session", "Whether to fork the parent's conversation context into the child (default: false). Set true to inherit context, but may fail if the session is stale or compacted."),
        ("permission_mode", "Permission mode for Claude (e.g., 'plan', 'default'). Omit for --dangerously-skip-permissions."),
        ("allowed_tools", "Tool patterns to allow (e.g., ['Read', 'Grep']). Omit for no restriction."),
        ("disallowed_tools", "Tool patterns to disallow (e.g., ['Bash']). Omit for no restriction."),
        ("working_dir", "Working directory for the agent (relative to worktree root)."),
        ("permissions", "Explicit permission rules (object with 'allow' and 'deny' arrays of strings)."),
        ("standalone_repo", "When true, creates a standalone git repo instead of a worktree for information isolation."),
        ("allowed_dirs", "Directories from the parent project to be copied into the agent's context (only for standalone_repo).")
      ]
  toolHandlerEff args = do
    let forkSession = fromMaybe False (ssForkSession args)
        standaloneRepo = fromMaybe False (ssStandaloneRepo args)
        perms = AC.PermissionFlags
          { AC.permMode = ssPermissionMode args,
            AC.allowedTools = fromMaybe [] (ssAllowedTools args),
            AC.disallowedTools = fromMaybe [] (ssDisallowedTools args)
          }
        cfg = AC.SpawnSubtreeConfig
          { AC.stcTask = ssTask args
          , AC.stcBranchName = ssBranchName args
          , AC.stcForkSession = forkSession
          , AC.stcRole = Nothing
          , AC.stcAgentType = AC.Claude
          , AC.stcPerms = perms
          , AC.stcWorkingDir = ssWorkingDir args
          , AC.stcPermissions = ssPermissions args
          , AC.stcStandaloneRepo = standaloneRepo
          , AC.stcAllowedDirs = fromMaybe [] (ssAllowedDirs args)
          }
    result <- AC.spawnSubtree cfg
    case result of
      Left err | hasCustomCode "worktree.branch_exists" err -> do
        let cfg' = cfg { AC.stcBranchName = AC.stcBranchName cfg <> "-2" }
        result' <- AC.spawnSubtree cfg'
        case result' of
          Left err' -> pure $ errorResult (spawnErrorMessage err')
          Right spawnResult -> do
            emitSpawnEvent (AC.stcBranchName cfg') "claude" (ssTask args)
            pure $ successResult $ Aeson.toJSON spawnResult
      Left err -> pure $ errorResult (spawnErrorMessage err)
      Right spawnResult -> do
        emitSpawnEvent (ssBranchName args) "claude" (ssTask args)
        pure $ successResult $ Aeson.toJSON spawnResult

-- ============================================================================
-- SpawnLeafSubtree
-- ============================================================================

data SpawnLeafSubtree

data SpawnLeafSubtreeArgs = SpawnLeafSubtreeArgs
  { slsTask :: Text,
    slsBranchName :: Text,
    slsPermissionMode :: Maybe Text,
    slsAllowedTools :: Maybe [Text],
    slsDisallowedTools :: Maybe [Text],
    slsStandaloneRepo :: Maybe Bool,
    slsAllowedDirs :: Maybe [Text]
  }
  deriving (Show, Eq, Generic)

instance FromJSON SpawnLeafSubtreeArgs where
  parseJSON = withObject "SpawnLeafSubtreeArgs" $ \v ->
    SpawnLeafSubtreeArgs
      <$> v .: "task"
      <*> v .: "branch_name"
      <*> v .:? "permission_mode"
      <*> v .:? "allowed_tools"
      <*> v .:? "disallowed_tools"
      <*> v .:? "standalone_repo"
      <*> v .:? "allowed_dirs"

instance MCPTool SpawnLeafSubtree where
  type ToolArgs SpawnLeafSubtree = SpawnLeafSubtreeArgs
  toolName = "spawn_leaf_subtree"
  toolDescription = "Fork a Gemini agent into its own worktree and tmux window. Gets dev role (files PR, cannot spawn children). Gemini is a capable implementer — give it acceptance criteria and file paths, not line-by-line instructions. IMPORTANT: You MUST create a team using TeamCreate BEFORE calling any spawn tool — without a team, child agent messages will not be delivered to you. After spawning, return immediately — you will be notified when the agent sends updates or when Copilot approves their PR."
  toolSchema =
    genericToolSchemaWith @SpawnLeafSubtreeArgs
      [ ("task", "Description of the sub-problem to solve"),
        ("branch_name", "Branch name suffix (will be prefixed with current branch)"),
        ("permission_mode", "Permission mode for the agent. Omit for --dangerously-skip-permissions."),
        ("allowed_tools", "Tool patterns to allow. Omit for no restriction."),
        ("disallowed_tools", "Tool patterns to disallow. Omit for no restriction."),
        ("standalone_repo", "When true, creates a standalone git repo instead of a worktree for information isolation."),
        ("allowed_dirs", "Directories from the parent project to be copied into the agent's context (only for standalone_repo).")
      ]
  toolHandlerEff args = do
    -- WASM32 BUG WORKAROUND: Prompt's derived Semigroup (<>) hangs when
    -- evaluated inside the freer-simple coroutine context on GHC WASM32.
    -- Build prompts as raw Text instead of using the Prompt builder.
    let renderedTask = slsTask args <> "\n\n" <> leafProfileText
        standaloneRepo = fromMaybe False (slsStandaloneRepo args)
        perms = AC.PermissionFlags
          { AC.permMode = slsPermissionMode args,
            AC.allowedTools = fromMaybe [] (slsAllowedTools args),
            AC.disallowedTools = fromMaybe [] (slsDisallowedTools args)
          }
        cfg = AC.SpawnLeafSubtreeConfig
          { AC.slcTask = renderedTask
          , AC.slcBranchName = slsBranchName args
          , AC.slcRole = Nothing
          , AC.slcAgentType = AC.Gemini
          , AC.slcPerms = perms
          , AC.slcStandaloneRepo = standaloneRepo
          , AC.slcAllowedDirs = fromMaybe [] (slsAllowedDirs args)
          }
    result <- AC.spawnLeafSubtree cfg
    case result of
      Left err | hasCustomCode "worktree.branch_exists" err -> do
        let cfg' = cfg { AC.slcBranchName = AC.slcBranchName cfg <> "-2" }
        result' <- AC.spawnLeafSubtree cfg'
        case result' of
          Left err' -> pure $ errorResult (spawnErrorMessage err')
          Right spawnResult -> do
            emitSpawnEvent (AC.slcBranchName cfg') "gemini" (slsTask args)
            pure $ successResult $ Aeson.toJSON spawnResult
      Left err -> pure $ errorResult (spawnErrorMessage err)
      Right spawnResult -> do
        emitSpawnEvent (slsBranchName args) "gemini" (slsTask args)
        pure $ successResult $ Aeson.toJSON spawnResult

-- ============================================================================
-- SpawnWorkers (batch)
-- ============================================================================

data SpawnWorkers

-- | Worker type determines the completion protocol and allowed operations.
data WorkerType = Implementation | Research
  deriving (Show, Eq, Generic)

instance JsonSchema WorkerType

instance FromJSON WorkerType where
  parseJSON = withText "WorkerType" $ \case
    "implementation" -> pure Implementation
    "research" -> pure Research
    t -> fail $ "Unknown worker type: " <> T.unpack t

data WorkerSpec = WorkerSpec
  { wsName :: Text,
    wsTask :: Text,
    wsReadFirst :: Maybe [Text],
    wsSteps :: Maybe [Text],
    wsVerify :: Maybe [Text],
    wsDoneCriteria :: Maybe [Text],
    wsBoundary :: Maybe [Text],
    wsContext :: Maybe Text,
    wsPrompt :: Maybe Text,
    wsProfiles :: Maybe [Text],
    wsContextFiles :: Maybe [Text],
    wsVerifyTemplates :: Maybe [Text],
    wsType :: Maybe WorkerType,
    wsPermissionMode :: Maybe Text,
    wsAllowedTools :: Maybe [Text],
    wsDisallowedTools :: Maybe [Text]
  }
  deriving (Show, Eq, Generic)

instance JsonSchema WorkerSpec where
  toSchema =
    Aeson.Object $
      genericToolSchemaWith @WorkerSpec
        [ ("name", "Human-readable name for the leaf agent"),
        ("task", "Short description of the task"),
        ("read_first", "Files the agent should read before starting"),
        ("steps", "Numbered implementation steps"),
        ("verify", "Commands to verify the work"),
        ("done_criteria", "Acceptance criteria for completion"),
        ("boundary", "Things the agent must NOT do"),
        ("context", "Freeform context: code snippets, examples, detailed specs"),
        ("prompt", "Raw prompt (escape hatch). If provided, all other fields except name are ignored."),
        ("profiles", "Template profiles to include (e.g., 'general', 'haskell', 'rust')"),
        ("context_files", "Paths to files to include in context"),
        ("verify_templates", "Verification script templates"),
        ("type", "Worker type: 'implementation' (default) or 'research'. Research workers are read-only — they explore, search, and report findings via notify_parent."),
        ("permission_mode", "Permission mode for the agent. Omit for --dangerously-skip-permissions."),
        ("allowed_tools", "Tool patterns to allow. Omit for no restriction."),
        ("disallowed_tools", "Tool patterns to disallow. Omit for no restriction.")
      ]

instance FromJSON WorkerSpec where
  parseJSON = withObject "WorkerSpec" $ \v ->
    WorkerSpec
      <$> v .: "name"
      <*> v .: "task"
      <*> v .:? "read_first"
      <*> v .:? "steps"
      <*> v .:? "verify"
      <*> v .:? "done_criteria"
      <*> v .:? "boundary"
      <*> v .:? "context"
      <*> v .:? "prompt"
      <*> v .:? "profiles"
      <*> v .:? "context_files"
      <*> v .:? "verify_templates"
      <*> v .:? "type"
      <*> v .:? "permission_mode"
      <*> v .:? "allowed_tools"
      <*> v .:? "disallowed_tools"

data SpawnWorkersArgs = SpawnWorkersArgs
  { swsSpecs :: [WorkerSpec]
  }
  deriving (Show, Eq, Generic)

instance FromJSON SpawnWorkersArgs where
  parseJSON = withObject "SpawnWorkersArgs" $ \v ->
    SpawnWorkersArgs <$> v .: "specs"

instance MCPTool SpawnWorkers where
  type ToolArgs SpawnWorkers = SpawnWorkersArgs
  toolName = "spawn_workers"
  toolDescription = "Spawn multiple Gemini worker agents in one call. PREFER WORKERS OVER DOING WORK YOURSELF — Gemini costs 10-30x less than your Opus tokens. Any task you can specify clearly (implementation, research, file edits, test writing) should be a worker. If it touches 2+ files or takes more than 5 tool calls, spawn a worker. Give them acceptance criteria, key file paths, and anti-patterns, not step-by-step code. Each gets a tmux pane in YOUR window, working in YOUR directory on YOUR branch (ephemeral, no isolation, no PR). Workers send messages via notify_parent. Set type to 'research' for read-only exploration workers that search, read, and report findings without modifying anything. IMPORTANT: You MUST create a team using TeamCreate BEFORE calling any spawn tool — without a team, child agent messages will not be delivered to you. After spawning, return immediately — do not poll or wait."
  toolSchema =
    genericToolSchemaWith @SpawnWorkersArgs
      [ ("specs", "Array of worker specifications")
      ]
  toolHandlerEff args = do
    results <- forM (swsSpecs args) $ \spec -> do
      -- WASM32 BUG WORKAROUND: see renderWorkerPrompt comment.
      let prompt = case wsPrompt spec of
            Just p -> p
            Nothing -> renderWorkerPrompt spec
          perms = AC.PermissionFlags
            { AC.permMode = wsPermissionMode spec,
              AC.allowedTools = fromMaybe [] (wsAllowedTools spec),
              AC.disallowedTools = fromMaybe [] (wsDisallowedTools spec)
            }
          cfg = AC.SpawnWorkerConfig
            { AC.swcName = wsName spec,
              AC.swcPrompt = prompt,
              AC.swcPerms = perms
            }
      r <- AC.spawnWorker cfg
      case r of
        Right _ -> emitSpawnEvent (wsName spec) "gemini-worker" (wsTask spec)
        Left _ -> pure ()
      pure r
    let (errs, successes) = partitionEithers results
    pure $
      successResult $
        object
          [ "spawned" .= map Aeson.toJSON successes,
            "errors" .= map (Aeson.String . spawnErrorMessage) errs
          ]

-- ============================================================================
-- SpawnAcp (single ACP agent)
-- ============================================================================

data SpawnAcp

data SpawnAcpArgs = SpawnAcpArgs
  { saName :: Text,
    saPrompt :: Text,
    saPermissionMode :: Maybe Text,
    saAllowedTools :: Maybe [Text],
    saDisallowedTools :: Maybe [Text]
  }
  deriving (Show, Eq, Generic)

instance FromJSON SpawnAcpArgs where
  parseJSON = withObject "SpawnAcpArgs" $ \v ->
    SpawnAcpArgs
      <$> v .: "name"
      <*> v .: "prompt"
      <*> v .:? "permission_mode"
      <*> v .:? "allowed_tools"
      <*> v .:? "disallowed_tools"

instance MCPTool SpawnAcp where
  type ToolArgs SpawnAcp = SpawnAcpArgs
  toolName = "spawn_acp"
  toolDescription = "Spawn a Gemini agent via ACP (experimental). Runs headless — no tmux window. Gemini is a capable implementer — write specs, not code. Agent has MCP tools and communicates via structured protocol."
  toolSchema =
    genericToolSchemaWith @SpawnAcpArgs
      [ ("name", "Human-readable name for the agent"),
        ("prompt", "Initial prompt/instructions for the agent"),
        ("permission_mode", "Permission mode for the agent. Omit for --dangerously-skip-permissions."),
        ("allowed_tools", "Tool patterns to allow. Omit for no restriction."),
        ("disallowed_tools", "Tool patterns to disallow. Omit for no restriction.")
      ]
  toolHandlerEff args = do
    let renderedPrompt = saPrompt args <> "\n\n" <> workerProfileText
        perms = AC.PermissionFlags
          { AC.permMode = saPermissionMode args,
            AC.allowedTools = fromMaybe [] (saAllowedTools args),
            AC.disallowedTools = fromMaybe [] (saDisallowedTools args)
          }
        cfg = AC.SpawnAcpConfig
          { AC.sacName = saName args,
            AC.sacPrompt = renderedPrompt,
            AC.sacPerms = perms
          }
    result <- AC.spawnAcp cfg
    case result of
      Left err -> pure $ errorResult (spawnErrorMessage err)
      Right spawnResult -> do
        emitSpawnEvent (saName args) "gemini-acp" (saName args)
        pure $ successResult $ Aeson.toJSON spawnResult

-- | Helper to emit 'agent.spawned' event to the host.
emitSpawnEvent :: Text -> Text -> Text -> Eff Effects ()
emitSpawnEvent slug agentType taskSummary = do
  let eventPayload = BSL.toStrict $ Aeson.encode $ object
        [ "slug" .= slug,
          "agent_type" .= agentType,
          "task_summary" .= taskSummary
        ]
  void $ suspendEffect_ @LogEmitEvent (Log.EmitEventRequest
    { Log.emitEventRequestEventType = "agent.spawned",
      Log.emitEventRequestPayload = eventPayload,
      Log.emitEventRequestTimestamp = 0
    })

-- ============================================================================
-- Raw Text prompt builders (WASM32 workaround)
-- ============================================================================

-- WASM32 BUG: The Prompt newtype's derived Semigroup (<>) hangs when evaluated
-- inside the freer-simple coroutine context on GHC 9.12 WASM32-WASI. Even
-- `P.task t <> mempty` hangs, while `P.render (P.task t)` alone works fine.
-- Root cause unknown (possibly coerce + list ++ codegen, possibly stack overflow
-- in WASM RTS during derived instance evaluation).
--
-- Workaround: build all prompts as raw Text, bypassing the Prompt type entirely.
-- The Prompt module is still used by native Haskell code; only WASM tool handlers
-- are affected.

-- | Render a structured worker prompt as raw Text.
renderWorkerPrompt :: WorkerSpec -> Text
renderWorkerPrompt spec =
  T.intercalate "\n\n" $ filter (not . T.null) $
    [ "## TASK\n" <> wsTask spec ]
    <> maybe [] (\items -> ["## BOUNDARY\n" <> T.intercalate "\n" (map ("- " <>) items)]) (wsBoundary spec)
    <> maybe [] (\items -> ["## READ FIRST\n" <> T.intercalate "\n" (map ("- " <>) items)]) (wsReadFirst spec)
    <> maybe [] (\items -> ["## STEPS\n" <> T.intercalate "\n" (zipWith (\i s -> T.pack (show (i :: Int)) <> ". " <> s) [1..] items)]) (wsSteps spec)
    <> maybe [] (\t -> if T.null t then [] else ["## CONTEXT\n" <> t]) (wsContext spec)
    <> maybe [] (\items -> ["## VERIFY\n" <> T.intercalate "\n" (map (\c -> "- `" <> c <> "`") items)]) (wsVerify spec)
    <> maybe [] (\items -> ["## DONE CRITERIA\n" <> T.intercalate "\n" (map ("- " <>) items)]) (wsDoneCriteria spec)
    <> [profileTextFor (fromMaybe Implementation (wsType spec))]

-- | Select the profile text for a worker type.
profileTextFor :: WorkerType -> Text
profileTextFor Implementation = workerProfileText
profileTextFor Research = researchProfileText

-- | Pre-rendered leaf profile text.
leafProfileText :: Text
leafProfileText = "## Completion Protocol (Leaf Subtree)\nYou are a **leaf agent** in your own git worktree and branch. Your branch name follows the pattern `{parent}.{slug}`.\n\nWhen you are done:\n\n1. **Commit your changes** with a descriptive message.\n   - `git add <specific files>` \x2014 NEVER `git add .` or `git add -A`\n   - `git commit -m \"feat: <description>\"`\n2. **File a PR** using `file_pr` tool. The base branch is auto-detected from your branch name.\n3. **Copilot review is automatic.** After you file a PR, the system monitors for Copilot review and will notify your parent when approved. If Copilot posts comments, they'll appear in your pane \x2014 address them and push fixes.\n4. **Use `notify_parent` to send status updates** \x2014 e.g., \"PR filed, awaiting review\" or \"hit a blocker, need guidance.\" Call with `failure` status to escalate problems.\n\n**DO NOT:**\n- Merge your own PR (the parent TL merges)\n- Push to main or any branch other than your own\n- Create additional branches"

-- | Pre-rendered worker profile text. Same WASM workaround.
workerProfileText :: Text
workerProfileText = "## Completion Protocol (Worker)\nYou are an **ephemeral worker** \x2014 you run in the parent's directory on the parent's branch. You do NOT have your own worktree or branch.\n\nWhen you are done:\n\n1. **Call `notify_parent`** with status `success` and a DETAILED message containing your complete findings.\n   - Include FULL code snippets, exact file paths with line numbers, and concrete data.\n   - Your parent CANNOT see your terminal output. `notify_parent` is your ONLY communication channel.\n   - A terse summary like \"Task complete\" is useless \x2014 include everything the parent needs to act on your findings.\n   - For research tasks: include the actual code/data you found, not just \"I found it.\"\n   - For implementation tasks: describe exactly what you changed and how to verify it.\n2. If you failed after multiple attempts, call `notify_parent` with status `failure` and explain what went wrong.\n\n**DO NOT:**\n- Commit, push, or file PRs (you are ephemeral \x2014 the parent owns the branch)\n- Create new branches\n- Run `git checkout` or `git switch`\n- Print findings to stdout instead of sending them via `notify_parent`"

-- | Pre-rendered research worker profile text. Same WASM workaround.
researchProfileText :: Text
researchProfileText = "## Completion Protocol (Research Worker)\nYou are a **research worker** \x2014 your job is to explore, read, search, and synthesize. You do NOT modify anything.\n\nYour workflow:\n\n1. **Read files** (`Read`, `Glob`, `Grep`) to understand code structure and patterns.\n2. **Search broadly** \x2014 check multiple files, grep for patterns, follow imports and references.\n3. **Synthesize findings** into a clear, structured report.\n4. **Call `notify_parent`** with status `success` and your findings.\n   - Structure your report with headings, bullet points, and code references (file:line).\n   - Lead with the answer, then supporting evidence.\n   - If you cannot find what was asked, call `notify_parent` with status `failure` explaining what you searched and what was missing.\n\n**DO NOT:**\n- Edit, write, or create any files\n- Run git commands (commit, push, checkout, branch)\n- File PRs or run build commands\n- Make changes to the codebase in any way"
