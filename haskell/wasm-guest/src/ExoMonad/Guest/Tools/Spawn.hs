-- | Hylo spawn primitives: fork_wave, spawn_leaf_subtree, spawn_workers.
--
-- Core I/O functions are role-agnostic. Role-specific MCP wrappers
-- apply their own state transitions.
module ExoMonad.Guest.Tools.Spawn
  ( -- * Marker types
    ForkWave,
    SpawnLeafSubtree,
    SpawnWorkers,
    SpawnAcp,

    -- * Args types
    ForkWaveArgs (..),
    ForkWaveChild (..),
    SpawnLeafSubtreeArgs (..),
    SpawnWorkersArgs (..),
    WorkerSpec (..),
    WorkerType (..),
    SpawnAcpArgs (..),

    -- * Core functions (role wrappers call these)
    forkWaveCore,
    spawnLeafSubtreeCore,
    spawnWorkersCore,
    spawnAcpCore,

    -- * Result types
    ForkWaveResult (..),

    -- * Render functions
    forkWaveRender,
    spawnLeafRender,

    -- * Shared descriptions/schemas (role wrappers reuse these)
    forkWaveDescription,
    forkWaveSchema,
    spawnLeafSubtreeDescription,
    spawnLeafSubtreeSchema,
    spawnWorkersDescription,
    spawnWorkersSchema,

    -- * Helpers (re-exported for role code)
    spawnErrorMessage,
    hasCustomCode,
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
import Effects.Git qualified as Git
import ExoMonad.Effects.Git (GitGetStatus, GitHasUnpushedCommits)
import Effects.Log qualified as Log
import ExoMonad.Effects.Log (LogEmitEvent)
import ExoMonad.Guest.Effects.AgentControl qualified as AC
import ExoMonad.Guest.Tool.Class (MCPCallOutput, errorResult, successResult)
import ExoMonad.Guest.Tool.Schema (JsonSchema (..), genericToolSchemaWith)
import ExoMonad.Guest.Tool.SuspendEffect (suspendEffect, suspendEffect_)
import ExoMonad.Guest.Types (Effects)
import GHC.Generics (Generic)

-- ============================================================================
-- Helpers
-- ============================================================================

-- | Helper to convert EffectError to a human-readable message.
spawnErrorMessage :: EffectError -> Text
spawnErrorMessage (EffectError kind) = case kind of
  Just (EffectErrorKindCustom c) -> case customCode c of
    "worktree.branch_exists" -> "Branch already exists. Try a different slug."
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
-- ForkWave
-- ============================================================================

data ForkWave

data ForkWaveChild = ForkWaveChild
  { fwcSlug :: Text,
    fwcTask :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON ForkWaveChild where
  parseJSON = withObject "ForkWaveChild" $ \v ->
    ForkWaveChild
      <$> v .: "slug"
      <*> v .: "task"

instance JsonSchema ForkWaveChild where
  toSchema =
    Aeson.Object $
      genericToolSchemaWith @ForkWaveChild
        [ ("slug", "Branch name suffix (will be prefixed with current branch)"),
          ("task", "One-line task description — the child inherits your full context, so keep it brief")
        ]

data ForkWaveArgs = ForkWaveArgs
  { fwaChildren :: [ForkWaveChild]
  }
  deriving (Show, Eq, Generic)

instance FromJSON ForkWaveArgs where
  parseJSON = withObject "ForkWaveArgs" $ \v ->
    ForkWaveArgs <$> v .: "children"

-- | Structured result from fork_wave core logic.
data ForkWaveResult = ForkWaveResult
  { fwrSpawned :: [(Text, AC.SpawnResult)]  -- [(actualSlug, spawnResult)]
  , fwrErrors :: [Text]
  }

-- | Shared tool description for fork_wave.
forkWaveDescription :: Text
forkWaveDescription = "Fork any number of parallel Claude agents at this point in the conversation. Each inherits your full context window and starts in a worktree branched off your branch. Requires clean git state (committed and pushed). Use this tool to parallelize when work becomes applicative and can be split into independent streams. IMPORTANT: Create a team using TeamCreate BEFORE calling."

-- | Shared tool schema for fork_wave.
forkWaveSchema :: Aeson.Object
forkWaveSchema =
  genericToolSchemaWith @ForkWaveArgs
    [ ("children", "Array of children to spawn, each with a slug and task")
    ]

-- | Core fork_wave I/O: validate git state + spawn each child.
-- Returns structured result so wrappers can fire state transitions per child.
forkWaveCore :: ForkWaveArgs -> Eff Effects (Either Text ForkWaveResult)
forkWaveCore args = do
  -- Check for uncommitted changes
  statusResult <- suspendEffect @GitGetStatus (Git.GetStatusRequest {Git.getStatusRequestWorkingDir = "."})
  case statusResult of
    Right resp
      | not (null (Git.getStatusResponseDirtyFiles resp))
        || not (null (Git.getStatusResponseStagedFiles resp)) ->
        pure $ Left "Working tree has uncommitted changes. Commit and push your changes first, then call fork_wave."
    Left err ->
      pure $ Left ("Failed to check git status: " <> spawnErrorMessage err)
    _ -> do
      -- Check for unpushed commits
      unpushedResult <- suspendEffect @GitHasUnpushedCommits (Git.HasUnpushedCommitsRequest {Git.hasUnpushedCommitsRequestWorkingDir = ".", Git.hasUnpushedCommitsRequestRemote = "origin"})
      case unpushedResult of
        Right resp | Git.hasUnpushedCommitsResponseHasUnpushed resp ->
          pure $ Left "Local commits not pushed to remote. Run 'git push' first, then call fork_wave."
        Left err ->
          pure $ Left ("Failed to check unpushed commits: " <> spawnErrorMessage err)
        _ -> do
          -- Spawn each child with fork_session=true
          results <- forM (fwaChildren args) $ \child -> do
            let cfg = AC.SpawnSubtreeConfig
                  { AC.stcTask = fwcTask child
                  , AC.stcBranchName = fwcSlug child
                  , AC.stcForkSession = True
                  , AC.stcRole = Nothing
                  , AC.stcAgentType = AC.Claude
                  , AC.stcPerms = AC.defaultPermFlags
                  , AC.stcWorkingDir = Nothing
                  , AC.stcPermissions = Nothing
                  , AC.stcStandaloneRepo = False
                  , AC.stcAllowedDirs = []
                  }
            result <- AC.spawnSubtree cfg
            case result of
              Left err | hasCustomCode "worktree.branch_exists" err -> do
                let retrySlug = fwcSlug child <> "-2"
                let cfg' = cfg { AC.stcBranchName = retrySlug }
                result' <- AC.spawnSubtree cfg'
                case result' of
                  Left err' -> pure (Left (spawnErrorMessage err'))
                  Right spawnResult -> do
                    emitSpawnEvent retrySlug "claude" (fwcTask child)
                    pure (Right (retrySlug, spawnResult))
              Left err -> pure (Left (spawnErrorMessage err))
              Right spawnResult -> do
                emitSpawnEvent (fwcSlug child) "claude" (fwcTask child)
                pure (Right (fwcSlug child, spawnResult))

          let (errs, successes) = partitionEithers results
          pure $ Right $ ForkWaveResult
            { fwrSpawned = successes
            , fwrErrors = errs
            }

-- | Render a ForkWaveResult to MCPCallOutput.
forkWaveRender :: ForkWaveResult -> MCPCallOutput
forkWaveRender r =
  successResult $
    object
      [ "spawned" .= map (Aeson.toJSON . snd) (fwrSpawned r),
        "errors" .= map Aeson.String (fwrErrors r)
      ]

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

-- | Shared tool description for spawn_leaf_subtree.
spawnLeafSubtreeDescription :: Text
spawnLeafSubtreeDescription = "Fork a Gemini agent into its own worktree and tmux window. Gets dev role (files PR, cannot spawn children). Gemini is a capable implementer — give it acceptance criteria and file paths, not line-by-line instructions. IMPORTANT: You MUST create a team using TeamCreate BEFORE calling any spawn tool — without a team, child agent messages will not be delivered to you. After spawning, return immediately — you will be notified when the agent sends updates or when Copilot approves their PR."

-- | Shared tool schema for spawn_leaf_subtree.
spawnLeafSubtreeSchema :: Aeson.Object
spawnLeafSubtreeSchema =
  genericToolSchemaWith @SpawnLeafSubtreeArgs
    [ ("task", "Description of the sub-problem to solve"),
      ("branch_name", "Branch name suffix (will be prefixed with current branch)"),
      ("permission_mode", "Permission mode for the agent. Omit for --dangerously-skip-permissions."),
      ("allowed_tools", "Tool patterns to allow. Omit for no restriction."),
      ("disallowed_tools", "Tool patterns to disallow. Omit for no restriction."),
      ("standalone_repo", "When true, creates a standalone git repo instead of a worktree for information isolation."),
      ("allowed_dirs", "Directories from the parent project to be copied into the agent's context (only for standalone_repo).")
    ]

-- | Core spawn_leaf_subtree I/O.
-- Returns (actualSlug, spawnResult) on success.
spawnLeafSubtreeCore :: SpawnLeafSubtreeArgs -> Eff Effects (Either Text (Text, AC.SpawnResult))
spawnLeafSubtreeCore args = do
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
      let retrySlug = slsBranchName args <> "-2"
      let cfg' = cfg { AC.slcBranchName = retrySlug }
      result' <- AC.spawnLeafSubtree cfg'
      case result' of
        Left err' -> pure $ Left (spawnErrorMessage err')
        Right spawnResult -> do
          emitSpawnEvent retrySlug "gemini" (slsTask args)
          pure $ Right (retrySlug, spawnResult)
    Left err -> pure $ Left (spawnErrorMessage err)
    Right spawnResult -> do
      emitSpawnEvent (slsBranchName args) "gemini" (slsTask args)
      pure $ Right (slsBranchName args, spawnResult)

-- | Render a spawn leaf result to MCPCallOutput.
spawnLeafRender :: Either Text (Text, AC.SpawnResult) -> MCPCallOutput
spawnLeafRender (Left err) = errorResult err
spawnLeafRender (Right (_, sr)) = successResult $ Aeson.toJSON sr

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

-- | Shared tool description for spawn_workers.
spawnWorkersDescription :: Text
spawnWorkersDescription = "Spawn multiple Gemini worker agents in one call. PREFER WORKERS OVER DOING WORK YOURSELF — Gemini costs 10-30x less than your Opus tokens. Any task you can specify clearly (implementation, research, file edits, test writing) should be a worker. If it touches 2+ files or takes more than 5 tool calls, spawn a worker. Give them acceptance criteria, key file paths, and anti-patterns, not step-by-step code. Each gets a tmux pane in YOUR window, working in YOUR directory on YOUR branch (ephemeral, no isolation, no PR). Workers send messages via notify_parent. Set type to 'research' for read-only exploration workers that search, read, and report findings without modifying anything. IMPORTANT: You MUST create a team using TeamCreate BEFORE calling any spawn tool — without a team, child agent messages will not be delivered to you. After spawning, return immediately — do not poll or wait."

-- | Shared tool schema for spawn_workers.
spawnWorkersSchema :: Aeson.Object
spawnWorkersSchema =
  genericToolSchemaWith @SpawnWorkersArgs
    [ ("specs", "Array of worker specifications")
    ]

-- | Core spawn_workers I/O. No state transitions (workers are ephemeral).
spawnWorkersCore :: SpawnWorkersArgs -> Eff Effects MCPCallOutput
spawnWorkersCore args = do
  results <- forM (swsSpecs args) $ \spec -> do
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

-- | Core spawn_acp I/O.
spawnAcpCore :: SpawnAcpArgs -> Eff Effects MCPCallOutput
spawnAcpCore args = do
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

-- | Pre-rendered worker profile text.
workerProfileText :: Text
workerProfileText = "## Completion Protocol (Worker)\nYou are an **ephemeral worker** \x2014 you run in the parent's directory on the parent's branch. You do NOT have your own worktree or branch.\n\nWhen you are done:\n\n1. **Call `notify_parent`** with status `success` and a DETAILED message containing your complete findings.\n   - Include FULL code snippets, exact file paths with line numbers, and concrete data.\n   - Your parent CANNOT see your terminal output. `notify_parent` is your ONLY communication channel.\n   - A terse summary like \"Task complete\" is useless \x2014 include everything the parent needs to act on your findings.\n   - For research tasks: include the actual code/data you found, not just \"I found it.\"\n   - For implementation tasks: describe exactly what you changed and how to verify it.\n2. If you failed after multiple attempts, call `notify_parent` with status `failure` and explain what went wrong.\n\n**DO NOT:**\n- Commit, push, or file PRs (you are ephemeral \x2014 the parent owns the branch)\n- Create new branches\n- Run `git checkout` or `git switch`\n- Print findings to stdout instead of sending them via `notify_parent`"

-- | Pre-rendered research worker profile text.
researchProfileText :: Text
researchProfileText = "## Completion Protocol (Research Worker)\nYou are a **research worker** \x2014 your job is to explore, read, search, and synthesize. You do NOT modify anything.\n\nYour workflow:\n\n1. **Read files** (`Read`, `Glob`, `Grep`) to understand code structure and patterns.\n2. **Search broadly** \x2014 check multiple files, grep for patterns, follow imports and references.\n3. **Synthesize findings** into a clear, structured report.\n4. **Call `notify_parent`** with status `success` and your findings.\n   - Structure your report with headings, bullet points, and code references (file:line).\n   - Lead with the answer, then supporting evidence.\n   - If you cannot find what was asked, call `notify_parent` with status `failure` explaining what you searched and what was missing.\n\n**DO NOT:**\n- Edit, write, or create any files\n- Run git commands (commit, push, checkout, branch)\n- File PRs or run build commands\n- Make changes to the codebase in any way"
