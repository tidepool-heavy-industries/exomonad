-- | Hylo spawn primitives: spawn_subtree, spawn_workers.
module ExoMonad.Guest.Tools.Spawn
  ( SpawnSubtree,
    SpawnLeafSubtree,
    SpawnWorkers,
    SpawnSubtreeArgs (..),
    SpawnLeafSubtreeArgs (..),
    SpawnWorkersArgs (..),
    WorkerSpec (..),
    SpawnAcp,
    SpawnAcpArgs (..),
  )
where

import Control.Monad (forM, void)
import Data.Aeson (FromJSON, object, withObject, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Either (partitionEithers)
import Data.Text (Text)
import Data.Text qualified as T
import Effects.Log qualified as Log
import ExoMonad.Effects.Log (LogEmitEvent)
import ExoMonad.Guest.Effects.AgentControl qualified as AC
import ExoMonad.Guest.Tool.Class
import ExoMonad.Guest.Tool.Schema (JsonSchema (..), genericToolSchemaWith)
import ExoMonad.Guest.Tool.SuspendEffect (suspendEffect_)
import GHC.Generics (Generic)

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
    ssSecureMode :: Maybe Bool
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
      <*> v .:? "secure_mode"

instance MCPTool SpawnSubtree where
  type ToolArgs SpawnSubtree = SpawnSubtreeArgs
  toolName = "spawn_subtree"
  toolDescription = "Fork a Claude agent into its own worktree and Zellij tab. The child gets TL role (can spawn its own children). After spawning, return immediately — you will be notified via [CHILD COMPLETE] when it finishes. Do not poll or wait. Prefer spawn_leaf_subtree or spawn_workers for implementation work — Gemini agents are highly capable implementers and cost 10-30x less."
  toolSchema =
    genericToolSchemaWith @SpawnSubtreeArgs
      [ ("task", "Description of the sub-problem to solve"),
        ("branch_name", "Branch name suffix (will be prefixed with current branch)"),
        ("fork_session", "Whether to fork the parent's conversation context into the child (default: false). Set true to inherit context, but may fail if the session is stale or compacted."),
        ("permission_mode", "Permission mode for Claude (e.g., 'plan', 'default'). Omit for --dangerously-skip-permissions."),
        ("allowed_tools", "Tool patterns to allow (e.g., ['Read', 'Grep']). Omit for no restriction."),
        ("disallowed_tools", "Tool patterns to disallow (e.g., ['Bash']). Omit for no restriction."),
        ("secure_mode", "Enable secure isolated execution mode.")
      ]
  toolHandlerEff args = do
    let forkSession = maybe False id (ssForkSession args)
        perms = AC.PermissionFlags
          { AC.permMode = ssPermissionMode args,
            AC.allowedTools = maybe [] id (ssAllowedTools args),
            AC.disallowedTools = maybe [] id (ssDisallowedTools args)
          }
    result <- AC.spawnSubtree (ssTask args) (ssBranchName args) "" forkSession Nothing Nothing perms (ssSecureMode args)
    case result of
      Left err -> pure $ errorResult err
      Right spawnResult -> do
        let eventPayload = BSL.toStrict $ Aeson.encode $ object
              [ "slug" .= ssBranchName args,
                "agent_type" .= ("claude" :: Text),
                "task_summary" .= ssTask args
              ]
        void $ suspendEffect_ @LogEmitEvent (Log.EmitEventRequest
          { Log.emitEventRequestEventType = "agent.spawned",
            Log.emitEventRequestPayload = eventPayload,
            Log.emitEventRequestTimestamp = 0
          })
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
    slsSecureMode :: Maybe Bool
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
      <*> v .:? "secure_mode"

instance MCPTool SpawnLeafSubtree where
  type ToolArgs SpawnLeafSubtree = SpawnLeafSubtreeArgs
  toolName = "spawn_leaf_subtree"
  toolDescription = "Fork a Gemini agent into its own worktree and Zellij tab. Gets dev role (files PR, cannot spawn children). Gemini is a capable implementer — give it acceptance criteria and file paths, not line-by-line instructions. After spawning, return immediately — you will be notified via [CHILD COMPLETE] when it finishes."
  toolSchema =
    genericToolSchemaWith @SpawnLeafSubtreeArgs
      [ ("task", "Description of the sub-problem to solve"),
        ("branch_name", "Branch name suffix (will be prefixed with current branch)"),
        ("permission_mode", "Permission mode for the agent. Omit for --dangerously-skip-permissions."),
        ("allowed_tools", "Tool patterns to allow. Omit for no restriction."),
        ("disallowed_tools", "Tool patterns to disallow. Omit for no restriction."),
        ("secure_mode", "Enable secure isolated execution mode.")
      ]
  toolHandlerEff args = do
    -- WASM32 BUG WORKAROUND: Prompt's derived Semigroup (<>) hangs when
    -- evaluated inside the freer-simple coroutine context on GHC WASM32.
    -- Build prompts as raw Text instead of using the Prompt builder.
    let renderedTask = slsTask args <> "\n\n" <> leafProfileText
        perms = AC.PermissionFlags
          { AC.permMode = slsPermissionMode args,
            AC.allowedTools = maybe [] id (slsAllowedTools args),
            AC.disallowedTools = maybe [] id (slsDisallowedTools args)
          }
    result <- AC.spawnLeafSubtree renderedTask (slsBranchName args) Nothing Nothing perms (slsSecureMode args)
    case result of
      Left err -> pure $ errorResult err
      Right spawnResult -> do
        let eventPayload = BSL.toStrict $ Aeson.encode $ object
              [ "slug" .= slsBranchName args,
                "agent_type" .= ("gemini" :: Text),
                "task_summary" .= slsTask args
              ]
        void $ suspendEffect_ @LogEmitEvent (Log.EmitEventRequest
          { Log.emitEventRequestEventType = "agent.spawned",
            Log.emitEventRequestPayload = eventPayload,
            Log.emitEventRequestTimestamp = 0
          })
        pure $ successResult $ Aeson.toJSON spawnResult

-- ============================================================================
-- SpawnWorkers (batch)
-- ============================================================================

data SpawnWorkers

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
    wsPermissionMode :: Maybe Text,
    wsAllowedTools :: Maybe [Text],
    wsDisallowedTools :: Maybe [Text]
  }
  deriving (Show, Eq, Generic)

instance JsonSchema WorkerSpec where
  toSchema =
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
  toolDescription = "Spawn multiple Gemini worker agents in one call. Gemini agents are capable implementers — give them acceptance criteria, key file paths, and anti-patterns, not step-by-step code. Each gets a Zellij pane in YOUR tab, working in YOUR directory on YOUR branch (ephemeral, no isolation, no PR). Workers call notify_parent when done, which delivers a message to your conversation. After spawning, return immediately — do not poll or wait."
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
              AC.allowedTools = maybe [] id (wsAllowedTools spec),
              AC.disallowedTools = maybe [] id (wsDisallowedTools spec)
            }
      r <- AC.spawnWorker (wsName spec) prompt perms
      case r of
        Right _ -> do
          let eventPayload = BSL.toStrict $ Aeson.encode $ object
                [ "slug" .= wsName spec,
                  "agent_type" .= ("gemini-worker" :: Text),
                  "task_summary" .= wsTask spec
                ]
          void $ suspendEffect_ @LogEmitEvent (Log.EmitEventRequest
            { Log.emitEventRequestEventType = "agent.spawned",
              Log.emitEventRequestPayload = eventPayload,
              Log.emitEventRequestTimestamp = 0
            })
        Left _ -> pure ()
      pure r
    let (errs, successes) = partitionEithers results
    pure $
      successResult $
        object
          [ "spawned" .= map Aeson.toJSON successes,
            "errors" .= map Aeson.String errs
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
  toolDescription = "Spawn a Gemini agent via ACP (experimental). Runs headless — no Zellij tab. Gemini is a capable implementer — write specs, not code. Agent has MCP tools and communicates via structured protocol."
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
            AC.allowedTools = maybe [] id (saAllowedTools args),
            AC.disallowedTools = maybe [] id (saDisallowedTools args)
          }
    result <- AC.spawnAcp (saName args) renderedPrompt perms
    case result of
      Left err -> pure $ errorResult err
      Right spawnResult -> do
        let eventPayload = BSL.toStrict $ Aeson.encode $ object
              [ "slug" .= saName args,
                "agent_type" .= ("gemini-acp" :: Text),
                "task_summary" .= saName args
              ]
        void $ suspendEffect_ @LogEmitEvent (Log.EmitEventRequest
          { Log.emitEventRequestEventType = "agent.spawned",
            Log.emitEventRequestPayload = eventPayload,
            Log.emitEventRequestTimestamp = 0
          })
        pure $ successResult $ Aeson.toJSON spawnResult

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
    <> [workerProfileText]

-- | Pre-rendered leaf profile text.
leafProfileText :: Text
leafProfileText = "## Completion Protocol (Leaf Subtree)\nYou are a **leaf agent** in your own git worktree and branch. Your branch name follows the pattern `{parent}.{slug}`.\n\nWhen you are done:\n\n1. **Commit your changes** with a descriptive message.\n   - `git add <specific files>` \x2014 NEVER `git add .` or `git add -A`\n   - `git commit -m \"feat: <description>\"`\n2. **File a PR** using `file_pr` tool. The base branch is auto-detected from your branch name.\n3. **Wait for Copilot review** if it arrives. Address review comments, push fixes.\n4. **Call `notify_parent`** with status `success`, a one-line summary, and the PR number.\n   - If you failed after multiple attempts, call `notify_parent` with status `failure` and explain what went wrong.\n\n**DO NOT:**\n- Merge your own PR (the parent TL merges)\n- Push to main or any branch other than your own\n- Create additional branches"

-- | Pre-rendered worker profile text. Same WASM workaround.
workerProfileText :: Text
workerProfileText = "## Completion Protocol (Worker)\nYou are an **ephemeral worker** \x2014 you run in the parent's directory on the parent's branch. You do NOT have your own worktree or branch.\n\nWhen you are done:\n\n1. **Commit your changes** to the current branch with a descriptive message.\n   - `git add <specific files>` \x2014 NEVER `git add .` or `git add -A`\n   - `git commit -m \"feat: <description>\"`\n2. **Call `notify_parent`** with status `success` and a one-line summary of what you accomplished.\n   - If you failed after multiple attempts, call `notify_parent` with status `failure` and explain what went wrong.\n\n**DO NOT:**\n- File a PR (you have no branch to PR from)\n- Push to remote (you're on the parent's branch)\n- Create new branches\n- Run `git checkout` or `git switch`"
