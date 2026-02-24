-- | Hylo spawn primitives: spawn_subtree, spawn_workers.
module ExoMonad.Guest.Tools.Spawn
  ( SpawnSubtree,
    SpawnLeafSubtree,
    SpawnWorkers,
    SpawnSubtreeArgs (..),
    SpawnLeafSubtreeArgs (..),
    SpawnWorkersArgs (..),
    WorkerSpec (..),
  )
where

import Control.Monad (forM)
import Control.Monad.Freer (Eff, runM, sendM)
import Data.Aeson (FromJSON, object, withObject, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Either (partitionEithers)
import Data.Text (Text)
import ExoMonad.Effects.Log (emitStructuredEvent)
import ExoMonad.Guest.Effects.AgentControl qualified as AC
import ExoMonad.Guest.Prompt qualified as P
import ExoMonad.Guest.Tool.Class
import ExoMonad.Guest.Tool.Schema (JsonSchema (..), genericToolSchemaWith)
import GHC.Generics (Generic)

-- ============================================================================
-- SpawnSubtree
-- ============================================================================

data SpawnSubtree

data SpawnSubtreeArgs = SpawnSubtreeArgs
  { ssTask :: Text,
    ssBranchName :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON SpawnSubtreeArgs where
  parseJSON = withObject "SpawnSubtreeArgs" $ \v ->
    SpawnSubtreeArgs
      <$> v .: "task"
      <*> v .: "branch_name"

instance MCPTool SpawnSubtree where
  type ToolArgs SpawnSubtree = SpawnSubtreeArgs
  toolName = "spawn_subtree"
  toolDescription = "Fork a Claude agent into its own worktree and Zellij tab. The child gets TL role (can spawn its own children). After spawning, return immediately — you will be notified via [CHILD COMPLETE] when it finishes. Do not poll or wait."
  toolSchema =
    genericToolSchemaWith @SpawnSubtreeArgs
      [ ("task", "Description of the sub-problem to solve"),
        ("branch_name", "Branch name suffix (will be prefixed with current branch)")
      ]
  toolHandler args = do
    result <- runM $ AC.runAgentControl $ AC.spawnSubtree (ssTask args) (ssBranchName args) ""
    case result of
      Left err -> pure $ errorResult err
      Right spawnResult -> do
        emitStructuredEvent "agent.spawned" $
          object
            [ "slug" .= ssBranchName args,
              "agent_type" .= ("claude" :: Text),
              "task_summary" .= ssTask args
            ]
        pure $ successResult $ Aeson.toJSON spawnResult

  toolHandlerEff args = do
    result <- AC.runAgentControlSuspend $ AC.spawnSubtree (ssTask args) (ssBranchName args) ""
    case result of
      Left err -> pure $ errorResult err
      Right spawnResult -> do
        sendM $ emitStructuredEvent "agent.spawned" $
          object
            [ "slug" .= ssBranchName args,
              "agent_type" .= ("claude" :: Text),
              "task_summary" .= ssTask args
            ]
        pure $ successResult $ Aeson.toJSON spawnResult

-- ============================================================================
-- SpawnLeafSubtree
-- ============================================================================

data SpawnLeafSubtree

data SpawnLeafSubtreeArgs = SpawnLeafSubtreeArgs
  { slsTask :: Text,
    slsBranchName :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON SpawnLeafSubtreeArgs where
  parseJSON = withObject "SpawnLeafSubtreeArgs" $ \v ->
    SpawnLeafSubtreeArgs
      <$> v .: "task"
      <*> v .: "branch_name"

instance MCPTool SpawnLeafSubtree where
  type ToolArgs SpawnLeafSubtree = SpawnLeafSubtreeArgs
  toolName = "spawn_leaf_subtree"
  toolDescription = "Fork a Gemini agent into its own worktree and Zellij tab. Gets dev role (files PR, cannot spawn children). After spawning, return immediately — you will be notified via [CHILD COMPLETE] when it finishes."
  toolSchema =
    genericToolSchemaWith @SpawnLeafSubtreeArgs
      [ ("task", "Description of the sub-problem to solve"),
        ("branch_name", "Branch name suffix (will be prefixed with current branch)")
      ]
  toolHandler args = do
    let renderedTask = P.render $ P.task (slsTask args) <> P.leafProfile
    result <- runM $ AC.runAgentControl $ AC.spawnLeafSubtree renderedTask (slsBranchName args)
    case result of
      Left err -> pure $ errorResult err
      Right spawnResult -> do
        emitStructuredEvent "agent.spawned" $
          object
            [ "slug" .= slsBranchName args,
              "agent_type" .= ("gemini" :: Text),
              "task_summary" .= slsTask args
            ]
        pure $ successResult $ Aeson.toJSON spawnResult

  toolHandlerEff args = do
    let renderedTask = P.render $ P.task (slsTask args) <> P.leafProfile
    result <- AC.runAgentControlSuspend $ AC.spawnLeafSubtree renderedTask (slsBranchName args)
    case result of
      Left err -> pure $ errorResult err
      Right spawnResult -> do
        sendM $ emitStructuredEvent "agent.spawned" $
          object
            [ "slug" .= slsBranchName args,
              "agent_type" .= ("gemini" :: Text),
              "task_summary" .= slsTask args
            ]
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
    wsVerifyTemplates :: Maybe [Text]
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
        ("verify_templates", "Verification script templates")
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
  toolDescription = "Spawn multiple Gemini worker agents in one call. Like Claude Code teammates but using Gemini — each gets a Zellij pane in YOUR tab, working in YOUR directory on YOUR branch (ephemeral, no isolation, no PR). Workers call notify_parent when done, which delivers a message to your conversation. After spawning, return immediately — do not poll or wait."
  toolSchema =
    genericToolSchemaWith @SpawnWorkersArgs
      [ ("specs", "Array of worker specifications")
      ]
  toolHandler args = do
    results <- forM (swsSpecs args) $ \spec -> do
      let prompt = case wsPrompt spec of
            Just p -> p
            Nothing ->
              P.render $
                P.task (wsTask spec)
                  <> maybe mempty P.boundary (wsBoundary spec)
                  <> maybe mempty P.readFirst (wsReadFirst spec)
                  <> maybe mempty P.steps (wsSteps spec)
                  <> maybe mempty P.context (wsContext spec)
                  <> maybe mempty P.verify (wsVerify spec)
                  <> maybe mempty P.doneCriteria (wsDoneCriteria spec)
                  <> P.workerProfile

      r <- runM $ AC.runAgentControl $ AC.spawnWorker (wsName spec) prompt
      case r of
        Right _ ->
          emitStructuredEvent "agent.spawned" $
            object
              [ "slug" .= wsName spec,
                "agent_type" .= ("gemini-worker" :: Text),
                "task_summary" .= wsTask spec
              ]
        Left _ -> pure ()
      pure r
    let (errs, successes) = partitionEithers results
    pure $
      successResult $
        object
          [ "spawned" .= map Aeson.toJSON successes,
            "errors" .= map Aeson.String errs
          ]

  toolHandlerEff args = do
    results <- forM (swsSpecs args) $ \spec -> do
      let prompt = case wsPrompt spec of
            Just p -> p
            Nothing ->
              P.render $
                P.task (wsTask spec)
                  <> maybe mempty P.boundary (wsBoundary spec)
                  <> maybe mempty P.readFirst (wsReadFirst spec)
                  <> maybe mempty P.steps (wsSteps spec)
                  <> maybe mempty P.context (wsContext spec)
                  <> maybe mempty P.verify (wsVerify spec)
                  <> maybe mempty P.doneCriteria (wsDoneCriteria spec)
                  <> P.workerProfile
      r <- AC.runAgentControlSuspend $ AC.spawnWorker (wsName spec) prompt
      case r of
        Right _ ->
          sendM $ emitStructuredEvent "agent.spawned" $
            object
              [ "slug" .= wsName spec,
                "agent_type" .= ("gemini-worker" :: Text),
                "task_summary" .= wsTask spec
              ]
        Left _ -> pure ()
      pure r
    let (errs, successes) = partitionEithers results
    pure $
      successResult $
        object
          [ "spawned" .= map Aeson.toJSON successes,
            "errors" .= map Aeson.String errs
          ]
