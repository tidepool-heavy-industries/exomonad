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
import Control.Monad.Freer (runM)
import Data.Aeson (FromJSON, object, withObject, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Either (partitionEithers)
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Effects.Log (emitStructuredEvent)
import ExoMonad.Guest.Effects.AgentControl qualified as AC
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
  toolDescription = "Fork a worktree node off your current branch. Use when decomposing work into sub-problems that may need further decomposition. The child gets full coordination tools (can spawn its own children)."
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
  toolDescription = "Fork a worktree for a Gemini leaf agent. Gets own branch for PR filing but cannot spawn children."
  toolSchema =
    genericToolSchemaWith @SpawnLeafSubtreeArgs
      [ ("task", "Description of the sub-problem to solve"),
        ("branch_name", "Branch name suffix (will be prefixed with current branch)")
      ]
  toolHandler args = do
    result <- runM $ AC.runAgentControl $ AC.spawnLeafSubtree (slsTask args) (slsBranchName args)
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
    wsPrompt :: Maybe Text
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
        ("prompt", "Raw prompt (escape hatch). If provided, all other fields except name are ignored.")
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

-- | Assemble a structured WorkerSpec into a markdown prompt.
renderWorkerPrompt :: WorkerSpec -> Text
renderWorkerPrompt spec =
  case wsPrompt spec of
    Just p -> p -- Raw prompt takes precedence (escape hatch)
    Nothing ->
      T.intercalate "\n\n" $
        filter
          (not . T.null)
          [ "## Task: " <> wsTask spec,
            renderSection "READ FIRST" (fmap (map ("- " <>)) (wsReadFirst spec)),
            renderNumbered "STEPS" (wsSteps spec),
            renderSection "VERIFY" (fmap (map ("```\n" <>) . map (<> "\n```")) (wsVerify spec)),
            renderSection "DONE CRITERIA" (fmap (map ("- " <>)) (wsDoneCriteria spec)),
            renderSection "BOUNDARY" (fmap (map ("- " <>)) (wsBoundary spec)),
            maybe "" (\c -> "### CONTEXT\n\n" <> c) (wsContext spec)
          ]
  where
    renderSection _ Nothing = ""
    renderSection _ (Just []) = ""
    renderSection heading (Just items) = "### " <> heading <> "\n" <> T.intercalate "\n" items

    renderNumbered _ Nothing = ""
    renderNumbered _ (Just []) = ""
    renderNumbered heading (Just items) = "### " <> heading <> "\n" <> T.intercalate "\n" (zipWith (\i s -> T.pack (show (i :: Int)) <> ". " <> s) [1 ..] items)

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
  toolDescription = "Spawn multiple worker agents in one call. Each gets a Zellij pane in the current worktree."
  toolSchema =
    genericToolSchemaWith @SpawnWorkersArgs
      [ ("specs", "Array of worker specifications")
      ]
  toolHandler args = do

    results <- forM (swsSpecs args) $ \spec -> do
      r <- runM $ AC.runAgentControl $ AC.spawnWorker (wsName spec) (renderWorkerPrompt spec)
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
