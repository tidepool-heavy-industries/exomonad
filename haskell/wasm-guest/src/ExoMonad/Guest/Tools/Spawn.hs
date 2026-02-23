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
import Data.Vector qualified as V
import Effects.Template qualified as Proto
import ExoMonad.Effects.Log (emitStructuredEvent)
import ExoMonad.Effects.Template qualified as Template
import ExoMonad.Guest.Effects.AgentControl qualified as AC
import ExoMonad.Guest.Proto (fromText, toText)
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
    -- Render task through template system with "leaf" profile for completion protocol.
    let req =
          Proto.RenderWorkerPromptRequest
            { Proto.renderWorkerPromptRequestTask = fromText $ slsTask args,
              Proto.renderWorkerPromptRequestReadFirst = V.empty,
              Proto.renderWorkerPromptRequestSteps = V.empty,
              Proto.renderWorkerPromptRequestVerify = V.empty,
              Proto.renderWorkerPromptRequestDoneCriteria = V.empty,
              Proto.renderWorkerPromptRequestBoundary = V.empty,
              Proto.renderWorkerPromptRequestContext = fromText "",
              Proto.renderWorkerPromptRequestProfiles = V.fromList [fromText "leaf"],
              Proto.renderWorkerPromptRequestContextFiles = V.empty,
              Proto.renderWorkerPromptRequestVerifyTemplates = V.empty
            }
    renderedTask <- Template.renderWorkerPrompt req >>= \case
      Right resp -> pure $ toText $ Proto.renderWorkerPromptResponseRendered resp
      Left _ -> pure $ slsTask args -- Fallback to raw task on render failure
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

-- | Assemble a structured WorkerSpec into a markdown prompt (fallback).
renderWorkerPromptFallback :: WorkerSpec -> Text
renderWorkerPromptFallback spec =
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
  toolDescription = "Spawn multiple Gemini worker agents in one call. Like Claude Code teammates but using Gemini — each gets a Zellij pane in YOUR tab, working in YOUR directory on YOUR branch (ephemeral, no isolation, no PR). Workers call notify_parent when done, which delivers a message to your conversation. After spawning, return immediately — do not poll or wait."
  toolSchema =
    genericToolSchemaWith @SpawnWorkersArgs
      [ ("specs", "Array of worker specifications")
      ]
  toolHandler args = do
    results <- forM (swsSpecs args) $ \spec -> do
      prompt <- case wsPrompt spec of
        Just p -> pure p
        Nothing -> do
          let req =
                Proto.RenderWorkerPromptRequest
                  { Proto.renderWorkerPromptRequestTask = fromText $ wsTask spec,
                    Proto.renderWorkerPromptRequestReadFirst = V.fromList $ map fromText $ maybe [] id (wsReadFirst spec),
                    Proto.renderWorkerPromptRequestSteps = V.fromList $ map fromText $ maybe [] id (wsSteps spec),
                    Proto.renderWorkerPromptRequestVerify = V.fromList $ map fromText $ maybe [] id (wsVerify spec),
                    Proto.renderWorkerPromptRequestDoneCriteria = V.fromList $ map fromText $ maybe [] id (wsDoneCriteria spec),
                    Proto.renderWorkerPromptRequestBoundary = V.fromList $ map fromText $ maybe [] id (wsBoundary spec),
                    Proto.renderWorkerPromptRequestContext = fromText $ maybe "" id (wsContext spec),
                    -- Always include "worker" profile for completion protocol
                    Proto.renderWorkerPromptRequestProfiles = V.fromList $ map fromText $ "worker" : maybe [] id (wsProfiles spec),
                    Proto.renderWorkerPromptRequestContextFiles = V.fromList $ map fromText $ maybe [] id (wsContextFiles spec),
                    Proto.renderWorkerPromptRequestVerifyTemplates = V.fromList $ map fromText $ maybe [] id (wsVerifyTemplates spec)
                  }
          Template.renderWorkerPrompt req >>= \case
            Right resp -> pure $ toText $ Proto.renderWorkerPromptResponseRendered resp
            Left _ -> pure $ renderWorkerPromptFallback spec

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
