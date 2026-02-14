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
import Data.Aeson (FromJSON, object, withObject, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Either (partitionEithers)
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Guest.Effects.AgentControl qualified as AC
import ExoMonad.Guest.Tool.Class
import GHC.Generics (Generic)
import Control.Monad.Freer (runM)

-- ============================================================================
-- SpawnSubtree
-- ============================================================================

data SpawnSubtree

data SpawnSubtreeArgs = SpawnSubtreeArgs
  { ssTask :: Text,
    ssBranchName :: Text,
    ssParentSessionId :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON SpawnSubtreeArgs where
  parseJSON = withObject "SpawnSubtreeArgs" $ \v ->
    SpawnSubtreeArgs
      <$> v .: "task"
      <*> v .: "branch_name"
      <*> v .: "parent_session_id"

instance MCPTool SpawnSubtree where
  type ToolArgs SpawnSubtree = SpawnSubtreeArgs
  toolName = "spawn_subtree"
  toolDescription = "Fork a worktree node off your current branch. Use when decomposing work into sub-problems that may need further decomposition. The child gets full coordination tools (can spawn its own children)."
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "required" .= (["task", "branch_name", "parent_session_id"] :: [Text]),
        "properties"
          .= object
            [ "task"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Description of the sub-problem to solve" :: Text)
                  ],
              "branch_name"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Branch name suffix (will be prefixed with current branch)" :: Text)
                  ],
              "parent_session_id"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Your CLAUDE_SESSION_ID (read from $CLAUDE_SESSION_ID env var). Enables context inheritance via --resume --fork-session." :: Text)
                  ]
            ]
      ]
  toolHandler args = do
    result <- runM $ AC.runAgentControl $ AC.spawnSubtree (ssTask args) (ssBranchName args) (ssParentSessionId args)
    case result of
      Left err -> pure $ errorResult err
      Right spawnResult -> pure $ successResult $ Aeson.toJSON spawnResult

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
    object
      [ "type" .= ("object" :: Text),
        "required" .= (["task", "branch_name"] :: [Text]),
        "properties"
          .= object
            [ "task"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Description of the sub-problem to solve" :: Text)
                  ],
              "branch_name"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Branch name suffix (will be prefixed with current branch)" :: Text)
                  ]
            ]
      ]
  toolHandler args = do
    result <- runM $ AC.runAgentControl $ AC.spawnLeafSubtree (slsTask args) (slsBranchName args)
    case result of
      Left err -> pure $ errorResult err
      Right spawnResult -> pure $ successResult $ Aeson.toJSON spawnResult

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
    Just p -> p  -- Raw prompt takes precedence (escape hatch)
    Nothing -> T.intercalate "\n\n" $ filter (not . T.null)
      [ "## Task: " <> wsTask spec
      , renderSection "READ FIRST" (fmap (map ("- " <>)) (wsReadFirst spec))
      , renderNumbered "STEPS" (wsSteps spec)
      , renderSection "VERIFY" (fmap (map ("```\n" <>) . map (<> "\n```")) (wsVerify spec))
      , renderSection "DONE CRITERIA" (fmap (map ("- " <>)) (wsDoneCriteria spec))
      , renderSection "BOUNDARY" (fmap (map ("- " <>)) (wsBoundary spec))
      , maybe "" (\c -> "### CONTEXT\n\n" <> c) (wsContext spec)
      ]
  where
    renderSection _ Nothing = ""
    renderSection _ (Just []) = ""
    renderSection heading (Just items) = "### " <> heading <> "\n" <> T.intercalate "\n" items

    renderNumbered _ Nothing = ""
    renderNumbered _ (Just []) = ""
    renderNumbered heading (Just items) = "### " <> heading <> "\n" <> T.intercalate "\n" (zipWith (\i s -> T.pack (show (i :: Int)) <> ". " <> s) [1..] items)

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
    object
      [ "type" .= ("object" :: Text),
        "required" .= (["specs"] :: [Text]),
        "properties"
          .= object
            [ "specs"
                .= object
                  [ "type" .= ("array" :: Text),
                    "description" .= ("Array of worker specifications" :: Text),
                    "items"
                      .= object
                        [ "type" .= ("object" :: Text),
                          "required" .= (["name", "task"] :: [Text]),
                          "properties"
                            .= object
                              [ "name"
                                  .= object
                                    [ "type" .= ("string" :: Text),
                                      "description" .= ("Human-readable name for the leaf agent" :: Text)
                                    ],
                                "task"
                                  .= object
                                    [ "type" .= ("string" :: Text),
                                      "description" .= ("Short description of the task" :: Text)
                                    ],
                                "read_first"
                                  .= object
                                    [ "type" .= ("array" :: Text),
                                      "items" .= object ["type" .= ("string" :: Text)],
                                      "description" .= ("Files the agent should read before starting" :: Text)
                                    ],
                                "steps"
                                  .= object
                                    [ "type" .= ("array" :: Text),
                                      "items" .= object ["type" .= ("string" :: Text)],
                                      "description" .= ("Numbered implementation steps" :: Text)
                                    ],
                                "verify"
                                  .= object
                                    [ "type" .= ("array" :: Text),
                                      "items" .= object ["type" .= ("string" :: Text)],
                                      "description" .= ("Commands to verify the work" :: Text)
                                    ],
                                "done_criteria"
                                  .= object
                                    [ "type" .= ("array" :: Text),
                                      "items" .= object ["type" .= ("string" :: Text)],
                                      "description" .= ("Acceptance criteria for completion" :: Text)
                                    ],
                                "boundary"
                                  .= object
                                    [ "type" .= ("array" :: Text),
                                      "items" .= object ["type" .= ("string" :: Text)],
                                      "description" .= ("Things the agent must NOT do" :: Text)
                                    ],
                                "context"
                                  .= object
                                    [ "type" .= ("string" :: Text),
                                      "description" .= ("Freeform context: code snippets, examples, detailed specs" :: Text)
                                    ],
                                "prompt"
                                  .= object
                                    [ "type" .= ("string" :: Text),
                                      "description" .= ("Raw prompt (escape hatch). If provided, all other fields except name are ignored." :: Text)
                                    ]
                              ]
                        ]
                  ]
            ]
      ]
  toolHandler args = do
    results <- forM (swsSpecs args) $ \spec ->
      runM $ AC.runAgentControl $ AC.spawnWorker (wsName spec) (renderWorkerPrompt spec)
    let (errs, successes) = partitionEithers results
    pure $
      successResult $
        object
          [ "spawned" .= map Aeson.toJSON successes,
            "errors" .= map Aeson.String errs
          ]