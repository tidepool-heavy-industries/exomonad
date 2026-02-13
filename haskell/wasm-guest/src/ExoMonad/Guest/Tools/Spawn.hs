-- | Hylo spawn primitives: spawn_subtree, spawn_workers.
module ExoMonad.Guest.Tools.Spawn
  ( SpawnSubtree,
    SpawnWorkers,
    SpawnSubtreeArgs (..),
    SpawnWorkersArgs (..),
    WorkerSpec (..),
  )
where

import Control.Monad (forM)
import Data.Aeson (FromJSON, object, withObject, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Either (partitionEithers)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import ExoMonad.Guest.Effects.AgentControl qualified as AC
import ExoMonad.Guest.Tool.Class
import GHC.Generics (Generic)
import Polysemy (runM)

-- ============================================================================
-- SpawnSubtree
-- ============================================================================

data SpawnSubtree

data SpawnSubtreeArgs = SpawnSubtreeArgs
  { ssTask :: Text,
    ssBranchName :: Text,
    ssContext :: Maybe Text,
    ssAgentType :: Maybe AC.AgentType
  }
  deriving (Show, Eq, Generic)

instance FromJSON SpawnSubtreeArgs where
  parseJSON = withObject "SpawnSubtreeArgs" $ \v ->
    SpawnSubtreeArgs
      <$> v .: "task"
      <*> v .: "branch_name"
      <*> v .:? "context"
      <*> v .:? "agent_type"

instance MCPTool SpawnSubtree where
  type ToolArgs SpawnSubtree = SpawnSubtreeArgs
  toolName = "spawn_subtree"
  toolDescription = "Fork a worktree node off your current branch. Use when decomposing work into sub-problems that may need further decomposition. The child gets full coordination tools (can spawn its own children)."
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
                  ],
              "context"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Additional context to pass to the child agent" :: Text)
                  ],
              "agent_type"
                .= object
                  [ "type" .= ("string" :: Text),
                    "enum" .= (["claude", "gemini"] :: [Text]),
                    "description" .= ("Agent type (default: claude)" :: Text)
                  ]
            ]
      ]
  toolHandler args = do
    let agentTy = fromMaybe AC.Claude (ssAgentType args)
    result <- runM $ AC.runAgentControl $ AC.spawnSubtree (ssTask args) (ssBranchName args) (ssContext args) agentTy
    case result of
      Left err -> pure $ errorResult err
      Right spawnResult -> pure $ successResult $ Aeson.toJSON spawnResult

-- ============================================================================
-- SpawnWorkers (batch)
-- ============================================================================

data SpawnWorkers

data WorkerSpec = WorkerSpec
  { wsName :: Text,
    wsPrompt :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON WorkerSpec where
  parseJSON = withObject "WorkerSpec" $ \v ->
    WorkerSpec
      <$> v .: "name"
      <*> v .: "prompt"

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
                          "required" .= (["name", "prompt"] :: [Text]),
                          "properties"
                            .= object
                              [ "name"
                                  .= object
                                    [ "type" .= ("string" :: Text),
                                      "description" .= ("Human-readable name for the leaf agent" :: Text)
                                    ],
                                "prompt"
                                  .= object
                                    [ "type" .= ("string" :: Text),
                                      "description" .= ("Implementation instructions for the agent" :: Text)
                                    ]
                              ]
                        ]
                  ]
            ]
      ]
  toolHandler args = do
    results <- forM (swsSpecs args) $ \spec ->
      runM $ AC.runAgentControl $ AC.spawnWorker (wsName spec) (wsPrompt spec)
    let (errs, successes) = partitionEithers results
    pure $
      successResult $
        object
          [ "spawned" .= map Aeson.toJSON successes,
            "errors" .= map Aeson.String errs
          ]
