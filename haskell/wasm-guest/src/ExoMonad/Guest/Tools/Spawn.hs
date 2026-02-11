-- | Hylo spawn primitives: spawn_subtree and spawn_worker.
module ExoMonad.Guest.Tools.Spawn
  ( SpawnSubtree,
    SpawnWorker,
    SpawnSubtreeArgs (..),
    SpawnWorkerArgs (..),
  )
where

import Data.Aeson (FromJSON, object, withObject, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
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
-- SpawnWorker
-- ============================================================================

data SpawnWorker

data SpawnWorkerArgs = SpawnWorkerArgs
  { swName :: Text,
    swPrompt :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON SpawnWorkerArgs where
  parseJSON = withObject "SpawnWorkerArgs" $ \v ->
    SpawnWorkerArgs
      <$> v .: "name"
      <*> v .: "prompt"

instance MCPTool SpawnWorker where
  type ToolArgs SpawnWorker = SpawnWorkerArgs
  toolName = "spawn_worker"
  toolDescription = "Spawn an agent for a focused, bounded task. Use when the work is concrete enough to execute without further decomposition. The child works and exits."
  toolSchema =
    object
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
  toolHandler args = do
    result <- runM $ AC.runAgentControl $ AC.spawnWorker (swName args) (swPrompt args)
    case result of
      Left err -> pure $ errorResult err
      Right spawnResult -> pure $ successResult $ Aeson.toJSON spawnResult
