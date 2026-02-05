-- | Agent control tool definitions and handlers.
module ExoMonad.Guest.Tools.Agent
  ( -- * Tool types
    SpawnAgents,
    CleanupAgents,
    CleanupMergedAgents,
    ListAgents,

    -- * Argument types (exported for tests)
    SpawnAgentsArgs (..),
    CleanupAgentsArgs (..),
    CleanupMergedAgentsArgs (..),
    ListAgentsArgs (..),

    -- * Re-export AgentType for use in other modules
    AC.AgentType (..),
  )
where

import Data.Aeson (FromJSON, object, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import ExoMonad.Guest.Effects.AgentControl qualified as AC
import ExoMonad.Guest.Tool.Class
import GHC.Generics (Generic)
import Polysemy (runM)

-- ============================================================================
-- SpawnAgents
-- ============================================================================

-- | Spawn Claude Code agents for GitHub issues in isolated worktrees.
data SpawnAgents

data SpawnAgentsArgs = SpawnAgentsArgs
  { saIssues :: [Text],
    saOwner :: Text,
    saRepo :: Text,
    saWorktreeDir :: Maybe Text,
    saAgentType :: Maybe AC.AgentType
  }
  deriving (Show, Eq, Generic)

instance FromJSON SpawnAgentsArgs where
  parseJSON = Aeson.withObject "SpawnAgentsArgs" $ \v ->
    SpawnAgentsArgs
      <$> v .: "issues"
      <*> v .: "owner"
      <*> v .: "repo"
      <*> v .:? "worktree_dir"
      <*> v .:? "agent_type"

instance MCPTool SpawnAgents where
  type ToolArgs SpawnAgents = SpawnAgentsArgs
  toolName = "spawn_agents"
  toolDescription = "Spawn agents (Claude/Gemini) for GitHub issues in isolated worktrees"
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "required" .= (["issues", "owner", "repo"] :: [Text]),
        "properties"
          .= object
            [ "issues"
                .= object
                  [ "type" .= ("array" :: Text),
                    "items" .= object ["type" .= ("string" :: Text)],
                    "description" .= ("GitHub issue numbers to spawn agents for" :: Text)
                  ],
              "owner"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("GitHub repository owner" :: Text)
                  ],
              "repo"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("GitHub repository name" :: Text)
                  ],
              "worktree_dir"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Base directory for worktrees (default: .exomonad/worktrees)" :: Text)
                  ],
              "agent_type"
                .= object
                  [ "type" .= ("string" :: Text),
                    "enum" .= (["claude", "gemini"] :: [Text]),
                    "description" .= ("Agent type (default: gemini)" :: Text)
                  ]
            ]
      ]
  toolHandler args = do
    let opts =
          AC.SpawnOptions
            { AC.owner = saOwner args,
              AC.repo = saRepo args,
              AC.worktreeDir = saWorktreeDir args,
              AC.agentType = fromMaybe AC.Gemini (saAgentType args) -- Default applied at edge
            }
    result <- runM $ AC.runAgentControl $ AC.spawnAgents (saIssues args) opts
    pure $ successResult $ Aeson.toJSON result

-- ============================================================================
-- CleanupAgents
-- ============================================================================

-- | Clean up agent worktrees and close their Zellij tabs.
data CleanupAgents

data CleanupAgentsArgs = CleanupAgentsArgs
  { caIssues :: [Text],
    caForce :: Maybe Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON CleanupAgentsArgs where
  parseJSON = Aeson.withObject "CleanupAgentsArgs" $ \v ->
    CleanupAgentsArgs
      <$> v .: "issues"
      <*> v .:? "force"

instance MCPTool CleanupAgents where
  type ToolArgs CleanupAgents = CleanupAgentsArgs
  toolName = "cleanup_agents"
  toolDescription = "Clean up agent worktrees and close their Zellij tabs"
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "required" .= (["issues"] :: [Text]),
        "properties"
          .= object
            [ "issues"
                .= object
                  [ "type" .= ("array" :: Text),
                    "items" .= object ["type" .= ("string" :: Text)],
                    "description" .= ("Issue IDs to clean up" :: Text)
                  ],
              "force"
                .= object
                  [ "type" .= ("boolean" :: Text),
                    "description" .= ("Force deletion even if worktree has uncommitted changes (default: false)" :: Text)
                  ]
            ]
      ]
  toolHandler args = do
    let force = maybe False id (caForce args)
    result <- runM $ AC.runAgentControl $ AC.cleanupAgents (caIssues args) force
    pure $ successResult $ Aeson.toJSON result

-- ============================================================================
-- CleanupMergedAgents
-- ============================================================================

-- | Clean up agent worktrees for merged branches.
data CleanupMergedAgents

data CleanupMergedAgentsArgs = CleanupMergedAgentsArgs
  deriving (Show, Eq, Generic)

instance FromJSON CleanupMergedAgentsArgs where
  parseJSON = Aeson.withObject "CleanupMergedAgentsArgs" $ \_ ->
    pure CleanupMergedAgentsArgs

instance MCPTool CleanupMergedAgents where
  type ToolArgs CleanupMergedAgents = CleanupMergedAgentsArgs
  toolName = "cleanup_merged_agents"
  toolDescription = "Clean up agent worktrees whose branches have been merged to main"
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "properties" .= object []
      ]
  toolHandler _args = do
    result <- runM $ AC.runAgentControl AC.cleanupMergedAgents
    pure $ successResult $ Aeson.toJSON result

-- ============================================================================
-- ListAgents
-- ============================================================================

-- | List active agent worktrees.
data ListAgents

data ListAgentsArgs = ListAgentsArgs
  deriving (Show, Eq, Generic)

instance FromJSON ListAgentsArgs where
  parseJSON = Aeson.withObject "ListAgentsArgs" $ \_ ->
    pure ListAgentsArgs

instance MCPTool ListAgents where
  type ToolArgs ListAgents = ListAgentsArgs
  toolName = "list_agents"
  toolDescription = "List active agent worktrees"
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "properties" .= object []
      ]
  toolHandler _args = do
    result <- runM $ AC.runAgentControl AC.listAgents
    case result of
      Left err -> pure $ errorResult err
      Right agents -> pure $ successResult $ Aeson.toJSON agents
