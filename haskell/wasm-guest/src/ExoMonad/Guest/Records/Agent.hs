-- | Agent control tool record and handlers.
--
-- TL-only tools for spawning and managing sub-agents.
module ExoMonad.Guest.Records.Agent
  ( AgentTools (..),
    agentToolsHandler,
    agentToolsSchema,
    agentTools, -- Convenience alias for handler
  )
where

import ExoMonad.Guest.Tool.Mode (AsHandler, AsSchema, ToolMode ((:-)), mkHandler, mkSchema)
import ExoMonad.Guest.Tools.Agent (CleanupAgents, CleanupMergedAgents, ListAgents, SpawnAgents, SpawnGeminiTeammate)
import GHC.Generics (Generic)

-- | Agent control tools record.
data AgentTools mode = AgentTools
  { spawnAgents :: mode :- SpawnAgents,
    spawnGeminiTeammate :: mode :- SpawnGeminiTeammate,
    cleanupAgents :: mode :- CleanupAgents,
    cleanupMergedAgents :: mode :- CleanupMergedAgents,
    listAgents :: mode :- ListAgents
  }
  deriving (Generic)

-- | Agent tools handler record.
agentToolsHandler :: AgentTools AsHandler
agentToolsHandler =
  AgentTools
    { spawnAgents = mkHandler @SpawnAgents,
      spawnGeminiTeammate = mkHandler @SpawnGeminiTeammate,
      cleanupAgents = mkHandler @CleanupAgents,
      cleanupMergedAgents = mkHandler @CleanupMergedAgents,
      listAgents = mkHandler @ListAgents
    }

-- | Agent tools schema record.
agentToolsSchema :: AgentTools AsSchema
agentToolsSchema =
  AgentTools
    { spawnAgents = mkSchema @SpawnAgents,
      spawnGeminiTeammate = mkSchema @SpawnGeminiTeammate,
      cleanupAgents = mkSchema @CleanupAgents,
      cleanupMergedAgents = mkSchema @CleanupMergedAgents,
      listAgents = mkSchema @ListAgents
    }

-- | Default handler instance for use in Role.hs
agentTools :: AgentTools AsHandler
agentTools = agentToolsHandler
