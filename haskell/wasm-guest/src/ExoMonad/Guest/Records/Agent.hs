-- | Agent control tool record and handlers.
--
-- TL-only tools for spawning and managing sub-agents.
module ExoMonad.Guest.Records.Agent
  ( AgentTools (..),
    agentToolsHandler,
    agentToolsSchema,
  )
where

import GHC.Generics (Generic)

import ExoMonad.Guest.Tool.Mode (AsHandler, AsSchema, Handler, Schema, ToolMode ((:-)), mkHandler, mkSchema)
import ExoMonad.Guest.Tools.Agent (CleanupAgents, ListAgents, SpawnAgents)

-- | Agent control tools record.
data AgentTools mode = AgentTools
  { spawnAgents :: mode :- SpawnAgents,
    cleanupAgents :: mode :- CleanupAgents,
    listAgents :: mode :- ListAgents
  }
  deriving (Generic)

-- | Agent tools handler record.
agentToolsHandler :: AgentTools AsHandler
agentToolsHandler =
  AgentTools
    { spawnAgents = mkHandler @SpawnAgents,
      cleanupAgents = mkHandler @CleanupAgents,
      listAgents = mkHandler @ListAgents
    }

-- | Agent tools schema record.
agentToolsSchema :: AgentTools AsSchema
agentToolsSchema =
  AgentTools
    { spawnAgents = mkSchema @SpawnAgents,
      cleanupAgents = mkSchema @CleanupAgents,
      listAgents = mkSchema @ListAgents
    }
