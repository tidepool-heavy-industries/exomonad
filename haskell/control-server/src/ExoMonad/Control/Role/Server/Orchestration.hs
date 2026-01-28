{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoStarIsType #-}

-- | Orchestration Server for the Role DSL.
--
-- This server groups tools for orchestrating parallel agent work:
-- - spawn_agents: Create worktrees and launch agents
-- - exo_status: Get development context
--
-- = Usage
--
-- @
-- -- Schema mode: extract tool definitions
-- orchestrationSchema :: OrchestrationServer AsSchema es
-- orchestrationSchema = OrchestrationServer
--   { osDescription = "Tools for orchestrating parallel agent work"
--   , osSpawnAgents = spawnAgentsInfo  -- MCPToolInfo from reification
--   , osExoStatus   = exoStatusInfo
--   }
--
-- -- Handler mode: provide implementations
-- orchestrationHandlers :: OrchestrationServer (AsHandler es) es
-- orchestrationHandlers = OrchestrationServer
--   { osDescription = "Tools for orchestrating parallel agent work"
--   , osSpawnAgents = spawnAgentsHandler  -- Args -> Eff es Result
--   , osExoStatus   = exoStatusHandler
--   }
-- @
module ExoMonad.Control.Role.Server.Orchestration
  ( OrchestrationServer(..)
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import GHC.Generics (Generic)

import ExoMonad.Control.Role.Types (ToolField)
import ExoMonad.Control.Role.Tool.SpawnAgents (SpawnAgents)
import ExoMonad.Control.Role.Tool.ExoStatus (ExoStatus)

-- | MCP Server for orchestration tools.
--
-- In 'AsSchema' mode, tool fields become 'MCPToolInfo' for registration.
-- In 'AsHandler' mode, tool fields become handler functions.
data OrchestrationServer mode (es :: [Type -> Type]) = OrchestrationServer
  { osDescription :: Text
    -- ^ Server description for MCP listing
  , osSpawnAgents :: ToolField mode es SpawnAgents
    -- ^ Spawn parallel worker agents
  , osExoStatus   :: ToolField mode es ExoStatus
    -- ^ Get development context
  }
  deriving Generic
