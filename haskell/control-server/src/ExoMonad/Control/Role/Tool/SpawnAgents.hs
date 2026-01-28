{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | SpawnAgents tool definition for the Role DSL.
--
-- This module provides the 'ToolSpec' instance for the SpawnAgents tool,
-- which spawns parallel worker agents in Zellij tabs.
--
-- @
-- -- Tool marker type
-- data SpawnAgents = SpawnAgents
--
-- -- ToolSpec instance provides metadata
-- instance ToolSpec SpawnAgents where
--   type Args SpawnAgents = SpawnAgentsArgs
--   type Result SpawnAgents = SpawnAgentsResult
--   toolName = "spawn_agents"
--   toolDescription = "Spawn parallel worker agents in isolated worktrees"
-- @
module ExoMonad.Control.Role.Tool.SpawnAgents
  ( -- * Tool Marker
    SpawnAgents(..)

    -- * Re-exports
  , SpawnAgentsArgs(..)
  , SpawnAgentsResult(..)
  ) where

import ExoMonad.Control.Role.Types (ToolSpec(..))
import ExoMonad.Control.ExoTools.SpawnAgents.Types
  ( SpawnAgentsArgs(..)
  , SpawnAgentsResult(..)
  )

-- | Marker type for the SpawnAgents tool.
--
-- This is a phantom type that carries tool metadata via the 'ToolSpec' typeclass.
-- The actual handler is provided when constructing a server value.
data SpawnAgents = SpawnAgents

-- | ToolSpec instance for SpawnAgents.
--
-- This provides the tool name, description, and associated types for MCP registration
-- and handler type computation.
instance ToolSpec SpawnAgents where
  type Args SpawnAgents = SpawnAgentsArgs
  type Result SpawnAgents = SpawnAgentsResult
  toolName = "spawn_agents"
  toolDescription = "Spawn parallel worker agents in isolated worktrees. Creates git worktrees for issues and launches Claude sessions in Zellij tabs."
