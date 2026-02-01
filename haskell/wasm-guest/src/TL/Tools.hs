-- | TL (Tech Lead) role tool record.
--
-- TL is the supervisor role that can spawn/manage dev agents.
-- Tools focus on orchestration, not duplicating CLI capabilities.
module TL.Tools
  ( TLTools (..),
    tlToolsHandler,
    tlToolsSchema,
  )
where

import GHC.Generics (Generic)

import ExoMonad.Guest.Records.Agent (AgentTools (..), agentToolsHandler, agentToolsSchema)
import ExoMonad.Guest.Tool.Mode (AsHandler, AsSchema, ToolMode ((:-)))

-- | Tools available to the TL role.
--
-- Orchestration-focused:
-- - spawn_agents: Spawn Claude Code agents for issues in isolated worktrees
-- - cleanup_agents: Clean up agent worktrees and Zellij tabs
-- - list_agents: List active agent worktrees
--
-- NOT included (use Claude Code native tools):
-- - git commands (git status, git log, etc.)
-- - file operations (Read, Write, Edit)
-- - GitHub queries (gh issue, gh pr)
data TLTools mode = TLTools
  { agent :: AgentTools mode
  }
  deriving (Generic)

-- | TL tools handler record.
tlToolsHandler :: TLTools AsHandler
tlToolsHandler =
  TLTools
    { agent = agentToolsHandler
    }

-- | TL tools schema record.
tlToolsSchema :: TLTools AsSchema
tlToolsSchema =
  TLTools
    { agent = agentToolsSchema
    }
