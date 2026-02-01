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

import ExoMonad.Guest.Records.Agent (AgentTools (..), agentToolsHandler, agentToolsSchema)
import ExoMonad.Guest.Records.RefactorPreview (RefactorPreviewTools (..), refactorPreviewToolsHandler, refactorPreviewToolsSchema)
import ExoMonad.Guest.Tool.Mode (AsHandler, AsSchema, ToolMode ((:-)))
import GHC.Generics (Generic)

-- | Tools available to the TL role.
--
-- Orchestration-focused:
-- - spawn_agents: Spawn Claude Code agents for issues in isolated worktrees
-- - cleanup_agents: Clean up agent worktrees and Zellij tabs
-- - list_agents: List active agent worktrees
--
-- Refactoring-focused:
-- - refactor_preview: Generate ast-grep rule and preview diff
-- - enact_refactor: Apply the refactor
-- - steer_refactor: Refine the rule
-- - discard_refactor: Discard the plan
--
-- NOT included (use Claude Code native tools):
-- - git commands (git status, git log, etc.)
-- - file operations (Read, Write, Edit)
-- - GitHub queries (gh issue, gh pr)
data TLTools mode = TLTools
  { agent :: AgentTools mode,
    refactor :: RefactorPreviewTools mode
  }
  deriving (Generic)

-- | TL tools handler record.
tlToolsHandler :: TLTools AsHandler
tlToolsHandler =
  TLTools
    { agent = agentToolsHandler,
      refactor = refactorPreviewToolsHandler
    }

-- | TL tools schema record.
tlToolsSchema :: TLTools AsSchema
tlToolsSchema =
  TLTools
    { agent = agentToolsSchema,
      refactor = refactorPreviewToolsSchema
    }
