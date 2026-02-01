-- | Dev role tool list.
--
-- This module defines the tools available to the Dev role.
-- The Dev role is for spawned development agents that focus on code tasks.
-- They do NOT have access to agent control tools (spawn/cleanup/list agents).
module Dev.Tools
  ( DevTools,
  )
where

import ExoMonad.Guest.Tools.File (ReadFile, WriteFile)
import ExoMonad.Guest.Tools.Git (GitBranch, GitLog, GitStatus)
import ExoMonad.Guest.Tools.GitHub (GitHubGetIssue)

-- | Tools available to the Dev role.
--
-- Dev agents have access to:
-- - Git tools (branch, status, log)
-- - File tools (read, write)
-- - GitHub issue reading (get single issue)
--
-- Dev agents do NOT have:
-- - Agent control (spawn, cleanup, list)
-- - GitHub listing tools (list issues, list PRs)
type DevTools =
  '[ GitBranch,
     GitStatus,
     GitLog,
     ReadFile,
     WriteFile,
     GitHubGetIssue
   ]
