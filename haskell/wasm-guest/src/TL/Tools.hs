-- | TL (Tech Lead) role tool list.
--
-- This module defines the tools available to the TL role.
-- TL is the supervisor role that can spawn/manage dev agents.
-- It includes all Dev tools plus agent control tools.
module TL.Tools
  ( TLTools,
  )
where

import Dev.Tools (DevTools)
import ExoMonad.Guest.Tool.Class (type (:++))
import ExoMonad.Guest.Tools.Agent (CleanupAgents, ListAgents, SpawnAgents)
import ExoMonad.Guest.Tools.GitHub (GitHubListIssues, GitHubListPRs)

-- | Tools available to the TL role.
--
-- TL agents have all Dev tools plus:
-- - Agent control (spawn, cleanup, list)
-- - GitHub listing tools (list issues, list PRs)
--
-- This extends DevTools with TL-specific capabilities.
type TLTools =
  DevTools
    :++ '[ SpawnAgents,
           CleanupAgents,
           ListAgents,
           GitHubListIssues,
           GitHubListPRs
         ]
