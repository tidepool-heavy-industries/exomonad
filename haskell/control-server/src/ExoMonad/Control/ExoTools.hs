-- | External Orchestration tools (Tier 1) as Graph DSL nodes.
--
-- Includes tools for interacting with GitHub Issues, git, and PRs.
module ExoMonad.Control.ExoTools
  ( -- * Exo Status
    exoStatusLogic,
    ExoStatusArgs (..),
    ExoStatusResult (..),

    -- * Spawn Agents
    spawnAgentsLogic,
    SpawnAgentsArgs (..),
    SpawnAgentsResult (..),
    findRepoRoot,

    -- * File PR
    filePRLogic,
    FilePRArgs (..),
    FilePRResult (..),
    PRInfo (..),

    -- * Helpers
    parseIssueNumber,
    slugify,
    extractIssueNumber,
  )
where

import ExoMonad.Control.ExoTools.FilePR
import ExoMonad.Control.ExoTools.Internal
import ExoMonad.Control.ExoTools.SpawnAgents
import ExoMonad.Control.ExoTools.Status
