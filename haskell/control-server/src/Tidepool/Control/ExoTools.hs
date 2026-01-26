-- | External Orchestration tools (Tier 1) as Graph DSL nodes.
--
-- Includes tools for interacting with GitHub Issues, git, and PRs.
--
-- Note: exo_complete and pre_commit_check have been folded into the Stop hook.
-- Their logic modules are kept for internal use but are no longer exported as MCP tools.
module Tidepool.Control.ExoTools
  ( -- * Exo Status
    ExoStatusGraph(..)
  , exoStatusHandlers
  , exoStatusLogic
  , ExoStatusArgs(..)
  , ExoStatusResult(..)

    -- * Spawn Agents
  , SpawnAgentsGraph(..)
  , spawnAgentsHandlers
  , spawnAgentsLogic
  , SpawnAgentsArgs(..)
  , SpawnAgentsResult(..)
  , findHangarRoot

    -- * File PR
  , FilePRGraph(..)
  , filePRHandlers
  , filePRLogic
  , FilePRArgs(..)
  , FilePRResult(..)
  , PRInfo(..)

    -- * Pr Review Status
  , PrReviewStatusGraph(..)
  , prReviewStatusHandlers
  , prReviewStatusLogic
  , PrReviewStatusArgs(..)
  , PrReviewStatusResult(..)
  , AuthorFeedback(..)
  , FeedbackSummary(..)

    -- * Helpers
  , parseIssueNumber
  , slugify
  , extractIssueNumber
  ) where

import Tidepool.Control.ExoTools.Status
import Tidepool.Control.ExoTools.SpawnAgents
import Tidepool.Control.ExoTools.FilePR
import Tidepool.Control.ExoTools.PrReviewStatus
import Tidepool.Control.ExoTools.Internal
