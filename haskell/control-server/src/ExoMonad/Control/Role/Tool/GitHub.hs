{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | GitHub tool definitions for the Role DSL.
--
-- Minimal read-only GitHub tools for the initial port.
module ExoMonad.Control.Role.Tool.GitHub
  ( -- * List Issues
    GHIssueList(..)
  , GHIssueListArgs(..)
  , GHIssueListResult(..)

    -- * Show Issue
  , GHIssueShow(..)
  , GHIssueShowArgs(..)
  , GHIssueShowResult(..)
  ) where

import ExoMonad.Control.Role.Types (ToolSpec(..))
import ExoMonad.Control.GHTools.Types
  ( GHIssueListArgs(..)
  , GHIssueListResult(..)
  , GHIssueShowArgs(..)
  , GHIssueShowResult(..)
  )

-- ════════════════════════════════════════════════════════════════════════════
-- GH ISSUE LIST
-- ════════════════════════════════════════════════════════════════════════════

-- | Marker type for the GHIssueList tool.
data GHIssueList = GHIssueList

instance ToolSpec GHIssueList where
  type Args GHIssueList = GHIssueListArgs
  type Result GHIssueList = GHIssueListResult
  toolName = "gh_issue_list"
  toolDescription = "List GitHub issues. Filter by status (open/closed) and labels."

-- ════════════════════════════════════════════════════════════════════════════
-- GH ISSUE SHOW
-- ════════════════════════════════════════════════════════════════════════════

-- | Marker type for the GHIssueShow tool.
data GHIssueShow = GHIssueShow

instance ToolSpec GHIssueShow where
  type Args GHIssueShow = GHIssueShowArgs
  type Result GHIssueShow = GHIssueShowResult
  toolName = "gh_issue_show"
  toolDescription = "Show details of a specific GitHub issue by number."
