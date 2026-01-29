{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

-- | Role Tool Definitions.
--
-- This module defines the tool sets for each role using the 'mode :- record' pattern.
--
-- = Structure
--
-- * Shared Tool Sets (Orchestration, TUI, GitHub, etc.)
-- * Role-Specific Tool Sets
-- * Top-Level Role Records (TL, Dev, PM)
module ExoMonad.Control.Role.Tool.Definitions
  ( -- * Shared Tool Sets
    OrchestrationTools(..)
  , TUITools(..)
  , GitHubTools(..)
  , KaizenTools(..)
  
    -- * Role-Specific Tool Sets
  , TLSpecificTools(..)
  , DevSpecificTools(..)
  , PMSpecificTools(..)

    -- * Role Tool Records
  , TLTools(..)
  , DevTools(..)
  , PMTools(..)
  ) where

import GHC.Generics (Generic)
import ExoMonad.Graph.Generic ((:-))
import ExoMonad.Graph.Types (Tool, Description, type (:@))

-- Import argument/result types
import ExoMonad.Control.ExoTools.SpawnAgents.Types (SpawnAgentsArgs, SpawnAgentsResult)
import ExoMonad.Control.ExoTools.Status.Types (ExoStatusArgs)
import ExoMonad.Control.ExoTools.Internal (ExoStatusResult)
import ExoMonad.Control.TUITools.Types (PopupArgs, PopupResult)
import ExoMonad.Control.GHTools.Types (GHIssueListArgs, GHIssueListResult, GHIssueShowArgs, GHIssueShowResult)
import ExoMonad.Control.KaizenTools.Types (KaizenReportArgs, KaizenReportResult)
import ExoMonad.Control.TLTools.Types (TLCreateIssueArgs, TLCreateIssueResult)
import ExoMonad.Control.ExoTools.FilePR.Types (FilePRArgs, FilePRResult)
import ExoMonad.Control.PMStatus.Types (PmStatusArgs, PmStatusResult)

-- ════════════════════════════════════════════════════════════════════════════
-- SHARED TOOL SETS
-- ════════════════════════════════════════════════════════════════════════════

data OrchestrationTools mode = OrchestrationTools
  { spawnAgents :: mode :- Tool SpawnAgentsArgs SpawnAgentsResult
      :@ Description "Spawn parallel worker agents in isolated worktrees. Creates git worktrees for issues and launches Claude sessions in Zellij tabs."
  , exoStatus   :: mode :- Tool ExoStatusArgs ExoStatusResult
      :@ Description "Get current development context including issue details, worktree info, dirty files, and associated PR."
  } deriving Generic

data TUITools mode = TUITools
  { popup :: mode :- Tool PopupArgs PopupResult
      :@ Description "Display an interactive popup dialog with various UI elements (sliders, checkboxes, text inputs, choices). Returns user selections."
  } deriving Generic

data GitHubTools mode = GitHubTools
  { ghIssueList :: mode :- Tool GHIssueListArgs GHIssueListResult
      :@ Description "List GitHub issues. Filter by status (open/closed) and labels."
  , ghIssueShow :: mode :- Tool GHIssueShowArgs GHIssueShowResult
      :@ Description "Show details of a specific GitHub issue by number."
  } deriving Generic

data KaizenTools mode = KaizenTools
  { kaizenReport :: mode :- Tool KaizenReportArgs KaizenReportResult
      :@ Description "File a UX friction report, bug, or improvement idea for the framework."
  } deriving Generic

-- ════════════════════════════════════════════════════════════════════════════
-- ROLE-SPECIFIC TOOL SETS
-- ════════════════════════════════════════════════════════════════════════════

data TLSpecificTools mode = TLSpecificTools
  { tlCreateIssue :: mode :- Tool TLCreateIssueArgs TLCreateIssueResult
      :@ Description "Create a new issue with domain-specific structured fields (Category, Priority, etc.). Enforces best practices for issue creation."
  } deriving Generic

data DevSpecificTools mode = DevSpecificTools
  { filePR :: mode :- Tool FilePRArgs FilePRResult
      :@ Description "File a GitHub pull request for the current branch. Issue number and title are inferred from branch name."
  } deriving Generic

data PMSpecificTools mode = PMSpecificTools
  { pmStatus :: mode :- Tool PmStatusArgs PmStatusResult
      :@ Description "Get sprint health dashboard: velocity, cycle time, PR lag, and current state distribution."
  } deriving Generic

-- ════════════════════════════════════════════════════════════════════════════
-- ROLE TOOL RECORDS
-- ════════════════════════════════════════════════════════════════════════════

data TLTools mode = TLTools
  { orchestration :: OrchestrationTools mode
  , tui           :: TUITools mode
  , github        :: GitHubTools mode
  , kaizen        :: KaizenTools mode
  , specific      :: TLSpecificTools mode
  } deriving Generic

data DevTools mode = DevTools
  { orchestration :: OrchestrationTools mode
  , tui           :: TUITools mode
  , github        :: GitHubTools mode
  , kaizen        :: KaizenTools mode
  , specific      :: DevSpecificTools mode
  } deriving Generic

data PMTools mode = PMTools
  { orchestration :: OrchestrationTools mode
  , tui           :: TUITools mode
  , github        :: GitHubTools mode
  , kaizen        :: KaizenTools mode
  , specific      :: PMSpecificTools mode
  } deriving Generic
