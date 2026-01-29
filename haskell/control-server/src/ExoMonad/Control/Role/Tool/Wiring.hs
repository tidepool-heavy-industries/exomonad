{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

-- | Wiring for Role Tool Servers.
--
-- This module constructs the 'AsHandler' records for each role, wiring
-- the tool logic to the dispatchable record structure.
module ExoMonad.Control.Role.Tool.Wiring
  ( -- * Role Servers
    tlTools
  , devTools
  , pmTools

    -- * Shared Tool Sets
  , orchestrationTools
  , tuiTools
  , gitHubTools
  , kaizenTools
  ) where

import Control.Monad.Freer (Eff, Member)

import ExoMonad.Graph.Generic (ToolHandler(..), AsHandler)
import ExoMonad.Control.Role.Tool.Definitions

-- Import Logic
import ExoMonad.Control.ExoTools.SpawnAgents (spawnAgentsLogic)
import ExoMonad.Control.ExoTools.Status (exoStatusLogic)
import ExoMonad.Control.TUITools (popupLogic)
import ExoMonad.Control.GHTools (ghIssueListLogic, ghIssueShowLogic)
import ExoMonad.Control.KaizenTools (kaizenReportLogic)
import ExoMonad.Control.TLTools (tlCreateIssueLogic)
import ExoMonad.Control.ExoTools.FilePR (filePRLogic)
import ExoMonad.Control.PMStatus (pmStatusLogic)

-- Import Constraints
import ExoMonad.Effects.Env (Env)
import ExoMonad.Effects.GitHub (GitHub)
import ExoMonad.Effects.DockerSpawner (DockerSpawner)
import ExoMonad.Effects.Git (Git)
import ExoMonad.Effects.Justfile (Justfile)
import ExoMonad.Effect.TUI (TUI)
import ExoMonad.Effect.Types (Time, Log)
import ExoMonad.Effects.Worktree (Worktree)
import ExoMonad.Effects.FileSystem (FileSystem)
import ExoMonad.Effects.Zellij (Zellij)
import ExoMonad.Effect.Gemini (GeminiOp)

-- ════════════════════════════════════════════════════════════════════════════
-- SHARED TOOL SETS
-- ════════════════════════════════════════════════════════════════════════════

orchestrationTools :: ( Member DockerSpawner es, Member Env es, Member Git es, Member Justfile es
                      , Member Worktree es, Member FileSystem es, Member Zellij es
                      , Member GeminiOp es, Member Log es, Member GitHub es
                      ) 
                   => OrchestrationTools (AsHandler es)
orchestrationTools = OrchestrationTools
  { spawnAgents = ToolHandler spawnAgentsLogic
  , exoStatus   = ToolHandler exoStatusLogic
  }

tuiTools :: (Member TUI es) => TUITools (AsHandler es)
tuiTools = TUITools
  { popup = ToolHandler popupLogic
  }

gitHubTools :: (Member GitHub es, Member Env es, Member Log es) => GitHubTools (AsHandler es)
gitHubTools = GitHubTools
  { ghIssueList = ToolHandler ghIssueListLogic
  , ghIssueShow = ToolHandler ghIssueShowLogic
  }

kaizenTools :: (Member GitHub es, Member Env es, Member Log es) => KaizenTools (AsHandler es)
kaizenTools = KaizenTools
  { kaizenReport = ToolHandler kaizenReportLogic
  }

-- ════════════════════════════════════════════════════════════════════════════
-- ROLE TOOL RECORDS
-- ════════════════════════════════════════════════════════════════════════════

tlTools :: ( Member DockerSpawner es, Member Env es, Member Git es, Member Justfile es
           , Member Worktree es, Member FileSystem es, Member Zellij es
           , Member GeminiOp es, Member Log es
           , Member TUI es
           , Member GitHub es
           ) => TLTools (AsHandler es)
tlTools = TLTools
  { orchestration = orchestrationTools
  , tui           = tuiTools
  , github        = gitHubTools
  , kaizen        = kaizenTools
  , specific      = TLSpecificTools 
      { tlCreateIssue = ToolHandler tlCreateIssueLogic 
      }
  }

devTools :: ( Member Env es, Member Log es
            , Member TUI es
            , Member GitHub es
            , Member Git es
            ) => DevTools (AsHandler es)
devTools = DevTools
  { tui           = tuiTools
  , github        = gitHubTools
  , kaizen        = kaizenTools
  , specific      = DevSpecificTools
      { filePR = ToolHandler filePRLogic
      }
  }

pmTools :: ( Member Env es, Member Log es
           , Member TUI es
           , Member GitHub es
           , Member Time es
           ) => PMTools (AsHandler es)
pmTools = PMTools
  { tui           = tuiTools
  , github        = gitHubTools
  , kaizen        = kaizenTools
  , specific      = PMSpecificTools
      { pmStatus = ToolHandler pmStatusLogic
      }
  }