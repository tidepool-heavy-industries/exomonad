{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

-- | Wiring for Role Tool Servers.
--
-- This module constructs the 'AsHandler' records for each role, wiring
-- the tool logic to the dispatchable record structure.
module ExoMonad.Control.Role.Tool.Wiring
  ( -- * Role Servers
    tlTools,
    devTools,
    pmTools,

    -- * Shared Tool Sets
    orchestrationTools,
    tuiTools,
    gitHubTools,
    kaizenTools,
  )
where

import Control.Monad.Freer (Eff, Member)
-- Import Logic

import ExoMonad.Control.ExoTools.FilePR (filePRLogic)
import ExoMonad.Control.ExoTools.SpawnAgents (spawnAgentsLogic)
import ExoMonad.Control.ExoTools.Status (exoStatusLogic)
import ExoMonad.Control.GHTools (ghIssueListLogic, ghIssueShowLogic)
import ExoMonad.Control.KaizenTools (kaizenReportLogic)
import ExoMonad.Control.PMStatus (pmStatusLogic)
import ExoMonad.Control.PMTools
  ( pmEpicCreateLogic,
    pmEpicListLogic,
    pmEpicUpdateLogic,
    pmInterviewLogic,
    pmPitchLogic,
  )
import ExoMonad.Control.Role.Tool.Definitions
import ExoMonad.Control.TLTools (tlCreateIssueLogic)
import ExoMonad.Control.TUITools (popupLogic)
-- Import Constraints

import ExoMonad.Effect.Gemini (GeminiOp)
import ExoMonad.Effect.TUI (TUI)
import ExoMonad.Effect.Types (Log, Time)
import ExoMonad.Effects.DockerSpawner (DockerSpawner)
import ExoMonad.Effects.Env (Env)
import ExoMonad.Effects.FileSystem (FileSystem)
import ExoMonad.Effects.Git (Git)
import ExoMonad.Effects.GitHub (GitHub)
import ExoMonad.Effects.Justfile (Justfile)
import ExoMonad.Effects.Worktree (Worktree)
import ExoMonad.Effects.Zellij (Zellij)
import ExoMonad.Graph.Generic (AsHandler, ToolHandler (..))

-- ════════════════════════════════════════════════════════════════════════════
-- SHARED TOOL SETS
-- ════════════════════════════════════════════════════════════════════════════

orchestrationTools ::
  ( Member DockerSpawner es,
    Member Env es,
    Member Git es,
    Member Justfile es,
    Member Worktree es,
    Member FileSystem es,
    Member Zellij es,
    Member GeminiOp es,
    Member Log es,
    Member GitHub es
  ) =>
  OrchestrationTools (AsHandler es)
orchestrationTools =
  OrchestrationTools
    { spawnAgents = ToolHandler spawnAgentsLogic,
      exoStatus = ToolHandler exoStatusLogic
    }

tuiTools :: (Member TUI es) => TUITools (AsHandler es)
tuiTools =
  TUITools
    { popup = ToolHandler popupLogic
    }

gitHubTools :: (Member GitHub es, Member Env es, Member Log es) => GitHubTools (AsHandler es)
gitHubTools =
  GitHubTools
    { ghIssueList = ToolHandler ghIssueListLogic,
      ghIssueShow = ToolHandler ghIssueShowLogic
    }

kaizenTools :: (Member GitHub es, Member Env es, Member Log es) => KaizenTools (AsHandler es)
kaizenTools =
  KaizenTools
    { kaizenReport = ToolHandler kaizenReportLogic
    }

pmEpicTools :: (Member Log es) => PMEpicTools (AsHandler es)
pmEpicTools =
  PMEpicTools
    { pmEpicCreate = ToolHandler pmEpicCreateLogic,
      pmEpicList = ToolHandler pmEpicListLogic,
      pmEpicUpdate = ToolHandler pmEpicUpdateLogic
    }

pmStrategyTools :: (Member Log es) => PMStrategyTools (AsHandler es)
pmStrategyTools =
  PMStrategyTools
    { pmPitch = ToolHandler pmPitchLogic,
      pmInterview = ToolHandler pmInterviewLogic
    }

-- ════════════════════════════════════════════════════════════════════════════
-- ROLE TOOL RECORDS
-- ════════════════════════════════════════════════════════════════════════════

tlTools ::
  ( Member DockerSpawner es,
    Member Env es,
    Member Git es,
    Member Justfile es,
    Member Worktree es,
    Member FileSystem es,
    Member Zellij es,
    Member GeminiOp es,
    Member Log es,
    Member TUI es,
    Member GitHub es
  ) =>
  TLTools (AsHandler es)
tlTools =
  TLTools
    { orchestration = orchestrationTools,
      tui = tuiTools,
      github = gitHubTools,
      kaizen = kaizenTools,
      specific =
        TLSpecificTools
          { tlCreateIssue = ToolHandler tlCreateIssueLogic
          }
    }

devTools ::
  ( Member Env es,
    Member Log es,
    Member TUI es,
    Member GitHub es,
    Member Git es
  ) =>
  DevTools (AsHandler es)
devTools =
  DevTools
    { tui = tuiTools,
      github = gitHubTools,
      kaizen = kaizenTools,
      git =
        GitTools
          { filePR = ToolHandler filePRLogic
          }
    }

pmTools ::
  ( Member Env es,
    Member Log es,
    Member TUI es,
    Member GitHub es,
    Member Time es
  ) =>
  PMTools (AsHandler es)
pmTools =
  PMTools
    { tui = tuiTools,
      github = gitHubTools,
      kaizen = kaizenTools,
      epic = pmEpicTools,
      strategy = pmStrategyTools,
      specific =
        PMSpecificTools
          { pmStatus = ToolHandler pmStatusLogic
          }
    }
