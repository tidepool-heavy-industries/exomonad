{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Role Handlers - Runtime implementation of Role DSL servers.
--
-- This module wires up the abstract Role DSL definitions to concrete
-- logic functions from the ExoTools modules.
module ExoMonad.Control.Role.Handlers
  ( -- * Role Handlers
    tlHandlers
  , devHandlers
  , pmHandlers

    -- * Server Handlers
  , orchestrationHandlers
  , tuiHandlers
  , workflowHandlers
  , planningHandlers
  , gitHubHandlers
  ) where

import Control.Monad.Freer (Members)

import ExoMonad.Control.Role.Types (AsHandler, ToolHandler(..), emptyHooks)

-- Role Definitions
import ExoMonad.Control.Role.Definition.TL (TLRole(..), tlMetadata)
import ExoMonad.Control.Role.Definition.Dev (DevRole(..), devMetadata)
import ExoMonad.Control.Role.Definition.PM (PMRole(..), pmMetadata)

-- Server Definitions
import ExoMonad.Control.Role.Server.Orchestration (OrchestrationServer(..))
import ExoMonad.Control.Role.Server.TUI (TUIServer(..))
import ExoMonad.Control.Role.Server.Workflow (WorkflowServer(..))
import ExoMonad.Control.Role.Server.Planning (PlanningServer(..))
import ExoMonad.Control.Role.Server.GitHub (GitHubServer(..))
import ExoMonad.Control.Role.Server.TL (TLServer(..))
import ExoMonad.Control.Role.Server.Kaizen (KaizenServer(..))

-- Tool Logic Imports
import ExoMonad.Control.ExoTools
  ( exoStatusLogic
  , spawnAgentsLogic
  , filePRLogic
  )
import ExoMonad.Control.PMStatus (pmStatusLogic)
import ExoMonad.Control.TUITools (popupLogic)
import ExoMonad.Control.GHTools
  ( ghIssueListLogic
  , ghIssueShowLogic
  )
import ExoMonad.Control.TLTools (tlCreateIssueLogic)
import ExoMonad.Control.KaizenTools (kaizenReportLogic)

-- Effect Imports
import ExoMonad.Effect.Types (Log, Time)
import ExoMonad.Effects.Git (Git)
import ExoMonad.Effects.GitHub (GitHub)
import ExoMonad.Effects.Worktree (Worktree)
import ExoMonad.Effects.FileSystem (FileSystem)
import ExoMonad.Effects.Env (Env)
import ExoMonad.Effects.Zellij (Zellij)
import ExoMonad.Effects.DockerSpawner (DockerSpawner)
import ExoMonad.Effect.Gemini (GeminiOp)
import ExoMonad.Effect.TUI (TUI)

-- ════════════════════════════════════════════════════════════════════════════
-- ROLE HANDLERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Team Lead role handlers.
tlHandlers :: 
  ( Members '[Log, Git, GitHub, Worktree, FileSystem, Env, Zellij, DockerSpawner, GeminiOp, TUI, Time] es
  ) => TLRole (AsHandler es) es
tlHandlers = TLRole
  { tlMetadataField = tlMetadata
  , tlHooks = emptyHooks
  , tlOrchestration = orchestrationHandlers
  , tlTUI = tuiHandlers
  , tlGitHub = gitHubHandlers
  , tlTools = tlServerHandlers
  , tlKaizen = kaizenHandlers
  }

-- | Developer role handlers.
devHandlers :: 
  ( Members '[Log, Git, GitHub, Env, TUI, FileSystem, Time] es
  ) => DevRole (AsHandler es) es
devHandlers = DevRole
  { devMetadataField = devMetadata
  , devHooks = emptyHooks
  , devWorkflow = workflowHandlers
  , devTUI = tuiHandlers
  , devGitHub = gitHubHandlers
  , devKaizen = kaizenHandlers
  }

-- | Project Manager role handlers.
pmHandlers :: 
  ( Members '[Log, GitHub, Env, Time, TUI, FileSystem] es
  ) => PMRole (AsHandler es) es
pmHandlers = PMRole
  { pmMetadataField = pmMetadata
  , pmHooks = emptyHooks
  , pmPlanning = planningHandlers
  , pmTUI = tuiHandlers
  , pmGitHub = gitHubHandlers
  , pmKaizen = kaizenHandlers
  }

-- ════════════════════════════════════════════════════════════════════════════
-- SERVER HANDLERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Orchestration server handlers.
orchestrationHandlers :: 
  ( Members '[Log, Git, GitHub, Worktree, FileSystem, Env, Zellij, DockerSpawner, GeminiOp] es
  ) => OrchestrationServer (AsHandler es) es
orchestrationHandlers = OrchestrationServer
  { osDescription = "Agent orchestration tools"
  , osSpawnAgents = MkToolHandler spawnAgentsLogic
  , osExoStatus   = MkToolHandler exoStatusLogic
  }

-- | TUI server handlers.
tuiHandlers :: 
  ( Members '[Log, TUI] es
  ) => TUIServer (AsHandler es) es
tuiHandlers = TUIServer
  { tuiDescription = "Interactive UI tools"
  , tuiPopup = MkToolHandler popupLogic
  }

-- | Workflow server handlers.
workflowHandlers :: 
  ( Members '[Log, Git, GitHub] es
  ) => WorkflowServer (AsHandler es) es
workflowHandlers = WorkflowServer
  { wfDescription = "Developer workflow tools"
  , wfFilePR = MkToolHandler filePRLogic
  }

-- | Planning server handlers.
planningHandlers :: 
  ( Members '[Log, GitHub, Time] es
  ) => PlanningServer (AsHandler es) es
planningHandlers = PlanningServer
  { plDescription = "Project management tools"
  , plPMStatus = MkToolHandler pmStatusLogic
  }

-- | GitHub server handlers.
gitHubHandlers :: 
  ( Members '[Log, GitHub, Env] es
  ) => GitHubServer (AsHandler es) es
gitHubHandlers = GitHubServer
  { ghDescription = "GitHub issue management"
  , ghIssueList = MkToolHandler ghIssueListLogic
  , ghIssueShow = MkToolHandler ghIssueShowLogic
  }

-- | TL server handlers.
tlServerHandlers :: 
  ( Members '[Log, GitHub, Env] es
  ) => TLServer (AsHandler es) es
tlServerHandlers = TLServer
  { tlCreateIssue = MkToolHandler tlCreateIssueLogic
  }

-- | Kaizen server handlers.
kaizenHandlers ::
  ( Members '[GitHub, Log] es
  ) => KaizenServer (AsHandler es) es
kaizenHandlers = KaizenServer
  { kzDescription = "Continuous improvement and feedback tools"
  , kzReport = MkToolHandler kaizenReportLogic
  }
