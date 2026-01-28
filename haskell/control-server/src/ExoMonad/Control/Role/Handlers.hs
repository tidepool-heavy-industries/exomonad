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

-- Effect Imports
import ExoMonad.Effect.Types (Log, Time, runReturn)
import ExoMonad.Effects.Git (Git)
import ExoMonad.Effects.GitHub (GitHub)
import ExoMonad.Effects.Worktree (Worktree)
import ExoMonad.Effects.FileSystem (FileSystem)
import ExoMonad.Effects.Env (Env)
import ExoMonad.Effects.Zellij (Zellij)
import ExoMonad.Effects.DockerSpawner (DockerSpawner)
import ExoMonad.Effect.Gemini (GeminiOp)
import ExoMonad.Effect.TUI (TUI)
import ExoMonad.Graph.Goto (unwrapSingleChoice)

-- ════════════════════════════════════════════════════════════════════════════
-- ROLE HANDLERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Team Lead role handlers.
tlHandlers :: 
  ( Members '[Log, Git, GitHub, Worktree, FileSystem, Env, Zellij, DockerSpawner, GeminiOp, TUI] es
  ) => TLRole (AsHandler es) es
tlHandlers = TLRole
  { tlMetadataField = tlMetadata
  , tlHooks = emptyHooks
  , tlOrchestration = orchestrationHandlers
  , tlTUI = tuiHandlers
  , tlGitHub = gitHubHandlers
  }

-- | Developer role handlers.
devHandlers :: 
  ( Members '[Log, Git, GitHub, Env, TUI] es
  ) => DevRole (AsHandler es) es
devHandlers = DevRole
  { devMetadataField = devMetadata
  , devHooks = emptyHooks
  , devWorkflow = workflowHandlers
  , devTUI = tuiHandlers
  , devGitHub = gitHubHandlers
  }

-- | Project Manager role handlers.
pmHandlers :: 
  ( Members '[Log, GitHub, Env, Time, TUI] es
  ) => PMRole (AsHandler es) es
pmHandlers = PMRole
  { pmMetadataField = pmMetadata
  , pmHooks = emptyHooks
  , pmPlanning = planningHandlers
  , pmTUI = tuiHandlers
  , pmGitHub = gitHubHandlers
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
  , osSpawnAgents = MkToolHandler $ fmap unwrapSingleChoice . spawnAgentsLogic
  , osExoStatus   = MkToolHandler $ fmap unwrapSingleChoice . exoStatusLogic
  }

-- | TUI server handlers.
tuiHandlers :: 
  ( Members '[Log, TUI] es
  ) => TUIServer (AsHandler es) es
tuiHandlers = TUIServer
  { tuiDescription = "Interactive UI tools"
  , tuiPopup = MkToolHandler $ runReturn . popupLogic
  }

-- | Workflow server handlers.
workflowHandlers :: 
  ( Members '[Log, Git, GitHub] es
  ) => WorkflowServer (AsHandler es) es
workflowHandlers = WorkflowServer
  { wfDescription = "Developer workflow tools"
  , wfFilePR = MkToolHandler $ fmap unwrapSingleChoice . filePRLogic
  }

-- | Planning server handlers.
planningHandlers :: 
  ( Members '[Log, GitHub, Time] es
  ) => PlanningServer (AsHandler es) es
planningHandlers = PlanningServer
  { plDescription = "Project management tools"
  , plPMStatus = MkToolHandler $ fmap unwrapSingleChoice . pmStatusLogic
  }

-- | GitHub server handlers.
gitHubHandlers :: 
  ( Members '[Log, GitHub, Env] es
  ) => GitHubServer (AsHandler es) es
gitHubHandlers = GitHubServer
  { ghDescription = "GitHub issue management"
  , ghIssueList = MkToolHandler $ fmap unwrapSingleChoice . ghIssueListLogic
  , ghIssueShow = MkToolHandler $ fmap unwrapSingleChoice . ghIssueShowLogic
  }