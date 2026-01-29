{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Role Handlers - Runtime implementation of Role DSL servers.
module ExoMonad.Control.Role.Handlers
  ( -- * Role Handlers
    tlHandlers
  , devHandlers
  , pmHandlers
  ) where

import Control.Monad.Freer (Member)

import ExoMonad.Control.Role.Types (emptyHooks)
import ExoMonad.Graph.Generic (AsHandler)

-- Role Definitions
import ExoMonad.Control.Role.Definition.TL (TLRole(..), tlMetadata)
import ExoMonad.Control.Role.Definition.Dev (DevRole(..), devMetadata)
import ExoMonad.Control.Role.Definition.PM (PMRole(..), pmMetadata)

-- Tool Wiring
import ExoMonad.Control.Role.Tool.Wiring (tlTools, devTools, pmTools)

-- Effect Imports (Constraints)
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
-- ROLE HANDLERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Team Lead role handlers.
tlHandlers :: 
  ( Member DockerSpawner es, Member Env es, Member Git es, Member Justfile es
  , Member Worktree es, Member FileSystem es, Member Zellij es
  , Member GeminiOp es, Member Log es
  , Member TUI es
  , Member GitHub es
  ) => TLRole (AsHandler es) es
tlHandlers = TLRole
  { tlToolsRecord = tlTools
  , tlMetadata    = tlMetadata
  , tlHooks       = emptyHooks
  }

-- | Developer role handlers.
devHandlers :: 
  ( Member Env es, Member Git es, Member Log es
  , Member TUI es, Member GitHub es
  ) => DevRole (AsHandler es) es
devHandlers = DevRole
  { devToolsRecord = devTools
  , devMetadata    = devMetadata
  , devHooks       = emptyHooks
  }

-- | Project Manager role handlers.
pmHandlers :: 
  ( Member Env es, Member Log es
  , Member TUI es, Member GitHub es
  , Member Time es
  ) => PMRole (AsHandler es) es
pmHandlers = PMRole
  { pmToolsRecord = pmTools
  , pmMetadata    = pmMetadata
  , pmHooks       = emptyHooks
  }
