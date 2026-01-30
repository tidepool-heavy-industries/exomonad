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
    tlHandlers,
    devHandlers,
    pmHandlers,
  )
where

import Control.Monad.Freer (LastMember, Member)
import Control.Monad.Freer.Reader (Reader)
-- Role Definitions

-- Tool Wiring

-- Effect Imports (Constraints)
import ExoMonad.Control.Effects.Effector (Effector)
import ExoMonad.Control.Role.Definition.Dev (DevRole (..), devMetadata)
import ExoMonad.Control.Role.Definition.PM (PMRole (..), pmMetadata)
import ExoMonad.Control.Role.Definition.TL (TLRole (..), tlMetadata)
import ExoMonad.Control.Role.Hook.Definitions (DevHooks (..), PMHooks (..), TLHooks (..))
import ExoMonad.Control.Role.Hook.Wiring (commonHooks)
import ExoMonad.Control.Role.Tool.Wiring (devTools, pmTools, tlTools)
import ExoMonad.Control.Types (ServerConfig)
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
import ExoMonad.Graph.Generic (AsHandler)
import OpenTelemetry.Trace (Tracer)

-- ════════════════════════════════════════════════════════════════════════════
-- ROLE HANDLERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Team Lead role handlers.
tlHandlers ::
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
    Member GitHub es,
    Member Effector es,
    Member (Reader ServerConfig) es,
    Member (Reader Tracer) es,
    LastMember IO es
  ) =>
  TLRole (AsHandler es)
tlHandlers =
  TLRole
    { tlToolsRecord = tlTools,
      tlMetadata = tlMetadata,
      tlHooks = TLHooks {common = commonHooks}
    }

-- | Developer role handlers.
devHandlers ::
  ( Member Env es,
    Member Git es,
    Member Log es,
    Member TUI es,
    Member GitHub es,
    Member Effector es,
    Member (Reader ServerConfig) es,
    Member (Reader Tracer) es,
    Member Zellij es,
    LastMember IO es
  ) =>
  DevRole (AsHandler es)
devHandlers =
  DevRole
    { devToolsRecord = devTools,
      devMetadata = devMetadata,
      devHooks = DevHooks {common = commonHooks}
    }

-- | Project Manager role handlers.
pmHandlers ::
  ( Member Env es,
    Member Log es,
    Member TUI es,
    Member GitHub es,
    Member Time es,
    Member Effector es,
    Member (Reader ServerConfig) es,
    Member (Reader Tracer) es,
    Member Git es,
    Member Zellij es,
    LastMember IO es
  ) =>
  PMRole (AsHandler es)
pmHandlers =
  PMRole
    { pmToolsRecord = pmTools,
      pmMetadata = pmMetadata,
      pmHooks = PMHooks {common = commonHooks}
    }
