{-# LANGUAGE OverloadedStrings #-}

-- | Stop hook logic for dev agents.
--
-- This module re-exports the library-level stop hook checks.
-- TL role uses defaultHooks (no checks).
-- Dev role uses devHooks (full validation).
module StopHook
  ( devHooks,
    runStopHookChecks,
  )
where

import ExoMonad.Guest.Effects.StopHook (runStopHookChecks)
import ExoMonad.Guest.Types (allowResponse, postToolUseResponse)
import ExoMonad.Types (HookConfig (..))
import Polysemy (embed)

-- ============================================================================
-- Hook Configuration
-- ============================================================================

-- | Hook config for dev agents with full stop validation.
devHooks :: HookConfig
devHooks =
  HookConfig
    { preToolUse = \_ -> pure (allowResponse Nothing),
      postToolUse = \_ -> pure (postToolUseResponse Nothing),
      onStop = \_ -> embed runStopHookChecks,
      onSubagentStop = \_ -> embed runStopHookChecks
    }
