{-# LANGUAGE OverloadedStrings #-}

-- | Stop hook logic shared across roles.
--
-- Re-exports runStopHookChecks from the wasm-guest library.
-- Used by both TLRole and DevRole (via HttpDevHooks).
module StopHook
  ( devHooks,
    runStopHookChecks,
  )
where

import ExoMonad.Guest.Effects.StopHook (runStopHookChecks)
import ExoMonad.Guest.Types (allowResponse, postToolUseResponse)
import ExoMonad.Types (HookConfig (..), defaultSessionStartHook)
import Control.Monad.Freer (send)

-- ============================================================================
-- Hook Configuration
-- ============================================================================

-- | Hook config for dev agents with full stop validation.
devHooks :: HookConfig
devHooks =
  HookConfig
    { preToolUse = \_ -> pure (allowResponse Nothing),
      postToolUse = \_ -> pure (postToolUseResponse Nothing),
      onStop = \_ -> send runStopHookChecks,
      onSubagentStop = \_ -> send runStopHookChecks,
      onSessionStart = defaultSessionStartHook
    }
