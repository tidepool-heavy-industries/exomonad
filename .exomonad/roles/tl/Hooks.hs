{-# LANGUAGE OverloadedStrings #-}

module Hooks (tlHooks) where

import ExoMonad
import ExoMonad.Guest.Types (blockStopResponse, StopHookOutput)
-- import Control.Monad (unless)

tlHooks :: HookConfig
tlHooks = defaultHooks
  { onStop = tlStopHook
  , onSubagentStop = subagentStopHook
  }

-- Full effect access in hooks (Git, GitHub, Log, etc.)
-- Note: Git dirty checks are not yet available in HookEffects.
-- Stubbing out to always allow for now.
tlStopHook :: HookInput -> Sem HookEffects StopHookOutput
tlStopHook _input = do
  -- hypothetical future check:
  -- dirty <- gitGetDirtyFiles
  -- unless (null dirty) $
  --   return $ blockStopResponse $ "Uncommitted changes: " <> show dirty
  return allowStopResponse

subagentStopHook :: HookInput -> Sem HookEffects StopHookOutput
subagentStopHook _input = do
  -- dirty <- gitGetDirtyFiles
  -- unless (null dirty) $
  --   return $ blockStopResponse "Subagent has uncommitted changes"
  return allowStopResponse