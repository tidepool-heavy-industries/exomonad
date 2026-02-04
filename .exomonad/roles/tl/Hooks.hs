{-# LANGUAGE OverloadedStrings #-}

module Hooks (tlHooks) where

import ExoMonad

tlHooks :: HookConfig
tlHooks = defaultHooks
  { onStop = tlStopHook
  , onSubagentStop = subagentStopHook
  }

-- Full effect access in hooks (Git, GitHub, Log, etc.)
tlStopHook :: HookInput -> Eff HookEffects StopOutput
tlStopHook input = do
  -- Check for uncommitted changes
  dirty <- gitGetDirtyFiles
  unless (null dirty) $
    block $ "Uncommitted changes: " <> show dirty
  allow

subagentStopHook :: HookInput -> Eff HookEffects StopOutput
subagentStopHook input = do
  dirty <- gitGetDirtyFiles
  unless (null dirty) $
    block "Subagent has uncommitted changes"
  allow
