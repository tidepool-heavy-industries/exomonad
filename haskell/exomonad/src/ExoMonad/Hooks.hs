module ExoMonad.Hooks
  ( HookConfig (..),
    HookEffects,
    defaultHooks,
    HookInput (..),
    HookOutput (..),
    HookSpecificOutput (..),
    HookEventType (..),
    PreToolUseOutput (..),
    StopHookOutput (..),
    StopDecision (..),
    allowResponse,
    allowStopResponse,
    blockStopResponse,
  )
where

import ExoMonad.Types
