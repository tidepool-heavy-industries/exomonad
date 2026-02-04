{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module ExoMonad.Types
  ( RoleConfig (..),
    HookConfig (..),
    HookEffects,
    defaultHooks,
    -- * Re-exports for hook authoring
    HookInput (..),
    Sem,
    allowStopResponse,
    blockStopResponse,
    StopHookOutput (..),
  )
where

import Data.Text (Text)
import ExoMonad.Guest.Types (HookInput (..), HookOutput, StopHookOutput (..), allowResponse, allowStopResponse, blockStopResponse)
import GHC.Generics (Generic)
import Polysemy (Embed, Sem, Members)

-- | Effects available to hooks.
-- Currently allows arbitrary IO via Embed IO (required for Host Calls).
-- Future versions may restrict this to specific effects (Git, GitHub, Log).
type HookEffects = '[Embed IO]

-- | Role configuration.
-- Defines the role name, available tools, and lifecycle hooks.
data RoleConfig tools = RoleConfig
  { roleName :: Text,
    tools :: tools,
    hooks :: HookConfig
  }
  deriving (Generic)

-- | Configuration for lifecycle hooks.
data HookConfig = HookConfig
  { -- | Called before any tool use. Can allow, block, or modify the tool call.
    preToolUse :: HookInput -> Sem HookEffects HookOutput,
    -- | Called when the agent stops (e.g. /stop or session end).
    onStop :: HookInput -> Sem HookEffects StopHookOutput,
    -- | Called when a sub-agent stops.
    onSubagentStop :: HookInput -> Sem HookEffects StopHookOutput
  }

-- | Default hooks that allow everything.
defaultHooks :: HookConfig
defaultHooks =
  HookConfig
    { preToolUse = \_ -> pure (allowResponse Nothing),
      onStop = \_ -> pure allowStopResponse,
      onSubagentStop = \_ -> pure allowStopResponse
    }