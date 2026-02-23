{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module ExoMonad.Types
  ( RoleConfig (..),
    HookConfig (..),
    HookEffects,
    defaultHooks,
    defaultSessionStartHook,
  )
where

import Control.Monad (void)
import Control.Monad.Freer (Eff, sendM)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import ExoMonad.Effects.Log qualified as Log
import ExoMonad.Effects.Session qualified as Session
import ExoMonad.Guest.Types (HookInput (..), HookOutput (..), HookSpecificOutput (..), StopHookOutput, allowResponse, allowStopResponse, postToolUseResponse)
import GHC.Generics (Generic)

-- | Effects available to hooks.
-- Currently allows arbitrary IO via IO (required for Host Calls).
-- Future versions may restrict this to specific effects (Git, GitHub, Log).
type HookEffects = '[IO]

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
    preToolUse :: HookInput -> Eff HookEffects HookOutput,
    -- | Called after any tool use. Can inject additional context into the conversation.
    postToolUse :: HookInput -> Eff HookEffects HookOutput,
    -- | Called when the agent stops (e.g. /stop or session end).
    onStop :: HookInput -> Eff HookEffects StopHookOutput,
    -- | Called when a sub-agent stops.
    onSubagentStop :: HookInput -> Eff HookEffects StopHookOutput,
    -- | Called on session start. Default: registers Claude session ID for fork-session.
    onSessionStart :: HookInput -> Eff HookEffects HookOutput
  }

-- | Default hooks that allow everything.
defaultHooks :: HookConfig
defaultHooks =
  HookConfig
    { preToolUse = \_ -> pure (allowResponse Nothing),
      postToolUse = \_ -> pure (postToolUseResponse Nothing),
      onStop = \_ -> pure allowStopResponse,
      onSubagentStop = \_ -> pure allowStopResponse,
      onSessionStart = defaultSessionStartHook
    }

-- | Default SessionStart hook: registers Claude session UUID and team info,
-- then instructs Claude to create the team via TeamCreate.
defaultSessionStartHook :: HookInput -> Eff HookEffects HookOutput
defaultSessionStartHook hookInput = do
  let claudeUuid = hiSessionId hookInput
  -- Use birth_branch (exomonad_session_id) for team name to match spawn_workers lookup.
  -- Falls back to agent_id, then "root".
  let identity = fromMaybe (fromMaybe "root" (hiAgentId hookInput)) (hiExomonadSessionId hookInput)
  let teamName = "exo-" <> identity
  let inboxName = "team-lead"
  sendM $ void $ Session.registerClaudeSession claudeUuid
  teamResult <- sendM $ Session.registerTeam teamName inboxName
  case teamResult of
    Left _err ->
      sendM $
        void $
          Log.logError
            Log.ErrorRequest
              { Log.errorRequestMessage = TL.fromStrict $ "[SessionStart] registerTeam failed for team: " <> teamName,
                Log.errorRequestFields = ""
              }
    Right _ -> pure ()
  let instruction = "Create a team named \"" <> teamName <> "\" using TeamCreate before proceeding."
  pure $
    HookOutput
      { continue_ = True,
        stopReason = Nothing,
        suppressOutput = Nothing,
        systemMessage = Nothing,
        hookSpecificOutput = Just $ SessionStartOutput { ssAdditionalContext = Just instruction }
      }
