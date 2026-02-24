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
    teamRegistrationPostToolUse,
  )
where

import Control.Monad (void)
import Control.Monad.Freer (Eff)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import ExoMonad.Effects.Log (LogError, LogInfo)
import ExoMonad.Effects.Log qualified as Log
import ExoMonad.Effects.Session qualified as Session
import ExoMonad.Guest.Tool.Suspend.Types (SuspendYield)
import ExoMonad.Guest.Tool.SuspendEffect (suspendEffect, suspendEffect_)
import ExoMonad.Guest.Types (HookInput (..), HookOutput (..), HookSpecificOutput (..), StopHookOutput, allowResponse, allowStopResponse, postToolUseResponse)
import GHC.Generics (Generic)

-- | Effects available to hooks.
-- Currently allows arbitrary IO via IO (required for Host Calls).
-- Future versions may restrict this to specific effects (Git, GitHub, Log).
type HookEffects = '[SuspendYield, IO]

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

-- | Default SessionStart hook: registers Claude session UUID,
-- then instructs Claude to create a team via TeamCreate.
-- Team name registration happens in PostToolUse (teamRegistrationPostToolUse)
-- after TeamCreate returns the actual auto-generated name.
defaultSessionStartHook :: HookInput -> Eff HookEffects HookOutput
defaultSessionStartHook hookInput = do
  let claudeUuid = hiSessionId hookInput
  void $ suspendEffect_ @Session.SessionRegisterClaudeId
    (Session.RegisterClaudeSessionRequest { Session.registerClaudeSessionRequestClaudeSessionId = TL.fromStrict claudeUuid })
  let instruction = "Create a team using TeamCreate before proceeding."
  pure $
    HookOutput
      { continue_ = True,
        stopReason = Nothing,
        suppressOutput = Nothing,
        systemMessage = Nothing,
        hookSpecificOutput = Just $ SessionStartOutput { ssAdditionalContext = Just instruction }
      }

-- | PostToolUse hook that registers the team after TeamCreate completes.
-- Extracts the auto-generated team name from TeamCreate's tool_response
-- and registers it with the server so notify_parent can route via Teams inbox.
teamRegistrationPostToolUse :: HookInput -> Eff HookEffects HookOutput
teamRegistrationPostToolUse hookInput =
  case hiToolName hookInput of
    Just "TeamCreate" -> do
      case extractTeamName (hiToolResponse hookInput) of
        Just teamName -> do
          let inboxName = "team-lead"
          teamResult <- suspendEffect @Session.SessionRegisterTeam
            (Session.RegisterTeamRequest
              { Session.registerTeamRequestTeamName = TL.fromStrict teamName,
                Session.registerTeamRequestInboxName = TL.fromStrict inboxName
              })
          case teamResult of
            Left _err ->
              void $ suspendEffect_ @Log.LogError
                (Log.ErrorRequest
                  { Log.errorRequestMessage = TL.fromStrict $ "[PostToolUse] registerTeam failed for team: " <> teamName,
                    Log.errorRequestFields = ""
                  })
            Right _ ->
              void $ suspendEffect_ @Log.LogInfo
                (Log.InfoRequest
                  { Log.infoRequestMessage = TL.fromStrict $ "[PostToolUse] Registered team: " <> teamName,
                    Log.infoRequestFields = ""
                  })
        Nothing ->
          void $ suspendEffect_ @Log.LogError
            (Log.ErrorRequest
              { Log.errorRequestMessage = "[PostToolUse] TeamCreate response missing team_name field",
                Log.errorRequestFields = ""
              })
      pure (postToolUseResponse Nothing)
    _ -> pure (postToolUseResponse Nothing)

-- | Extract team_name from TeamCreate's tool_response JSON.
-- TeamCreate returns: {"team_name": "...", "team_file_path": "...", "lead_agent_id": "..."}
extractTeamName :: Maybe Aeson.Value -> Maybe Text
extractTeamName (Just (Aeson.Object obj)) =
  case KM.lookup (Key.fromText "team_name") obj of
    Just (Aeson.String name) -> Just name
    _ -> Nothing
extractTeamName _ = Nothing
