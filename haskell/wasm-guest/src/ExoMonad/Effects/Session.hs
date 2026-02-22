{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | Session effects for registering Claude Code session IDs and Teams info.
module ExoMonad.Effects.Session
  ( SessionRegisterClaudeId,
    registerClaudeSession,
    SessionRegisterTeam,
    registerTeam,
  )
where

import Data.Text (Text)
import Effects.Session qualified as Proto
import ExoMonad.Effect.Class (Effect (..), EffectError, runEffect)
import ExoMonad.Guest.Proto (fromText)

-- | Register Claude session ID effect.
data SessionRegisterClaudeId

instance Effect SessionRegisterClaudeId where
  type Input SessionRegisterClaudeId = Proto.RegisterClaudeSessionRequest
  type Output SessionRegisterClaudeId = Proto.RegisterClaudeSessionResponse
  effectId = "session.register_claude_id"

-- | Register the current Claude Code session UUID for fork-session support.
registerClaudeSession :: Text -> IO (Either EffectError Proto.RegisterClaudeSessionResponse)
registerClaudeSession claudeSessionId =
  runEffect @SessionRegisterClaudeId $
    Proto.RegisterClaudeSessionRequest
      { Proto.registerClaudeSessionRequestClaudeSessionId = fromText claudeSessionId
      }

-- | Register Claude Teams info effect.
data SessionRegisterTeam

instance Effect SessionRegisterTeam where
  type Input SessionRegisterTeam = Proto.RegisterTeamRequest
  type Output SessionRegisterTeam = Proto.RegisterTeamResponse
  effectId = "session.register_team"

-- | Register Claude Teams info for inbox-based message delivery.
registerTeam :: Text -> Text -> IO (Either EffectError Proto.RegisterTeamResponse)
registerTeam teamName inboxName =
  runEffect @SessionRegisterTeam $
    Proto.RegisterTeamRequest
      { Proto.registerTeamRequestTeamName = fromText teamName,
        Proto.registerTeamRequestInboxName = fromText inboxName
      }
