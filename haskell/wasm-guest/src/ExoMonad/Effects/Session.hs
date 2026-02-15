{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | Session effects for registering Claude Code session IDs.
module ExoMonad.Effects.Session
  ( SessionRegisterClaudeId,
    registerClaudeSession,
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
