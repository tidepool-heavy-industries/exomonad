{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | Session effects for registering Claude Code session IDs and Teams info.
module ExoMonad.Effects.Session
  ( SessionRegisterClaudeId,
    SessionRegisterTeam,

    -- * Proto types
    module Effects.Session,
  )
where

import Effects.Session
import ExoMonad.Effect.Class (Effect (..))

-- | Register Claude session ID effect.
data SessionRegisterClaudeId

instance Effect SessionRegisterClaudeId where
  type Input SessionRegisterClaudeId = RegisterClaudeSessionRequest
  type Output SessionRegisterClaudeId = RegisterClaudeSessionResponse
  effectId = "session.register_claude_id"

-- | Register Claude Teams info effect.
data SessionRegisterTeam

instance Effect SessionRegisterTeam where
  type Input SessionRegisterTeam = RegisterTeamRequest
  type Output SessionRegisterTeam = RegisterTeamResponse
  effectId = "session.register_team"
