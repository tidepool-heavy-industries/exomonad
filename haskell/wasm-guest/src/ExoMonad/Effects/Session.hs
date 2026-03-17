{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module ExoMonad.Effects.Session
  ( SessionRegisterTeam,
    SessionDeregisterTeam,
    module Effects.Session,
  )
where

import Effects.Session
import ExoMonad.Effect.Class (Effect (..))

data SessionRegisterTeam
instance Effect SessionRegisterTeam where
  type Input SessionRegisterTeam = RegisterTeamRequest
  type Output SessionRegisterTeam = RegisterTeamResponse
  effectId = "session.register_team"

data SessionDeregisterTeam
instance Effect SessionDeregisterTeam where
  type Input SessionDeregisterTeam = DeregisterTeamRequest
  type Output SessionDeregisterTeam = DeregisterTeamResponse
  effectId = "session.deregister_team"