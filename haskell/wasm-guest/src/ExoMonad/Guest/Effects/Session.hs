{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Helper functions for Session effects.
module ExoMonad.Guest.Effects.Session
  ( registerTeam,
  )
where

import Control.Monad.Freer (Eff, Member)
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Effects.Agent qualified as Agent
import Effects.EffectError (EffectError)
import Effects.Session qualified as Session
import ExoMonad.Effects.Session qualified as ES
import ExoMonad.Guest.Tool.Suspend.Types (SuspendYield)
import ExoMonad.Guest.Tool.SuspendEffect (suspendEffect)
import Proto3.Suite.Types (Enumerated (..))

-- | Register a team with agent type and model.
registerTeam ::
  (Member SuspendYield r) =>
  Text ->
  Text ->
  Agent.AgentType ->
  Text ->
  Eff r (Either EffectError Session.RegisterTeamResponse)
registerTeam teamName inboxName agentType model =
  suspendEffect @ES.SessionRegisterTeam
    ( Session.RegisterTeamRequest
        { Session.registerTeamRequestTeamName = TL.fromStrict teamName,
          Session.registerTeamRequestInboxName = TL.fromStrict inboxName,
          Session.registerTeamRequestAgentType = Enumerated (Right agentType),
          Session.registerTeamRequestModel = TL.fromStrict model
        }
    )
