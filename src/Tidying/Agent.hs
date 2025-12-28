{-# LANGUAGE OverloadedStrings #-}
-- | Tidying Agent
--
-- Wraps the Tidying OODA loop as a Tidepool Agent.
--
module Tidying.Agent
  ( tidying
  , TidyingM
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Tidepool
import Tidying.State (SessionState, newSession, UserInput(..))
import Tidying.Loop (tidyingTurn, TidyingEvent(..), Response(..))

-- | The Tidying agent monad (no extra effects)
type TidyingM = AgentM SessionState TidyingEvent '[]

-- | The Tidying agent
--
-- A prosthetic executive function for tackling overwhelming spaces.
-- Uses OODA (Observe-Orient-Decide-Act) pattern with pure routing.
--
-- Usage:
--
-- @
-- main = do
--   finalState <- tidepool config tidying
--   putStrLn $ "Processed " <> show finalState.itemsProcessed <> " items"
-- @
tidying :: SimpleAgent SessionState TidyingEvent
tidying = Agent
  { agentName       = "tidying"
  , agentInit       = newSession
  , agentTurn       = tidyingTurnWrapper
  , agentStartup    = tidyingStartup
  , agentShutdown   = tidyingShutdown
  , agentDispatcher = noDispatcher  -- Pure OODA, no mid-turn tools
  }

-- | Wrap tidyingTurn to match Agent turn signature
tidyingTurnWrapper :: Text -> TidyingM ()
tidyingTurnWrapper inputText = do
  let input = UserInput
        { inputText = Just inputText
        , inputPhotos = []  -- Photo support TBD
        }
  response <- tidyingTurn input
  -- Emit phase change event
  emit $ SituationClassified $ "Phase: " <> T.pack (show response.responsePhase)

-- | Startup: welcome message
tidyingStartup :: TidyingM ()
tidyingStartup = do
  emit $ SituationClassified "Let's tidy! Send me a photo of your space."

-- | Shutdown: session summary
tidyingShutdown :: TidyingM ()
tidyingShutdown = do
  emit $ SituationClassified "Great work! Session complete."
