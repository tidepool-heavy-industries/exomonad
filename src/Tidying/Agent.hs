{-# LANGUAGE OverloadedStrings #-}
-- | Tidying Agent
--
-- The Tidying agent owns its full lifecycle via 'tidyingRun'.
-- Uses OODA (Observe-Orient-Decide-Act) pattern with pure routing.
--
module Tidying.Agent
  ( tidying
  , TidyingM
  , tidyingRun
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
  , agentRun        = tidyingRun
  , agentDispatcher = noDispatcher  -- Pure OODA, no mid-turn tools
  }

-- | The Tidying agent's full lifecycle
tidyingRun :: TidyingM ()
tidyingRun = do
  -- Startup
  emit $ SituationClassified "Let's tidy! Send me a photo of your space."

  -- Main loop
  loop

  -- Shutdown
  emit $ SituationClassified "Great work! Session complete."
  where
    loop = do
      input <- requestText "> "
      case T.toLower (T.strip input) of
        "quit" -> pure ()
        "exit" -> pure ()
        "done" -> pure ()
        "" -> loop  -- Empty input, continue
        _ -> do
          let userInput = UserInput
                { inputText = Just input
                , inputPhotos = []  -- Photo support TBD
                }
          response <- tidyingTurn userInput
          emit $ SituationClassified $ "Phase: " <> T.pack (show response.responsePhase)
          loop
