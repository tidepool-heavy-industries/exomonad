{-# LANGUAGE OverloadedStrings #-}
-- | Tidying Agent
--
-- The Tidying agent owns its full lifecycle via 'tidyingRun'.
-- Uses OODA (Observe-Orient-Decide-Act) pattern with pure routing.
--
-- = Photo Support
--
-- Photos are provided via GUI (threepenny-gui with camera access).
-- The GUI sends base64-encoded photos directly - no filesystem involved.
-- CLI mode is text-only; use GUI for photo analysis.
--
module Tidying.Agent
  ( -- * Agent
    tidying
  , TidyingM
  , tidyingRun

    -- * Running
  , runTidyingSession
  ) where

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
--
-- CLI mode: text-only input. For photo support, use GUI.
tidyingRun :: TidyingM ()
tidyingRun = do
  -- Startup
  emit $ SituationClassified "Let's tidy! Tell me about your space."
  emit $ SituationClassified "(For photo analysis, use the GUI version)"

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
                , inputPhotos = []  -- Photos come via GUI, not CLI
                }
          response <- tidyingTurn userInput
          emit $ SituationClassified $ "Phase: " <> T.pack (show response.responsePhase)
          loop

-- ══════════════════════════════════════════════════════════════
-- RUNNING THE SESSION
-- ══════════════════════════════════════════════════════════════

-- | Run a tidying session with terminal I/O
--
-- Uses the tidepool runner with the tidying agent.
--
-- @
-- main = do
--   apiKey <- getEnv "ANTHROPIC_API_KEY"
--   let llmConfig = LLMConfig (T.pack apiKey) "claude-sonnet-4-20250514" 4096 Nothing
--   finalState <- runTidyingSession newSession print llmConfig
--   putStrLn $ "Processed " <> show finalState.itemsProcessed <> " items"
-- @
runTidyingSession
  :: SessionState
  -> (TidyingEvent -> IO ())
  -> LLMConfig
  -> IO SessionState
runTidyingSession initialState handleEvent llmConfig = do
  let config = AgentConfig
        { acOnEvent = handleEvent
        , acOnSave = \_ -> pure ()  -- No persistence for CLI
        , acGetInput = pure ""      -- Not used (RequestInput handles it)
        , acLLM = llmConfig
        , acLogLevel = Info
        , acQuitCommands = ["quit", "exit", "done"]
        }
      agent = tidying { agentInit = initialState }
  tidepool config agent
