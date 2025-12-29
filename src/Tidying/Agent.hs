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
-- = Tool-Based Flow
--
-- The LLM uses tools to interact with the user mid-turn:
--
-- 1. LLM analyzes photo, sees "blue mug on papers"
-- 2. LLM calls 'propose_disposition' with ranked choices
-- 3. Tool presents choices to user via QuestionUI effect
-- 4. User taps to confirm or types correction
-- 5. Answer returns to LLM, which continues processing
--
-- This requires the 'QuestionUI' effect in the stack. The basic CLI runner
-- (tidepool) doesn't support tools; use the GUI runner for full functionality.
--
module Tidying.Agent
  ( -- * Agent
    tidying
  , tidyingNoTools
  , TidyingM
  , TidyingExtra
  , tidyingRun

    -- * Running
  , runTidyingSession
  ) where

import qualified Data.Text as T
import Effectful (Eff)

import Tidepool
import Tidying.State (SessionState, newSession, UserInput(..))
import Tidying.Loop (tidyingTurn, TidyingEvent(..), Response(..))

-- | Extra effects for full Tidying agent (with tools)
--
-- The QuestionUI effect enables mid-turn tools like 'propose_disposition'
-- that pause for user input.
type TidyingExtra = '[QuestionUI]

-- | The Tidying agent monad (with QuestionUI for tool support)
type TidyingM = AgentM SessionState TidyingEvent TidyingExtra

-- | The Tidying agent with tool support
--
-- A prosthetic executive function for tackling overwhelming spaces.
-- Uses OODA (Observe-Orient-Decide-Act) pattern with tool-based user interaction.
--
-- Tools available:
--
-- * 'propose_disposition' - LLM proposes where item goes, user confirms
-- * 'ask_space_function' - LLM asks what the space is for
-- * 'confirm_done' - confirm session is complete
--
-- Requires 'tidepoolWith' or a GUI runner that provides QuestionUI.
tidying :: Agent SessionState TidyingEvent TidyingExtra
tidying = Agent
  { agentName       = "tidying"
  , agentInit       = newSession
  , agentRun        = tidyingRun
  , agentDispatcher = noDispatcher  -- TODO: Wire makeTidyingDispatcher when runner supports QuestionUI
  }

-- | Simple Tidying agent (no tools, for basic CLI use)
--
-- This version works with the basic 'tidepool' runner but doesn't
-- have access to mid-turn tools. For full functionality, use 'tidying'
-- with a GUI runner.
tidyingNoTools :: SimpleAgent SessionState TidyingEvent
tidyingNoTools = Agent
  { agentName       = "tidying"
  , agentInit       = newSession
  , agentRun        = tidyingRunNoTools
  , agentDispatcher = noDispatcher
  }

-- | Simple run loop without tools (for CLI use with 'tidepool' runner)
tidyingRunNoTools :: Eff (BaseEffects SessionState TidyingEvent) ()
tidyingRunNoTools = do
  -- Startup
  emit $ SituationClassified "Let's tidy! Tell me about your space."
  emit $ SituationClassified "(For photo analysis and full tool support, use the GUI version)"

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
        "" -> loop
        _ -> do
          let userInput = UserInput
                { inputText = Just input
                , inputPhotos = []
                }
          response <- tidyingTurn userInput
          emit $ SituationClassified $ "Phase: " <> T.pack (show response.responsePhase)
          loop

-- | The Tidying agent's full lifecycle (with tool support)
--
-- This run function has access to QuestionUI, enabling mid-turn tools
-- like 'propose_disposition'. The LLM can call tools that pause for
-- user input and then continue processing.
--
-- The tools are wired via 'makeTidyingDispatcher' in the runner.
-- See 'Tidying.Tools' for available tools.
tidyingRun :: TidyingM ()
tidyingRun = do
  -- Startup
  emit $ SituationClassified "Let's tidy! Take a photo of your space."

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
        "" -> loop
        _ -> do
          let userInput = UserInput
                { inputText = Just input
                , inputPhotos = []  -- Photos come via GUI, not text
                }
          response <- tidyingTurn userInput
          emit $ SituationClassified $ "Phase: " <> T.pack (show response.responsePhase)
          loop

-- ══════════════════════════════════════════════════════════════
-- RUNNING THE SESSION
-- ══════════════════════════════════════════════════════════════

-- | Run a tidying session with terminal I/O (no tools)
--
-- Uses the basic 'tidepool' runner with 'tidyingNoTools'.
-- For full tool support (photo analysis, propose_disposition), use a GUI runner.
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
        , acOnSave = \_ -> pure ()
        , acGetInput = pure ""
        , acLLM = llmConfig
        , acLogLevel = Info
        , acQuitCommands = ["quit", "exit", "done"]
        }
      agent = tidyingNoTools { agentInit = initialState }
  tidepool config agent
