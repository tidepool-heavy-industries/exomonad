{-# LANGUAGE OverloadedStrings #-}
-- | Runner for the Tidying agent with GUI integration
--
-- Connects the tidying agent loop to the threepenny-gui via GUIBridge.
-- Runs the agent in a background thread, routing input through the GUI
-- and streaming events to the narrative/debug panels.
module Tidying.GUI.Runner
  ( tidyingGameLoopWithGUI
  ) where

import Control.Concurrent.STM (atomically, readTVar)
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (toJSON)
import qualified Data.Text as T
import System.Environment (lookupEnv)

import Effectful (Eff, runEff, IOE, (:>), inject)

import Tidying.Agent (tidyingRun)
import Tidying.Loop (TidyingEvent(..))
import Tidying.State (SessionState(..))
import Tidepool (BaseEffects, RunnerEffects)
import Tidepool.Effect
  ( State
  , runState
  , runRandom
  , runEmit
  , runChatHistory
  , runLogWithBridge
  , runRequestInput
  , runLLMWithToolsHooked
  , LLMConfig(..)
  , LLMHooks(..)
  , LogLevel(..)
  , ToolDispatcher
  , ToolResult(..)
  )
import Tidepool.GUI.Core
  ( GUIBridge(..)
  , addNarrative
  , logInfo
  , logError
  , updateState
  , setLLMActive
  )
import Tidepool.GUI.Handler (makeGUIHandler)

-- | Run the tidying agent with GUI integration
--
-- This function should be spawned in a background thread:
--
-- @
-- _ <- forkIO $ tidyingGameLoopWithGUI bridge
-- @
--
-- The agent will:
-- - Request input via the GUI (text, photos)
-- - Stream events to the narrative log
-- - Update state for the phase indicator
-- - Handle errors gracefully
tidyingGameLoopWithGUI :: GUIBridge SessionState -> IO ()
tidyingGameLoopWithGUI bridge = do
  -- Get API key
  mApiKey <- lookupEnv "ANTHROPIC_API_KEY"
  apiKey <- case mApiKey of
    Just k -> pure $ T.pack k
    Nothing -> do
      addNarrative bridge "Error: ANTHROPIC_API_KEY not set"
      error "ANTHROPIC_API_KEY environment variable not set"

  let llmConfig = LLMConfig
        { llmApiKey = apiKey
        , llmModel = "claude-sonnet-4-20250514"
        , llmMaxTokens = 4096
        , llmThinkingBudget = Nothing
        }

  -- Set up GUI input handler
  let inputHandler = makeGUIHandler bridge

  -- Get initial state from bridge
  initialState <- atomically $ readTVar bridge.gbState

  -- Add welcome message
  addNarrative bridge "Welcome to your tidying session!"
  addNarrative bridge "I'm here to help you tackle that overwhelming space."
  addNarrative bridge "Tell me about what you'd like to tidy, or send a photo."

  -- Spinner hooks - show/hide loading indicator around LLM calls
  let spinnerHooks = LLMHooks
        { onTurnStart = setLLMActive bridge True
        , onTurnEnd = setLLMActive bridge False
        }

  -- Event handler - routes events to GUI
  let handleEvent :: TidyingEvent -> IO ()
      handleEvent = \case
        UserInputReceived input ->
          -- Prefix with "> " so chat parser shows as user message
          addNarrative bridge $ "> " <> input

        ResponseGenerated resp ->
          addNarrative bridge resp

        PhotoAnalyzed info ->
          addNarrative bridge $ "Analyzing your photo... " <> info

        SituationClassified situation ->
          logInfo bridge $ "ORIENT: " <> situation

        ActionTaken action ->
          logInfo bridge $ "DECIDE: " <> T.pack (show action)

        PhaseChanged _old new -> do
          -- State is synced separately, just log phase change
          logInfo bridge $ "PHASE: " <> T.pack (show new)

        SessionEnded itemsProcessed ->
          addNarrative bridge $ "Session complete! You processed "
            <> T.pack (show itemsProcessed) <> " items."

  -- The agent uses BaseEffects (no IOE). We inject into RunnerEffects which includes IOE.
  let theRun :: Eff (BaseEffects SessionState TidyingEvent) ()
      theRun = tidyingRun
      widened :: Eff (RunnerEffects SessionState TidyingEvent) ()
      widened = inject theRun

  -- Run the game loop with error handling
  result <- try $ runEff
    . runRandom
    . runEmit handleEvent
    . runStateWithSync bridge initialState
    . runChatHistory
    . runLogWithBridge bridge Tidepool.Effect.Debug
    . runRequestInput inputHandler
    . runLLMWithToolsHooked @_ @TidyingEvent spinnerHooks llmConfig noToolDispatcher
    $ widened

  case result of
    Left (err :: SomeException) -> do
      addNarrative bridge $ "Something went wrong: " <> T.pack (show err)
      logError bridge $ "CRASH: " <> T.pack (show err)
    Right () ->
      addNarrative bridge "Thanks for tidying with me today!"

-- | No-op tool dispatcher for agents without mid-turn tools
noToolDispatcher :: ToolDispatcher event es
noToolDispatcher _ _ = pure (Right (ToolSuccess (toJSON ())))

-- | Run State effect with synchronization to GUIBridge
--
-- Syncs state to the bridge's TVar after each action completes.
-- This ensures the GUI always shows the latest state.
runStateWithSync
  :: IOE :> es
  => GUIBridge state
  -> state
  -> Eff (State state : es) a
  -> Eff es a
runStateWithSync bridge initialState action = do
  (result, finalState) <- runState initialState action
  -- Sync final state to bridge
  liftIO $ updateState bridge (const finalState)
  pure result
