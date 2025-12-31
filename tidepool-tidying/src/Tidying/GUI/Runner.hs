{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- | Runner for the Tidying agent with GUI integration
--
-- Connects the tidying agent loop to the threepenny-gui via GUIBridge.
-- Runs the agent in a background thread, routing input through the GUI
-- and streaming events to the narrative/debug panels.
module Tidying.GUI.Runner
  ( tidyingGameLoopWithGUI
  ) where

import Control.Concurrent.MVar (takeMVar)
import Control.Concurrent.STM (atomically, readTVar, writeTVar)
import Control.Exception (SomeException, try)
import System.Timeout (timeout)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (fromJSON, Result(..), toJSON)
import qualified Data.Text as T
import System.Environment (lookupEnv)

import Effectful (Eff, runEff, IOE, (:>), inject)
import Effectful.Dispatch.Dynamic (reinterpret)
import qualified Effectful.State.Static.Local as EState

import Tidying.Agent (tidyingRun)
import Tidying.Loop (TidyingEvent(..))
import Tidying.State (SessionState(..))
import Tidepool.Question (Question(..), Answer(..), ItemDisposition(..))
import Tidying.Tools (makeTidyingDispatcher)
import Tidepool.Effect
  ( State(..)
  , runRandom
  , runTime
  , runEmit
  , runChatHistory
  , runLogWithBridge
  , runRequestInput
  , runLLMWithToolsHooked
  , runQuestionUI
  , QuestionHandler
  , LLMConfig(..)
  , LLMHooks(..)
  , LogLevel(..)
  )
import Tidepool.GUI.Core
  ( GUIBridge(..)
  , PendingRequest(..)
  , RequestResponse(..)
  , addNarrative
  , logDebug
  , logInfo
  , logWarn
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
        , llmModel = "claude-haiku-4-5-20251001"
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

        -- Tool events (from mid-turn tool calls)
        ItemProposed item choices ->
          logInfo bridge $ "TOOL: propose_disposition for '" <> item
            <> "' with choices: " <> T.intercalate ", " choices

        UserConfirmed item disposition ->
          addNarrative bridge $ "Got it! " <> item <> " -> " <> disposition

        UserCorrected item location ->
          addNarrative bridge $ "Thanks for the correction! " <> item <> " -> " <> location

        FunctionChosen fn ->
          logInfo bridge $ "TOOL: Space function set to '" <> fn <> "'"

        SessionConfirmedDone ->
          logInfo bridge "TOOL: User confirmed session done"

        ErrorOccurred err -> do
          -- Show errors visibly in both narrative and debug
          addNarrative bridge $ "⚠️ " <> err
          logWarn bridge $ "ERROR: " <> err

  -- Create the question handler for tools to use
  let questionHandler = makeQuestionHandler bridge

  -- Create tool dispatcher with the question handler
  -- Tools need QuestionHandler to show UI during LLM turns
  let toolDispatcher = makeTidyingDispatcher questionHandler

  -- Run the game loop with error handling
  -- tidyingRun is TidyingM which includes QuestionUI in its effect stack.
  -- We inject into a stack with IOE, then interpret QuestionUI.
  --
  -- The effect order is:
  -- TidyingM = Eff '[QuestionUI, LLM, State, Emit, RequestInput, Log, ChatHistory, Random]
  -- After inject (adding IOE): Eff '[QuestionUI, ...effects..., IOE]
  -- After runQuestionUI: Eff '[LLM, ...effects..., IOE] (BaseEffects + IOE)
  -- Remaining interpreters strip the rest
  --
  -- Note: tidyingTools defines the available tools, but they're only used
  -- when passed to runTurn/runTurnContent calls. The dispatcher handles
  -- executing tools when the LLM invokes them.
  result <- try $ runEff
    . runTime
    . runRandom
    . runEmit handleEvent
    . runStateWithGUISync bridge initialState
    . runChatHistory
    . runLogWithBridge bridge Tidepool.Effect.Debug
    . runRequestInput inputHandler
    . runLLMWithToolsHooked @_ @TidyingEvent spinnerHooks llmConfig toolDispatcher
    . runQuestionUI questionHandler
    . inject
    $ tidyingRun

  case result of
    Left (err :: SomeException) -> do
      addNarrative bridge $ "Something went wrong: " <> T.pack (show err)
      logError bridge $ "CRASH: " <> T.pack (show err)
    Right () ->
      addNarrative bridge "Thanks for tidying with me today!"

-- | Create a question handler that routes through the GUIBridge
--
-- Posts the Question as a PendingCustom request, blocks on MVar,
-- and parses the CustomResponse back to an Answer.
--
-- Has a 5-minute timeout to prevent blocking forever if GUI hangs.
makeQuestionHandler :: GUIBridge state -> QuestionHandler
makeQuestionHandler bridge = \question -> do
  -- Debug: log question being posted
  logInfo bridge $ "QuestionHandler: posting question type: " <> questionType question
  logDebug bridge $ "QuestionHandler: full question JSON: " <> T.pack (show (toJSON question))

  -- Post question to bridge as PendingCustom
  atomically $ writeTVar bridge.gbPendingRequest
    (Just $ PendingCustom "question" (toJSON question))

  logInfo bridge "QuestionHandler: blocking on MVar..."

  -- Block waiting for user response (5 minute timeout)
  -- 5 * 60 * 1000000 = 300000000 microseconds
  mResponse <- timeout 300000000 $ takeMVar bridge.gbRequestResponse

  logInfo bridge $ "QuestionHandler: MVar returned: " <> T.pack (show (isJust mResponse))

  -- Clear the pending request
  atomically $ writeTVar bridge.gbPendingRequest Nothing

  case mResponse of
    Nothing -> do
      -- Timeout occurred
      logError bridge "Question handler timed out (5 min)"
      pure $ fallbackAnswer question

    Just response -> do
      -- Parse response
      logInfo bridge $ "QuestionHandler: got response type: " <> responseType response
      case response of
        CustomResponse val -> do
          logDebug bridge $ "QuestionHandler: parsing CustomResponse: " <> T.pack (show val)
          case fromJSON val of
            Success answer -> do
              logInfo bridge $ "QuestionHandler: parsed answer successfully"
              pure answer
            Error err -> do
              logError bridge $ "Failed to parse answer: " <> T.pack err
              -- Fallback: treat as text answer
              pure $ fallbackAnswer question

        -- Shouldn't happen, but handle gracefully
        TextResponse txt -> do
          logWarn bridge $ "QuestionHandler: got TextResponse instead of CustomResponse: " <> txt
          pure $ fallbackAnswer question

        _ -> do
          logError bridge "Unexpected response type for question"
          pure $ fallbackAnswer question
  where
    isJust Nothing = False
    isJust (Just _) = True

    questionType :: Question -> T.Text
    questionType (ProposeDisposition _ _ _) = "ProposeDisposition"
    questionType (Confirm _ _) = "Confirm"
    questionType (Choose _ _ _) = "Choose"
    questionType (FreeText _ _) = "FreeText"

    responseType :: RequestResponse -> T.Text
    responseType (ChoiceResponse _) = "ChoiceResponse"
    responseType (TextResponse _) = "TextResponse"
    responseType (PhotoResponse _ _) = "PhotoResponse"
    responseType (TextWithPhotoResponse _ _ _) = "TextWithPhotoResponse"
    responseType (CustomResponse _) = "CustomResponse"

-- | Fallback answer when parsing fails
--
-- Returns a sensible default based on question type.
-- This ensures the agent gets the expected Answer variant.
fallbackAnswer :: Question -> Answer
fallbackAnswer = \case
  ProposeDisposition _ _ _ -> DispositionAnswer SkipForNow
  Confirm _ defVal -> ConfirmAnswer defVal
  Choose _ _ _ -> ChoiceAnswer ""
  FreeText _ _ -> TextAnswer ""

-- | Run State effect with live synchronization to GUIBridge
--
-- Unlike a simple runState wrapper, this interpreter syncs to the
-- GUIBridge TVar after EVERY Put operation. This ensures the GUI
-- shows state changes in real-time during long-running agent loops.
runStateWithGUISync
  :: IOE :> es
  => GUIBridge state
  -> state
  -> Eff (State state : es) a
  -> Eff es a
runStateWithGUISync bridge initialState action =
  fmap fst $ reinterpret (EState.runState initialState) (\_ -> \case
    Get -> EState.get
    Put s -> do
      EState.put s
      -- Sync to GUI bridge after every state change
      liftIO $ updateState bridge (const s)) action
