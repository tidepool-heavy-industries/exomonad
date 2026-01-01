{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE FieldSelectors #-}  -- Override NoFieldSelectors from cabal defaults

-- | Handlers for the Tidying Graph
--
-- This module implements the handlers for each node in the TidyingGraph.
-- Each handler is a function that takes its Needs inputs and returns
-- a GotoChoice indicating the next node to execute.
--
-- = Handler Implementations
--
-- The handlers mirror the OODA loop from Tidying.Loop:
--
-- * 'analyzePhotosHandler' - OBSERVE: Vision LLM analyzes photos
-- * 'extractHandler' - ORIENT: LLM extracts intent/item/choice
-- * 'routeHandler' - DECIDE: Pure routing based on extraction
-- * 'actHandler' - ACT: LLM generates response
module Tidying.Graph.Handlers
  ( -- * Handlers
    analyzePhotosHandler
  , extractHandler
  , routeHandler
  , actHandler

    -- * Handler record
  , tidyingHandlers

    -- * Graph-based turn (drop-in replacement for tidyingTurn)
  , tidyingTurnGraph
  ) where

import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import Data.Text qualified as T
import Effectful (Eff, type (:>))

import Tidepool.Effect
  ( State, LLM, Emit, Log, Time
  , get, modify, emit, logDebug, logInfo, logWarn, getCurrentTime
  , llmCallEither, llmCallWithTools, runTurnContent
  , TurnOutcome(..), TurnParseResult(..), TurnResult(..)
  )
import Tidepool.Anthropic.Http (ContentBlock(..), ImageSource(..))
import Tidepool.Template (Schema(..))
import Tidepool.Graph.Goto (GotoChoice(..), OneOf(..), To, gotoChoice, gotoExit)
import Tidepool.Graph.Generic (AsHandler)
import Tidepool.Graph.Types (Exit)

import Tidying.State
import Tidying.Action (Action(..))
import Tidying.Context
import Tidying.Output
  ( Extract(..), Intent(..), PhotoAnalysisOutput(..)
  , extractSchema, photoAnalysisSchema, actOutputSchema, ActOutput(..)
  )
import Tidying.Decide (decideFromExtract)
import Tidying.Templates (renderActPrompt, renderExtractPrompt)
import Tidying.Tools (tidyingTools)
import Tidying.Events (TidyingEvent(..))
import Tidying.Loop (Response(..))
import Tidying.Types (chaosLevelToText, ItemName(..))
import Tidying.Graph
  ( TidyingGraph(..)
  , PhotoResult(..), ExtractResult(..), ActInput(..)
  )


-- ════════════════════════════════════════════════════════════════════════════
-- TYPE ALIASES
-- ════════════════════════════════════════════════════════════════════════════

-- | Effect stack for tidying handlers
type TidyingEs es =
  ( State SessionState :> es
  , LLM :> es
  , Emit TidyingEvent :> es
  , Log :> es
  , Time :> es
  )


-- ════════════════════════════════════════════════════════════════════════════
-- HANDLERS
-- ════════════════════════════════════════════════════════════════════════════

-- | OBSERVE: Analyze photos via vision LLM
--
-- If photos are present, calls the vision LLM to analyze them.
-- Always transitions to the extract node with the result.
analyzePhotosHandler
  :: TidyingEs es
  => UserInput
  -> Eff es (GotoChoice '[To "tgExtract" PhotoResult])
analyzePhotosHandler input = do
  mAnalysis <- case input.inputPhotos of
    [] -> pure Nothing
    photos -> do
      logDebug $ "Analyzing " <> T.pack (show $ length photos) <> " photos via vision"

      let imageSources = map photoToImageSource photos
          imageBlocks = map ImageBlock imageSources
          userContent = TextBlock visionPrompt : imageBlocks

      result <- runTurnContent @PhotoAnalysisOutput
        visionSystemPrompt
        userContent
        photoAnalysisSchema.schemaJSON
        []

      case result of
        TurnCompleted (TurnParsed tr) -> do
          let analysis = outputToAnalysis tr.trOutput
          emit $ PhotoAnalyzed (analysis.paRoomType <> ", " <> chaosLevelToText analysis.paChaosLevel)
          pure (Just analysis)

        TurnCompleted (TurnParseFailed {tpfError}) -> do
          logWarn $ "Photo analysis parse failed: " <> T.pack tpfError
          emit $ ErrorOccurred $ "Photo analysis failed: " <> T.pack tpfError
          pure Nothing

        TurnBroken reason -> do
          logWarn $ "Photo analysis broken: " <> reason
          emit $ ErrorOccurred $ "Photo analysis error: " <> reason
          pure Nothing

  pure $ gotoChoice @"tgExtract" PhotoResult
    { prInput = input
    , prAnalysis = mAnalysis
    }


-- | ORIENT: Extract structured info from user input
--
-- Calls the LLM to extract intent, item, choice, etc. from user text.
-- Applies the overwhelm signal heuristic as a backup.
extractHandler
  :: TidyingEs es
  => PhotoResult
  -> Eff es (GotoChoice '[To "tgRoute" ExtractResult])
extractHandler pr = do
  logDebug "Calling LLM for extraction"

  let textPart = fromMaybe "" pr.prInput.inputText
      photoContext = case pr.prAnalysis of
        Nothing -> ""
        Just pa -> T.unlines
          [ "[Photo analysis]"
          , "Room type: " <> pa.paRoomType
          , "Clutter level: " <> chaosLevelToText pa.paChaosLevel
          , "Visible items: " <> T.intercalate ", " pa.paVisibleItems
          , maybe "" ("Blocked function: " <>) pa.paBlockedFunction
          , maybe "" ("Suggested first target: " <>) pa.paFirstTarget
          ]
      userMsg = if T.null textPart && T.null photoContext
                then "(empty input)"
                else T.strip $ photoContext <> "\n" <> textPart

  logDebug $ "Extract input: " <> T.take 100 userMsg
  result <- llmCallEither @Extract renderExtractPrompt userMsg extractSchema.schemaJSON

  rawExtract <- case result of
    Left err -> do
      logWarn $ "Extract parse failed: " <> err
      emit $ ErrorOccurred $ "Extract failed: " <> err
      pure Extract
        { exIntent = IntentContinue
        , exItem = Nothing
        , exChoice = Nothing
        , exPlace = Nothing
        , exFunction = Nothing
        , exAnchors = Nothing
        }
    Right ext -> pure ext

  -- Apply overwhelm signal heuristic
  let extract = if isOverwhelmedSignal pr.prInput.inputText
                   && rawExtract.exIntent `notElem` [IntentHelp, IntentStop]
                then rawExtract { exIntent = IntentHelp }
                else rawExtract

  logInfo $ "Extract: " <> T.pack (show extract)
  emit $ SituationClassified (T.pack $ show extract.exIntent)

  pure $ gotoChoice @"tgRoute" ExtractResult
    { erExtract = extract
    , erAnalysis = pr.prAnalysis
    , erInput = pr.prInput
    }


-- | DECIDE: Route based on extraction (pure logic + state mutation)
--
-- This handler:
-- 1. Calls decideFromExtract to get (Action, Phase)
-- 2. Updates SessionState based on extraction
-- 3. Builds TidyingContext from updated state
-- 4. Routes to act node or directly exits
routeHandler
  :: TidyingEs es
  => ExtractResult
  -> Eff es (GotoChoice '[To "tgAct" ActInput, To Exit Response])
routeHandler er = do
  st <- get @SessionState
  let extract = er.erExtract
      mPhotoAnalysis = er.erAnalysis

  logDebug $ "Phase: " <> T.pack (show (phase st))

  -- DECIDE: Pure routing
  let (action, nextPhase) = decideFromExtract st mPhotoAnalysis extract

  logDebug $ "Action: " <> T.pack (show action)
  emit $ ActionTaken action

  -- Emit phase change if different
  when (nextPhase /= phase st) $ do
    logInfo $ "Phase change: " <> T.pack (show (phase st)) <> " -> " <> T.pack (show nextPhase)
    emit $ PhaseChanged (phase st) nextPhase

  -- Update state
  applyStateTransition extract action nextPhase

  -- Build context from UPDATED state
  st' <- get @SessionState

  -- Check for session end (IntentStop exits directly)
  if extract.exIntent == IntentStop
    then do
      emit $ SessionEnded st'.itemsProcessed
      -- Generate summary response directly
      let ctx = buildTidyingContext st' mPhotoAnalysis er.erInput.inputText
      responseText <- generateActResponse ctx Summary
      pure $ gotoExit Response
        { responseText = responseText
        , responsePhase = nextPhase
        , responseItemsProcessed = st'.itemsProcessed
        , responseSessionEnded = True
        }
    else do
      let ctx = buildTidyingContext st' mPhotoAnalysis er.erInput.inputText
      pure $ gotoChoice @"tgAct" ActInput
        { aiContext = ctx
        , aiAction = action
        }


-- | ACT: Generate response via LLM
--
-- Generates a natural language response based on the action.
actHandler
  :: TidyingEs es
  => ActInput
  -> Eff es (GotoChoice '[To Exit Response])
actHandler ai = do
  st <- get @SessionState
  responseText <- generateActResponse ai.aiContext ai.aiAction

  pure $ gotoExit Response
    { responseText = responseText
    , responsePhase = phase st
    , responseItemsProcessed = st.itemsProcessed
    , responseSessionEnded = False
    }


-- ════════════════════════════════════════════════════════════════════════════
-- HANDLER RECORD
-- ════════════════════════════════════════════════════════════════════════════

-- | Complete handler record for the tidying graph
--
-- Note: This requires the effect stack to include all tidying effects.
-- The type signature uses constraint-kinded polymorphism.
tidyingHandlers
  :: TidyingEs es
  => TidyingGraph (AsHandler es)
tidyingHandlers = TidyingGraph
  { tgEntry = Proxy
  , tgAnalyzePhotos = analyzePhotosHandler
  , tgExtract = extractHandler
  , tgRoute = routeHandler
  , tgAct = actHandler
  , tgExit = Proxy
  }


-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH-BASED TURN
-- ════════════════════════════════════════════════════════════════════════════

-- | Run a tidying turn using the graph-based approach
--
-- This is a drop-in replacement for 'tidyingTurn' from Tidying.Loop.
-- It uses the graph executor to dispatch through the OODA nodes.
--
-- The flow is:
-- 1. Entry(UserInput) → tgAnalyzePhotos → tgExtract → tgRoute → tgAct → Exit(Response)
--
-- Note: This implementation manually sequences the handlers rather than
-- using runGraph, because the type-level machinery for FindEntryHandler
-- and DispatchGoto requires complex constraints that are hard to satisfy
-- in a polymorphic context.
--
-- A future version could use runGraph directly once the type inference
-- issues are resolved.
tidyingTurnGraph
  :: TidyingEs es
  => UserInput
  -> Eff es Response
tidyingTurnGraph input = do
  -- Step 1: Analyze photos
  photoChoice <- analyzePhotosHandler input
  let GotoChoice (Here photoResult) = photoChoice

  -- Step 2: Extract intent
  extractChoice <- extractHandler photoResult
  let GotoChoice (Here extractResult) = extractChoice

  -- Step 3: Route (may exit directly for IntentStop)
  routeChoice <- routeHandler extractResult
  case routeChoice of
    GotoChoice (Here actInput) -> do
      -- Step 4: Generate response
      actChoice <- actHandler actInput
      let GotoChoice (Here response) = actChoice
      pure response

    GotoChoice (There (Here response)) ->
      -- Direct exit (IntentStop with Summary)
      pure response


-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS (adapted from Tidying.Loop)
-- ════════════════════════════════════════════════════════════════════════════

-- | Vision system prompt
visionSystemPrompt :: T.Text
visionSystemPrompt = T.unlines
  [ "You are a tidying assistant analyzing a photo of a space."
  , "Your job is to understand what you see and identify the best first step."
  , ""
  , "Be specific about visible items - mention actual objects, not vague categories."
  , "For 'blocked_function', describe what the mess prevents (sitting, working, sleeping)."
  , "For 'first_target', pick something quick to clear that would create visible progress."
  ]

-- | Vision user prompt
visionPrompt :: T.Text
visionPrompt = "Analyze this space. What do you see? How messy is it? What should we tackle first?"

-- | Convert PhotoAnalysisOutput to PhotoAnalysis
outputToAnalysis :: PhotoAnalysisOutput -> PhotoAnalysis
outputToAnalysis pao = PhotoAnalysis
  { paRoomType = pao.paoRoomType
  , paChaosLevel = pao.paoChaosLevel
  , paVisibleItems = map (\(ItemName n) -> n) pao.paoVisibleItems
  , paBlockedFunction = pao.paoBlockedFunction
  , paFirstTarget = fmap (\(ItemName n) -> n) pao.paoFirstTarget
  }

-- | Generate response for action (from Loop.hs actResponse)
generateActResponse
  :: TidyingEs es
  => TidyingContext
  -> Action
  -> Eff es T.Text
generateActResponse ctx action = do
  logDebug $ "Calling LLM for action: " <> T.pack (show action)
  let systemPrompt = renderActPrompt ctx action
      userMsg = "Action: " <> T.pack (show action)
             <> maybe "" (\t -> "\nUser said: " <> t) ctx.tcUserText

  result <- llmCallWithTools @ActOutput systemPrompt userMsg actOutputSchema.schemaJSON tidyingTools

  case result of
    Left err -> do
      logWarn $ "Act failed: " <> err
      emit $ ErrorOccurred err
      pure $ "Error: " <> err <> "\n\nWhat's next?"
    Right output -> pure output.aoResponse

-- | Apply state transition (adapted from Loop.hs)
--
-- Note: This is a simplified version. The full version in Loop.hs
-- handles all the PhaseData transitions. We should refactor to share.
applyStateTransition
  :: (State SessionState :> es, Time :> es)
  => Extract
  -> Action
  -> Phase
  -> Eff es ()
applyStateTransition _extract action _nextPhase = do
  now <- getCurrentTime
  modify @SessionState $ \st ->
    st
      { itemsProcessed = st.itemsProcessed + itemDelta action
      , sessionStart = st.sessionStart <|> Just now
      -- TODO: Full phaseData transition logic from Loop.hs
      }
  where
    (<|>) :: Maybe a -> Maybe a -> Maybe a
    (<|>) (Just x) _ = Just x
    (<|>) Nothing y = y

-- | Item delta from action
itemDelta :: Action -> Int
itemDelta InstructTrash = 1
itemDelta (InstructPlace _) = 1
itemDelta InstructUnsure = 1
itemDelta _ = 0
