{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Tidying Loop - OODA-based tidying agent
--
-- = Architecture
--
-- The loop follows OODA (Observe-Orient-Decide-Act):
--
-- 1. OBSERVE: User provides input (photos + text)
-- 2. ORIENT: LLM classifies the situation
-- 3. DECIDE: Pure routing based on situation + phase
-- 4. ACT: Generate response (canned or LLM)
--
-- DECIDE is deterministic (no LLM). ORIENT and ACT may call the LLM.

module Tidying.Loop
  ( -- * Main loop
    tidyingTurn
  , Response(..)

    -- * Events
  , TidyingEvent(..)
  ) where

import Effectful
import Control.Monad (when)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Map.Strict qualified as Map

import Tidepool.Effect
  ( State, LLM, Emit, Log, TurnOutcome(..), TurnParseResult(..), TurnResult(..)
  , get, modify, emit, logDebug, logInfo, logWarn
  , llmCallEither, runTurnContent
  )
import Tidepool.Anthropic.Http (ContentBlock(..))
import Tidepool.Template (Schema(..))

import Tidying.State
import Tidying.Action
import Tidying.Decide (decideFromExtract)
import Tidying.Context
import Tidying.Output
  ( ActOutput(..), PhotoAnalysisOutput(..), photoAnalysisSchema
  , Extract(..), Intent(..), Choice(..)
  )
import Tidying.Templates
import Tidying.Act (cannedResponse)

-- ══════════════════════════════════════════════════════════════
-- TYPES
-- ══════════════════════════════════════════════════════════════

-- | Response from a tidying turn
data Response = Response
  { responseText :: Text
    -- ^ The message to send to user
  , responsePhase :: Phase
    -- ^ Current phase after this turn
  , responseItemsProcessed :: Int
    -- ^ Running count of items handled
  } deriving (Show, Eq)

-- | Events emitted during tidying
data TidyingEvent
  = PhotoAnalyzed Text          -- ^ Photo was analyzed
  | SituationClassified Text    -- ^ Situation was classified
  | ActionTaken Action          -- ^ Action was taken
  | PhaseChanged Phase Phase    -- ^ Phase transition (from, to)
  | SessionEnded Int            -- ^ Session ended, items processed
  | UserInputReceived Text      -- ^ User input received (for chat display)
  | ResponseGenerated Text      -- ^ Response generated (for chat display)
  deriving (Show, Eq)

-- ══════════════════════════════════════════════════════════════
-- MAIN LOOP
-- ══════════════════════════════════════════════════════════════

-- | Run a single tidying turn
--
-- Uses Tidepool effects for LLM calls, state, and events.
-- Follows OODA: Observe → Orient → Decide → Act
tidyingTurn
  :: ( State SessionState :> es
     , LLM :> es
     , Emit TidyingEvent :> es
     , Log :> es
     )
  => UserInput
  -> Eff es Response
tidyingTurn input = do
  st <- get @SessionState

  logDebug $ "Phase: " <> T.pack (show st.phase)
  logDebug $ "User text: " <> maybe "(none)" id input.inputText

  -- OBSERVE: Analyze photos if present (stubbed for now)
  mPhotoAnalysis <- analyzePhotos input.inputPhotos

  -- Build context for templates
  let ctx = buildTidyingContext st mPhotoAnalysis input.inputText

  -- EXTRACT: Get structured info from user input
  -- LLM extracts facts, Haskell decides what to do
  extract <- extractFromInput input

  logInfo $ "Extract: " <> T.pack (show extract)
  emit $ SituationClassified (T.pack $ show extract.exIntent)

  -- DECIDE: Pure routing based on extraction (no LLM)
  let (action, nextPhase) = decideFromExtract st extract

  logDebug $ "Action: " <> T.pack (show action)
  emit $ ActionTaken action

  -- Emit phase change if different
  when (nextPhase /= st.phase) $ do
    logInfo $ "Phase change: " <> T.pack (show st.phase) <> " -> " <> T.pack (show nextPhase)
    emit $ PhaseChanged st.phase nextPhase

  -- ACT: Generate response
  response <- actResponse ctx action

  -- Update state
  applyStateTransitionFromExtract extract action nextPhase

  -- Check for session end
  when (extract.exIntent == IntentStop) $ do
    finalState <- get @SessionState
    emit $ SessionEnded finalState.itemsProcessed

  pure Response
    { responseText = response
    , responsePhase = nextPhase
    , responseItemsProcessed = st.itemsProcessed + itemDelta action
    }

-- | How many items were processed by this action
itemDelta :: Action -> Int
itemDelta InstructTrash = 1
itemDelta (InstructPlace _) = 1
itemDelta InstructUnsure = 1
itemDelta _ = 0

-- ══════════════════════════════════════════════════════════════
-- OBSERVE: Photo Analysis (via Vision)
-- ══════════════════════════════════════════════════════════════

-- | Analyze photos using vision
-- Calls LLM with image content to understand the space
analyzePhotos
  :: ( LLM :> es
     , Emit TidyingEvent :> es
     , Log :> es
     )
  => [Photo]
  -> Eff es (Maybe PhotoAnalysis)
analyzePhotos [] = pure Nothing
analyzePhotos photos = do
  logDebug $ "Analyzing " <> T.pack (show $ length photos) <> " photos via vision"

  -- Convert photos to image sources
  let imageSources = map photoToImageSource photos
      imageBlocks = map ImageBlock imageSources
      -- Build content: text prompt followed by images
      userContent = TextBlock visionPrompt : imageBlocks

  -- Call LLM with vision
  result <- runTurnContent @PhotoAnalysisOutput
    visionSystemPrompt
    userContent
    photoAnalysisSchema.schemaJSON
    []  -- no tools for vision

  case result of
    TurnCompleted (TurnParsed tr) -> do
      let analysis = outputToAnalysis tr.trOutput
      emit $ PhotoAnalyzed (analysis.paRoomType <> ", " <> analysis.paChaosLevel)
      pure (Just analysis)

    TurnCompleted (TurnParseFailed {tpfError}) -> do
      logWarn $ "Photo analysis parse failed: " <> T.pack tpfError
      -- Fall back to stub
      let analysis = stubPhotoAnalysis photos
      case analysis of
        Just pa -> emit $ PhotoAnalyzed (pa.paRoomType <> " (stub), " <> pa.paChaosLevel)
        Nothing -> pure ()
      pure analysis

    TurnBroken reason -> do
      logWarn $ "Photo analysis broken: " <> reason
      pure (stubPhotoAnalysis photos)

-- | Convert PhotoAnalysisOutput to PhotoAnalysis
outputToAnalysis :: PhotoAnalysisOutput -> PhotoAnalysis
outputToAnalysis pao = PhotoAnalysis
  { paRoomType = pao.paoRoomType
  , paChaosLevel = pao.paoChaosLevel
  , paVisibleItems = pao.paoVisibleItems
  , paBlockedFunction = pao.paoBlockedFunction
  , paFirstTarget = pao.paoFirstTarget
  }

-- | System prompt for photo analysis
visionSystemPrompt :: Text
visionSystemPrompt = T.unlines
  [ "You are a tidying assistant analyzing a photo of a space."
  , "Your job is to understand what you see and identify the best first step."
  , ""
  , "Be specific about visible items - mention actual objects, not vague categories."
  , "For 'blocked_function', describe what the mess prevents (sitting, working, sleeping)."
  , "For 'first_target', pick something quick to clear that would create visible progress."
  ]

-- | User prompt for photo analysis
visionPrompt :: Text
visionPrompt = "Analyze this space. What do you see? How messy is it? What should we tackle first?"

-- ══════════════════════════════════════════════════════════════
-- EXTRACT: Information Extraction from User Input
-- ══════════════════════════════════════════════════════════════

-- | Extract structured info from user input
-- LLM extracts facts (intent, item, choice), Haskell decides what to do
extractFromInput
  :: ( LLM :> es
     , Log :> es
     )
  => UserInput
  -> Eff es Extract
extractFromInput input = do
  logDebug "Calling LLM for extraction"
  let userMsg = maybe "(photo only)" id input.inputText

  result <- llmCallEither @Extract renderExtractPrompt userMsg extractSchema.schemaJSON

  case result of
    Left err -> do
      logWarn $ "Extract parse failed: " <> err
      -- Fallback: assume they want to continue
      pure Extract
        { exIntent = IntentContinue
        , exItem = Nothing
        , exChoice = Nothing
        , exPlace = Nothing
        }
    Right ext -> pure ext

-- ══════════════════════════════════════════════════════════════
-- ACT: Response Generation
-- ══════════════════════════════════════════════════════════════

-- | Generate response for action
-- Uses canned responses when possible, LLM for complex ones
actResponse
  :: ( LLM :> es
     , Log :> es
     )
  => TidyingContext
  -> Action
  -> Eff es Text
actResponse ctx action = do
  -- Check for canned response first
  case cannedResponse action of
    Just response -> do
      logDebug $ "Canned response for: " <> T.pack (show action)
      pure response
    Nothing -> do
      -- Need LLM for this action
      logDebug $ "Calling LLM for action: " <> T.pack (show action)
      let systemPrompt = renderActPrompt ctx action
          -- Include action type and context in user message
          userMsg = "Action: " <> T.pack (show action)
                 <> maybe "" (\t -> "\nUser said: " <> t) ctx.tcUserText

      result <- llmCallEither @ActOutput systemPrompt userMsg actOutputSchema.schemaJSON

      case result of
        Left err -> do
          logWarn $ "Act parse failed: " <> err
          pure "What's next?"  -- fallback
        Right output -> pure output.aoResponse

-- ══════════════════════════════════════════════════════════════
-- STATE TRANSITIONS
-- ══════════════════════════════════════════════════════════════

-- | Apply state transition based on extraction and action
applyStateTransitionFromExtract
  :: State SessionState :> es
  => Extract
  -> Action
  -> Phase
  -> Eff es ()
applyStateTransitionFromExtract extract action nextPhase = modify @SessionState $ \st -> st
  { phase = nextPhase
  , itemsProcessed = st.itemsProcessed + itemDelta action
  , piles = updatePilesFromExtract st.piles extract action
  , emergentCats = updateCats st.emergentCats action
  , currentCategory = updateCurrentCategory st.currentCategory action
  }

-- | Update piles based on extraction
updatePilesFromExtract :: Piles -> Extract -> Action -> Piles
updatePilesFromExtract p extract action = case (extract.exIntent, extract.exChoice, action) of
  -- User decided to trash
  (IntentDecided, Just ChoiceTrash, _) ->
    let item = maybe "item" id extract.exItem
    in p { out = item : p.out }

  -- User decided to place/keep
  (IntentDecided, Just ChoicePlace, _) ->
    let item = maybe "item" id extract.exItem
    in p { belongs = item : p.belongs }

  (IntentDecided, Just ChoiceKeep, _) ->
    let item = maybe "item" id extract.exItem
    in p { belongs = item : p.belongs }

  -- User is unsure
  (IntentDecided, Just ChoiceUnsure, _) ->
    let item = maybe "item" id extract.exItem
    in p { unsure = item : p.unsure }

  -- After split, clear unsure (items moved to emergent cats)
  (_, _, InstructSplit _) ->
    p { unsure = [] }

  _ -> p

-- | Update emergent categories
updateCats :: Map.Map Text [Text] -> Action -> Map.Map Text [Text]
updateCats cats action = case action of
  InstructSplit catNames ->
    foldr (\name -> Map.insert name []) cats catNames
  _ -> cats

-- | Update current category being refined
updateCurrentCategory :: Maybe Text -> Action -> Maybe Text
updateCurrentCategory current action = case action of
  AckProgress msg
    | "Let's do " `T.isPrefixOf` msg ->
        Just $ T.drop (T.length "Let's do ") msg
  _ -> current

