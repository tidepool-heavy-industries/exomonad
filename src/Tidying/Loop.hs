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

    -- * Events (re-exported from Tidying.Events)
  , TidyingEvent(..)
  ) where

import Effectful
import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Data.Map.Strict qualified as Map

import Tidepool.Effect
  ( State, LLM, Emit, Log, Time, TurnOutcome(..), TurnParseResult(..), TurnResult(..)
  , get, modify, emit, logDebug, logInfo, logWarn, getCurrentTime
  , llmCallEither, llmCallWithTools, runTurnContent
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
import Tidying.Tools (tidyingTools)
import Tidying.Events (TidyingEvent(..))
import Tidying.Types (ItemName(..), SpaceFunction(..), CategoryName(..), chaosLevelToText)

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
  , responseSessionEnded :: Bool
    -- ^ True if session should end (user said stop/quit/done)
  } deriving (Show, Eq)

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
     , Time :> es
     )
  => UserInput
  -> Eff es Response
tidyingTurn input = do
  st <- get @SessionState

  logDebug $ "Phase: " <> T.pack (show (phase st))
  logDebug $ "User text: " <> maybe "(none)" id input.inputText

  -- OBSERVE: Analyze photos if present (stubbed for now)
  mPhotoAnalysis <- analyzePhotos input.inputPhotos

  -- EXTRACT: Get structured info from user input
  -- LLM extracts facts, Haskell decides what to do
  -- Pass photo analysis so extraction LLM knows what was seen
  extract <- extractFromInput mPhotoAnalysis input

  logInfo $ "Extract: " <> T.pack (show extract)
  emit $ SituationClassified (T.pack $ show extract.exIntent)

  -- DECIDE: Pure routing based on extraction (no LLM)
  -- Photo analysis informs routing (chaos level, blocked function, first target)
  let (action, nextPhase) = decideFromExtract st mPhotoAnalysis extract

  logDebug $ "Action: " <> T.pack (show action)
  emit $ ActionTaken action

  -- Emit phase change if different
  when (nextPhase /= phase st) $ do
    logInfo $ "Phase change: " <> T.pack (show (phase st)) <> " -> " <> T.pack (show nextPhase)
    emit $ PhaseChanged (phase st) nextPhase

  -- Update state BEFORE generating response so counts/piles are accurate
  applyStateTransitionFromExtract extract action nextPhase

  -- Build context from UPDATED state
  st' <- get @SessionState
  let ctx' = buildTidyingContext st' mPhotoAnalysis input.inputText

  -- ACT: Generate response (now has correct pile counts, etc.)
  response <- actResponse ctx' action

  -- Check for session end
  let sessionEnded = extract.exIntent == IntentStop
  when sessionEnded $ do
    emit $ SessionEnded st'.itemsProcessed

  pure Response
    { responseText = response
    , responsePhase = nextPhase
    , responseItemsProcessed = st'.itemsProcessed
    , responseSessionEnded = sessionEnded
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
      emit $ PhotoAnalyzed (analysis.paRoomType <> ", " <> chaosLevelToText analysis.paChaosLevel)
      pure (Just analysis)

    TurnCompleted (TurnParseFailed {tpfError}) -> do
      logWarn $ "Photo analysis parse failed: " <> T.pack tpfError
      -- Fall back to stub
      let analysis = stubPhotoAnalysis photos
      case analysis of
        Just pa -> emit $ PhotoAnalyzed (pa.paRoomType <> " (stub), " <> chaosLevelToText pa.paChaosLevel)
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
  , paVisibleItems = map (\(ItemName n) -> n) pao.paoVisibleItems
  , paBlockedFunction = pao.paoBlockedFunction
  , paFirstTarget = fmap (\(ItemName n) -> n) pao.paoFirstTarget
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
--
-- Photo analysis context is included when available so the LLM can
-- understand what was seen even if the user just sent a photo without text.
extractFromInput
  :: ( LLM :> es
     , Log :> es
     )
  => Maybe PhotoAnalysis  -- ^ Photo analysis context (if photos were analyzed)
  -> UserInput
  -> Eff es Extract
extractFromInput mPhotoAnalysis input = do
  logDebug "Calling LLM for extraction"

  -- Build user message with photo context if available
  let textPart = maybe "" id input.inputText
      photoContext = case mPhotoAnalysis of
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

  case result of
    Left err -> do
      logWarn $ "Extract parse failed: " <> err
      -- Fallback: assume they want to continue
      pure Extract
        { exIntent = IntentContinue
        , exItem = Nothing
        , exChoice = Nothing
        , exPlace = Nothing
        , exFunction = Nothing
        , exAnchors = Nothing
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
      -- Need LLM for this action - pass tools so LLM can propose dispositions
      logDebug $ "Calling LLM for action: " <> T.pack (show action)
      let systemPrompt = renderActPrompt ctx action
          -- Include action type and context in user message
          userMsg = "Action: " <> T.pack (show action)
                 <> maybe "" (\t -> "\nUser said: " <> t) ctx.tcUserText

      result <- llmCallWithTools @ActOutput systemPrompt userMsg actOutputSchema.schemaJSON tidyingTools

      case result of
        Left err -> do
          logWarn $ "Act failed: " <> err
          pure "What's next?"  -- fallback
        Right output -> pure output.aoResponse

-- ══════════════════════════════════════════════════════════════
-- STATE TRANSITIONS
-- ══════════════════════════════════════════════════════════════

-- | Apply state transition based on extraction and action
--
-- Sets sessionStart on first turn so session duration can be calculated.
-- Handles the PhaseData sum type transitions properly.
applyStateTransitionFromExtract
  :: ( State SessionState :> es
     , Time :> es
     )
  => Extract
  -> Action
  -> Phase
  -> Eff es ()
applyStateTransitionFromExtract extract action nextPhase = do
  -- Get current time to set sessionStart on first turn
  now <- getCurrentTime
  modify @SessionState $ \st ->
    -- Capture unsure items BEFORE clearing (for split transitions)
    let unsureItems = st.piles.unsure

        -- Calculate new piles
        newPiles = case action of
          InstructSplit _ ->
            -- Clear unsure pile - items will be moved to emergent categories
            st.piles { unsure = [] }
          _ ->
            updatePilesFromExtract st.piles extract action

        -- Build new PhaseData based on transition
        -- Pass unsure items so they can be distributed to categories
        newPhaseData = transitionPhaseData st.phaseData extract action nextPhase unsureItems

    in st
      { phaseData = newPhaseData
      , itemsProcessed = st.itemsProcessed + itemDelta action
      , piles = newPiles
      -- Set sessionStart on first turn (keep existing if already set)
      , sessionStart = st.sessionStart <|> Just now
      }

-- | Transition PhaseData based on action and next phase
--
-- The unsureItems parameter carries items from the unsure pile so they can
-- be distributed to emergent categories when transitioning to Refining.
transitionPhaseData :: PhaseData -> Extract -> Action -> Phase -> [ItemName] -> PhaseData
transitionPhaseData pd extract action nextPhase unsureItems = case (pd, nextPhase) of
  -- Surveying: accumulate function/anchors
  (SurveyingData mFn anchors, Surveying) ->
    SurveyingData
      (mFn <|> extract.exFunction)
      (anchors <> maybe [] id extract.exAnchors)

  -- Surveying → Sorting: start sorting with gathered data
  (SurveyingData mFn anchors, Sorting) ->
    let fn = case mFn <|> extract.exFunction of
               Just f -> f
               Nothing -> SpaceFunction "general"  -- fallback
        newAnchors = anchors <> maybe [] id extract.exAnchors
        active = ActiveState fn newAnchors Nothing
    in SortingData active (extract.exItem)

  -- Sorting: update current item, maybe anxiety
  (SortingData active _item, Sorting) ->
    SortingData active (extract.exItem <|> _item)

  -- Sorting → Splitting: create categories from split
  (SortingData active _item, Splitting) ->
    case action of
      InstructSplit cats ->
        SplittingData active cats
      _ ->
        -- Shouldn't happen, but fallback to default categories
        SplittingData active (CategoryName "unsure" :| [])

  -- Splitting → Sorting: go back to sorting
  (SplittingData active _cats, Sorting) ->
    SortingData active Nothing

  -- Sorting/Splitting → Refining: start refining
  (SortingData active _item, Refining) ->
    let cat = extractNextCategory action
        -- Put all unsure items in first (and only) category
        catsMap = Map.singleton cat unsureItems
    in RefiningData active catsMap cat Nothing

  (SplittingData active cats, Refining) ->
    -- Move to first category for refining
    -- Put all unsure items in the first category (user will re-sort from there)
    let cat = NE.head cats
        catsMap = Map.fromList [(c, []) | c <- NE.toList cats]
        catsWithItems = Map.adjust (++ unsureItems) cat catsMap
    in RefiningData active catsWithItems cat Nothing

  -- Refining: update current item/category
  (RefiningData active cats _curCat _item, Refining) ->
    let newCat = extractNextCategory action
        newItem = extract.exItem <|> _item
    in RefiningData active cats newCat newItem

  -- Any → DecisionSupport: remember stuck item
  (_, DecisionSupport) ->
    let active = getActiveStateFromPhaseData pd
        stuckItem = case extract.exItem of
                      Just i -> i
                      Nothing -> ItemName "unknown"
        returnPhase = phaseFromPhaseData pd
    in DecisionSupportData active stuckItem returnPhase

  -- DecisionSupport → Sorting: return from decision support
  (DecisionSupportData active _stuck _return, Sorting) ->
    SortingData active Nothing

  -- Fallback: keep existing data but might need to update item
  _ -> updateCurrentItemInPhaseData pd (extract.exItem)

-- | Extract next category from AckProgress action
extractNextCategory :: Action -> CategoryName
extractNextCategory (AckProgress msg)
  | "Let's do " `T.isPrefixOf` msg =
      CategoryName $ T.drop (T.length "Let's do ") msg
extractNextCategory _ = CategoryName "current"

-- | Get ActiveState from any PhaseData variant (defaults for Surveying)
getActiveStateFromPhaseData :: PhaseData -> ActiveState
getActiveStateFromPhaseData = \case
  SurveyingData mFn a ->
    let fn = maybe (SpaceFunction "general") id mFn
    in ActiveState fn a Nothing
  SortingData a _         -> a
  SplittingData a _       -> a
  RefiningData a _ _ _    -> a
  DecisionSupportData a _ _ -> a

-- | Get phase from PhaseData
phaseFromPhaseData :: PhaseData -> Phase
phaseFromPhaseData = \case
  SurveyingData {}        -> Surveying
  SortingData {}          -> Sorting
  SplittingData {}        -> Splitting
  RefiningData {}         -> Refining
  DecisionSupportData {}  -> DecisionSupport

-- | Update current item in PhaseData (for variants that have it)
updateCurrentItemInPhaseData :: PhaseData -> Maybe ItemName -> PhaseData
updateCurrentItemInPhaseData pd mItem = case pd of
  SortingData a _         -> SortingData a mItem
  RefiningData a cats cat _ -> RefiningData a cats cat mItem
  _ -> pd  -- Other variants don't have current item

-- | Update piles based on extraction
updatePilesFromExtract :: Piles -> Extract -> Action -> Piles
updatePilesFromExtract p extract _action = case (extract.exIntent, extract.exChoice) of
  -- User decided to trash
  (IntentDecided, Just ChoiceTrash) ->
    let item = maybe (ItemName "item") id extract.exItem
    in p { out = item : p.out }

  -- User decided to place/keep
  (IntentDecided, Just ChoicePlace) ->
    let item = maybe (ItemName "item") id extract.exItem
    in p { belongs = item : p.belongs }

  (IntentDecided, Just ChoiceKeep) ->
    let item = maybe (ItemName "item") id extract.exItem
    in p { belongs = item : p.belongs }

  -- User is unsure
  (IntentDecided, Just ChoiceUnsure) ->
    let item = maybe (ItemName "item") id extract.exItem
    in p { unsure = item : p.unsure }

  -- Note: InstructSplit handled specially in applyStateTransitionFromExtract
  -- to move items to emergent categories instead of losing them

  _ -> p

