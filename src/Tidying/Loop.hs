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
import Tidying.Situation (Situation(..), ItemClass(..), extractAnxietyTrigger)
import Tidying.Action
import Tidying.Decide (decide)
import Tidying.Context
import Tidying.Output (OrientOutput(..), ActOutput(..), PhotoAnalysisOutput(..), photoAnalysisSchema, applyOrientOutput, parseOrientToSituation)
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

  -- ORIENT: Classify situation
  situation <- orientSituation ctx st input

  logInfo $ "Situation: " <> T.pack (show situation)
  emit $ SituationClassified (T.pack $ show situation)

  -- DECIDE: Pure routing (no LLM)
  let (action, nextPhase) = decide st situation

  logDebug $ "Action: " <> T.pack (show action)
  emit $ ActionTaken action

  -- Emit phase change if different
  when (nextPhase /= st.phase) $ do
    logInfo $ "Phase change: " <> T.pack (show st.phase) <> " -> " <> T.pack (show nextPhase)
    emit $ PhaseChanged st.phase nextPhase

  -- ACT: Generate response
  response <- actResponse ctx action

  -- Update state
  applyStateTransition situation action nextPhase

  -- Check for session end
  when (situation == AllDone || situation == WantsToStop) $ do
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
-- ORIENT: Situation Classification
-- ══════════════════════════════════════════════════════════════

-- | Classify the situation using LLM
-- Uses fast-path for obvious cases, LLM for ambiguous ones
orientSituation
  :: ( LLM :> es
     , Log :> es
     , State SessionState :> es
     )
  => TidyingContext
  -> SessionState
  -> UserInput
  -> Eff es Situation
orientSituation ctx st input = do
  -- Fast-path: detect obvious situations without LLM
  case fastPathOrient st input of
    Just sit -> do
      logDebug $ "Fast-path orient: " <> T.pack (show sit)
      pure sit
    Nothing -> do
      -- Call LLM for classification
      logDebug "Calling LLM for orient"
      let systemPrompt = renderOrientPrompt ctx
          userMsg = maybe "(photo only)" id input.inputText

      result <- llmCallEither @OrientOutput systemPrompt userMsg orientOutputSchema.schemaJSON

      case result of
        Left err -> do
          logWarn $ "Orient parse failed: " <> err
          pure ActionDone  -- fallback
        Right output -> do
          -- Apply extracted info to state (function, anchors)
          modify @SessionState (applyOrientOutput output)
          pure $ parseOrientToSituation output

-- | Fast-path situations that don't need LLM
fastPathOrient :: SessionState -> UserInput -> Maybe Situation
fastPathOrient st input =
  let txt = input.inputText
  in case txt of
    -- Explicit signals
    Just t | isStopSignal t -> Just WantsToStop
    Just t | isContinueSignal t -> Just WantsToContinue
    Just t | isStuckSignal t -> Just (Stuck Nothing)

    -- Surveying phase fast-paths
    _ | st.phase == Surveying && not (hasFunction st) ->
        if isOverwhelmedSignal txt
          then Just OverwhelmedNeedMomentum
          else Just NeedFunction

    _ | st.phase == Surveying && hasFunction st && not (hasAnchors st) ->
        if isOverwhelmedSignal txt
          then Just OverwhelmedNeedMomentum
          else Nothing  -- let LLM decide

    -- Simple "done" responses
    Just t | isDoneSignal t -> Just ActionDone

    -- Need LLM for item classification, anxiety detection, etc.
    _ -> Nothing

isStopSignal :: Text -> Bool
isStopSignal t = any (`elem` T.words (T.toLower t)) ["stop", "pause", "tired", "later", "break"]

isContinueSignal :: Text -> Bool
isContinueSignal t = any (`elem` T.words (T.toLower t))
  ["continue", "keep", "going", "more", "yes", "yeah", "next"]

isStuckSignal :: Text -> Bool
isStuckSignal t = any (`elem` T.words (T.toLower t))
  ["stuck", "idk", "unsure", "help", "can't", "decide"]

isDoneSignal :: Text -> Bool
isDoneSignal t = T.toLower (T.strip t) `elem` ["done", "ok", "okay", "did it", "next"]

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

-- | Apply state transition based on situation and action
applyStateTransition
  :: State SessionState :> es
  => Situation
  -> Action
  -> Phase
  -> Eff es ()
applyStateTransition situation action nextPhase = modify @SessionState $ \st -> st
  { phase = nextPhase
  , itemsProcessed = st.itemsProcessed + itemDelta action
  , piles = updatePiles st.piles situation action
  , emergentCats = updateCats st.emergentCats action
  , lastAnxiety = extractAnxietyTrigger situation <|> st.lastAnxiety
  , currentCategory = updateCurrentCategory st.currentCategory action
  }
  where
    (<|>) :: Maybe a -> Maybe a -> Maybe a
    (<|>) (Just x) _ = Just x
    (<|>) Nothing y = y

-- | Update piles based on situation
updatePiles :: Piles -> Situation -> Action -> Piles
updatePiles p situation action = case (situation, action) of
  (ItemDescribed item Trash, _) ->
    p { out = item : p.out }

  (ItemDescribed item (Belongs _), _) ->
    p { belongs = item : p.belongs }

  (ItemDescribed item Unsure, _) ->
    p { unsure = item : p.unsure }

  -- After split, clear unsure (items moved to emergent cats)
  (_, InstructSplit _) ->
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

