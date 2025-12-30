{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Tidying Loop - Mode-based turn loop
--
-- = Architecture
--
-- Simple turn loop where Mode controls everything:
--
-- 1. Mode determines template, tools, and output schema
-- 2. LLM receives system prompt + photos + text + history
-- 3. LLM produces structured output + optional tool calls
-- 4. Output updates mode data; transition tools change mode
--
-- = Key insight
--
-- Mode is a SUM TYPE with data, not an enum. Each mode carries
-- its own context fields. The LLM navigates between modes via
-- transition tools (which return ToolBreak).

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
import Data.Aeson (Value, fromJSON, Result(..))
import Data.Text (Text)
import Data.Text qualified as T

import Tidepool.Effect
  ( State, LLM, Emit, Log, Time, TurnOutcome(..), TurnParseResult(..), TurnResult(..)
  , get, modify, emit, logDebug, logInfo, logWarn, getCurrentTime
  , runTurnContent
  )
import Tidepool.Anthropic.Http (ContentBlock(..))
import Tidepool.Template (Schema(..))

import Tidying.State
import Tidying.Context
import Tidying.Output
  ( PhotoAnalysisOutput(..), photoAnalysisSchema
  , SurveyingOutput(..), SortingOutput(..), ClarifyingOutput(..)
  , DecisionSupportOutput(..), WindingDownOutput(..)
  , schemaForMode
  )
import Tidying.Templates (templateForMode)
import Tidying.Tools (toolsForMode)
import Tidying.Events (TidyingEvent(..))
import Tidying.Types (ItemName(..), SpaceFunction(..), chaosLevelToText)

-- ══════════════════════════════════════════════════════════════
-- TYPES
-- ══════════════════════════════════════════════════════════════

-- | Response from a tidying turn
data Response = Response
  { responseText :: Text
    -- ^ The message to send to user
  , responseMode :: Mode
    -- ^ Current mode after this turn
  , responseItemsProcessed :: Int
    -- ^ Running count of items handled
  , responseSessionEnded :: Bool
    -- ^ True if session should end
  } deriving (Show, Eq)

-- ══════════════════════════════════════════════════════════════
-- MAIN LOOP
-- ══════════════════════════════════════════════════════════════

-- | Run a single tidying turn
--
-- Mode-based loop:
-- 1. Get mode from state
-- 2. Select template, tools, schema for mode
-- 3. Build context from mode data + session state
-- 4. Single multimodal LLM call
-- 5. Parse mode-specific output
-- 6. Update state from output
-- 7. Return response
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

  logDebug $ "Mode: " <> modeName st.mode
  logDebug $ "User text: " <> maybe "(none)" id input.inputText

  -- Set session start on first turn
  now <- getCurrentTime
  when (st.sessionStart == Nothing) $
    modify @SessionState $ \s -> s { sessionStart = Just now }

  -- Analyze photos if present
  mPhotoAnalysis <- analyzePhotos input.inputPhotos

  -- Build context from state + mode data + photo analysis
  let ctx = buildTidyingContext st mPhotoAnalysis input.inputText

  -- Get mode-specific config
  let systemPrompt = templateForMode st.mode ctx
      tools = toolsForMode st.mode
      schema = schemaForMode st.mode

  logDebug $ "System prompt:\n" <> systemPrompt
  logDebug $ "Tools: " <> T.pack (show (length tools))

  -- Build user content (photos + text)
  let userContent = buildUserContent input mPhotoAnalysis

  -- Single multimodal LLM call
  result <- runTurnContent @Value
    systemPrompt
    userContent
    schema
    tools

  -- Handle result
  case result of
    TurnCompleted (TurnParsed tr) -> do
      -- Update state from mode-specific output
      responseText <- updateFromOutput st.mode tr.trOutput

      st' <- get @SessionState
      emit $ ResponseGenerated responseText

      pure Response
        { responseText = responseText
        , responseMode = st'.mode
        , responseItemsProcessed = st'.itemsProcessed
        , responseSessionEnded = isSessionEnded st'.mode
        }

    TurnCompleted (TurnParseFailed {tpfError}) -> do
      logWarn $ "Parse failed: " <> T.pack tpfError
      emit $ ErrorOccurred $ "Parse failed: " <> T.pack tpfError
      pure Response
        { responseText = "I'm having trouble processing that. What's next?"
        , responseMode = st.mode
        , responseItemsProcessed = st.itemsProcessed
        , responseSessionEnded = False
        }

    TurnBroken reason -> do
      -- ToolBreak or other break - mode already changed by tool
      logInfo $ "Turn broken: " <> reason
      st' <- get @SessionState

      -- If mode changed, start a new turn with synthetic message
      if st'.mode /= st.mode
        then do
          logInfo $ "Mode changed to: " <> modeName st'.mode
          -- Recursive call with synthetic input
          tidyingTurn $ UserInput [] (Just reason)
        else do
          pure Response
            { responseText = "Let's continue. What's next?"
            , responseMode = st'.mode
            , responseItemsProcessed = st'.itemsProcessed
            , responseSessionEnded = isSessionEnded st'.mode
            }

-- | Check if session has ended
isSessionEnded :: Mode -> Bool
isSessionEnded (WindingDown _) = False  -- Not ended until end_session tool called
isSessionEnded _ = False

-- | Build user content for multimodal call
buildUserContent :: UserInput -> Maybe PhotoAnalysis -> [ContentBlock]
buildUserContent input mPhotoAnalysis =
  photoBlocks ++ contextBlock ++ textBlock
  where
    photoBlocks = map photoToContentBlock input.inputPhotos

    contextBlock = case mPhotoAnalysis of
      Nothing -> []
      Just pa -> [TextBlock $ T.unlines
        [ "[Photo analysis]"
        , "Room type: " <> pa.paRoomType
        , "Clutter level: " <> chaosLevelToText pa.paChaosLevel
        , "Visible items: " <> T.intercalate ", " pa.paVisibleItems
        , maybe "" ("Blocked function: " <>) pa.paBlockedFunction
        , maybe "" ("First target: " <>) pa.paFirstTarget
        ]]

    textBlock = case input.inputText of
      Nothing -> []
      Just t -> [TextBlock t]

-- ══════════════════════════════════════════════════════════════
-- UPDATE FROM MODE-SPECIFIC OUTPUT
-- ══════════════════════════════════════════════════════════════

-- | Update state from mode-specific output, return response text
updateFromOutput
  :: ( State SessionState :> es
     , Log :> es
     )
  => Mode
  -> Value
  -> Eff es Text
updateFromOutput (Surveying _) output = do
  case fromJSON output of
    Error err -> do
      logWarn $ "Failed to parse SurveyingOutput: " <> T.pack err
      pure "What is this space for?"
    Success (out :: SurveyingOutput) -> do
      -- Update session-level fields from discoveries
      modify @SessionState $ \st -> st
        { spaceFunction = (SpaceFunction <$> out.svDiscoveredFunction) <|> st.spaceFunction
        , anchors = maybe st.anchors (map ItemName) out.svDiscoveredAnchors
        }
      pure out.svResponse

updateFromOutput (Sorting _) output = do
  case fromJSON output of
    Error err -> do
      logWarn $ "Failed to parse SortingOutput: " <> T.pack err
      pure "Next?"
    Success (out :: SortingOutput) -> do
      -- Update mode data
      modify @SessionState $ \st -> st
        { mode = Sorting $ SortingData
            { sdCurrentItem = out.sortCurrentItem
            , sdItemLocation = out.sortItemLocation
            }
        }
      pure out.sortResponse

updateFromOutput (Clarifying _) output = do
  case fromJSON output of
    Error err -> do
      logWarn $ "Failed to parse ClarifyingOutput: " <> T.pack err
      pure "Let me describe that item..."
    Success (out :: ClarifyingOutput) -> do
      -- Clarifying mode data is set by transition tool, not updated from output
      pure out.clResponse

updateFromOutput (DecisionSupport _) output = do
  case fromJSON output of
    Error err -> do
      logWarn $ "Failed to parse DecisionSupportOutput: " <> T.pack err
      pure "Let's think about this..."
    Success (out :: DecisionSupportOutput) -> do
      pure out.dsResponse

updateFromOutput (WindingDown _) output = do
  case fromJSON output of
    Error err -> do
      logWarn $ "Failed to parse WindingDownOutput: " <> T.pack err
      pure "Good work today."
    Success (out :: WindingDownOutput) -> do
      -- Update winding down data
      modify @SessionState $ \st -> st
        { mode = WindingDown $ WindingDownData
            { wdSessionSummary = out.wdoSessionSummary
            , wdNextTime = maybe [] id out.wdoNextTime
            }
        }
      pure out.wdoResponse

-- ══════════════════════════════════════════════════════════════
-- PHOTO ANALYSIS
-- ══════════════════════════════════════════════════════════════

-- | Analyze photos using vision
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

  let imageSources = map photoToContentBlock photos
      userContent = TextBlock visionPrompt : imageSources

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
      emit $ ErrorOccurred $ "Photo analysis failed: " <> T.pack tpfError
      pure (stubPhotoAnalysis photos)

    TurnBroken reason -> do
      logWarn $ "Photo analysis broken: " <> reason
      emit $ ErrorOccurred $ "Photo analysis error: " <> reason
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

-- | Convert Photo to ContentBlock for user content
photoToContentBlock :: Photo -> ContentBlock
photoToContentBlock photo = ImageBlock (photoToImageSource photo)
