{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tidying.Orient
  ( -- * ORIENT: Classify situation
    orient
  , OrientContext(..)
  , buildOrientContext
  ) where

import Data.Aeson (ToJSON(..), (.=), object)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

import Tidying.State
import Tidying.Situation

-- For now, stub the LLM/template parts
-- In real impl, this would use Tidepool.Effect.LLM

-- | Context passed to orient template
data OrientContext = OrientContext
  { ocPhase         :: Phase
  , ocFunction      :: Maybe Text
  , ocAnchors       :: [Text]
  , ocUnsureCount   :: Int
  , ocLastAnxiety   :: Maybe Text
  , ocPhotoAnalysis :: Maybe Text  -- LLM-generated description of photos
  , ocUserText      :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON OrientContext where
  toJSON OrientContext{..} = object
    [ "phase" .= show ocPhase
    , "function" .= ocFunction
    , "anchors" .= ocAnchors
    , "unsure_count" .= ocUnsureCount
    , "last_anxiety" .= ocLastAnxiety
    , "photo_analysis" .= ocPhotoAnalysis
    , "user_text" .= ocUserText
    ]

-- | Build orient context from state and input
buildOrientContext
  :: SessionState
  -> Maybe Text        -- ^ Photo analysis (pre-computed)
  -> Maybe Text        -- ^ User text
  -> OrientContext
buildOrientContext st photoAnalysis userText = OrientContext
  { ocPhase = st.phase
  , ocFunction = st.function
  , ocAnchors = st.anchors
  , ocUnsureCount = unsureCount st.piles
  , ocLastAnxiety = st.lastAnxiety
  , ocPhotoAnalysis = photoAnalysis
  , ocUserText = userText
  }

-- | ORIENT: Classify the situation
-- In real impl, this calls LLM with orient template
orient
  :: Monad m
  => (OrientContext -> m Text)  -- ^ LLM call (template runner)
  -> SessionState
  -> Maybe Text                  -- ^ Photo analysis
  -> UserInput
  -> m Situation
orient runLLM st photoAnalysis input = do
  let ctx = buildOrientContext st photoAnalysis input.inputText

  -- Fast-path: detect obvious situations without LLM
  case fastPathOrient st input of
    Just sit -> pure sit
    Nothing -> do
      -- Call LLM for classification
      response <- runLLM ctx
      case parseSituation response of
        Just sit -> pure sit
        Nothing -> pure ActionDone  -- fallback

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
        -- They answered function, could ask anchors or start moving
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
