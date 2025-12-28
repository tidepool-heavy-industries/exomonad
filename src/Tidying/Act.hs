{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tidying.Act
  ( -- * ACT: Generate response
    act
  , ActContext(..)

    -- * Canned responses
  , cannedResponse
  ) where

import Data.Aeson (ToJSON(..), (.=), object)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

import Tidying.State
import Tidying.Action

-- | Context for act templates
data ActContext = ActContext
  { acFunction      :: Maybe Text
  , acAnchors       :: [Text]
  , acPhotoAnalysis :: Maybe Text
  , acUnsurePile    :: [Text]
  , acItem          :: Maybe Text      -- for decision aid
  , acTrigger       :: Maybe Text      -- for pivot
  , acAlternative   :: Maybe Text      -- for pivot
  , acSplitCats     :: [Text]          -- for split
  , acItemsProcessed :: Int
  , acSessionMinutes :: Int
  } deriving (Show, Generic)

instance ToJSON ActContext where
  toJSON ActContext{..} = object
    [ "function" .= acFunction
    , "anchors" .= acAnchors
    , "photo_analysis" .= acPhotoAnalysis
    , "unsure_pile" .= acUnsurePile
    , "item" .= acItem
    , "trigger" .= acTrigger
    , "alternative" .= acAlternative
    , "split_categories" .= acSplitCats
    , "items_processed" .= acItemsProcessed
    , "session_minutes" .= acSessionMinutes
    ]

-- | ACT: Generate response text
-- For actions that need LLM, calls the template runner
-- For canned actions, returns fixed text
act
  :: Monad m
  => (Action -> ActContext -> m Text)  -- ^ LLM template runner
  -> SessionState
  -> Maybe Text                         -- ^ Photo analysis
  -> Action
  -> m Text
act runLLM st photoAnalysis action =
  case cannedResponse action of
    Just response -> pure response
    Nothing -> do
      let ctx = buildActContext st photoAnalysis action
      runLLM action ctx

-- | Build context for act template
buildActContext :: SessionState -> Maybe Text -> Action -> ActContext
buildActContext st photoAnalysis action = ActContext
  { acFunction = st.function
  , acAnchors = st.anchors
  , acPhotoAnalysis = photoAnalysis
  , acUnsurePile = st.piles.unsure
  , acItem = extractItem action
  , acTrigger = extractTrigger action
  , acAlternative = extractAlternative action
  , acSplitCats = extractSplitCats action
  , acItemsProcessed = st.itemsProcessed
  , acSessionMinutes = 0  -- TODO: compute from sessionStart
  }

extractItem :: Action -> Maybe Text
extractItem (DecisionAid item) = Just item
extractItem _ = Nothing

extractTrigger :: Action -> Maybe Text
extractTrigger (PivotAway trigger _) = Just trigger
extractTrigger _ = Nothing

extractAlternative :: Action -> Maybe Text
extractAlternative (PivotAway _ alt) = Just alt
extractAlternative _ = Nothing

extractSplitCats :: Action -> [Text]
extractSplitCats (InstructSplit cats) = cats
extractSplitCats _ = []

-- | Canned responses that don't need LLM
cannedResponse :: Action -> Maybe Text
cannedResponse = \case
  -- Questions
  AskFunction ->
    Just "What do you need to DO in this space?"

  AskAnchors ->
    Just "What's definitely staying no matter what?"

  AskWhatIsIt ->
    Just "What is it?"

  AskWhereLive ->
    Just "Desk or somewhere else?"

  -- Simple instructions
  InstructTrash ->
    Just "Trash. Toss it by the door. Next."

  InstructUnsure ->
    Just "Unsure pile, floor to your right. Next."

  InstructNext ->
    Just "Next thing."

  InstructBag ->
    Just "Bag the trash by the door."

  InstructPlace loc ->
    Just $ loc <> ". Next."

  -- Energy check is fixed
  EnergyCheck ->
    Just "Energy check: keep going, or stop here for today?"

  -- These need LLM
  FirstInstruction -> Nothing
  InstructSplit _ -> Nothing
  DecisionAid _ -> Nothing
  PivotAway _ _ -> Nothing
  AckProgress _ -> Nothing
  Summary -> Nothing
