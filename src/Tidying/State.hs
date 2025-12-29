{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tidying.State
  ( -- * Core types
    Phase(..)
  , PhaseData(..)
  , SessionState(..)
  , newSession

    -- * Piles
  , Piles(..)
  , emptyPiles
  , unsureCount

    -- * User input
  , UserInput(..)
  , Photo(..)

    -- * Accessors for phase-specific data
  , getFunction
  , getAnchors
  , getCurrentItem
  , getLastAnxiety
  , getCurrentCategory
  , getEmergentCats

    -- * Helpers
  , hasFunction
  , hasAnchors
  , isOverwhelmedSignal
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import GHC.Generics (Generic)

import Tidying.Types
  ( ItemName, SpaceFunction(..), AnxietyTrigger, CategoryName )

-- | Session phases
data Phase
  = Surveying        -- ^ Gathering photos, function, anchors
  | Sorting          -- ^ Main sorting loop: belongs/out/unsure
  | Splitting        -- ^ Breaking unsure pile into categories
  | Refining         -- ^ Working through sub-piles
  | DecisionSupport  -- ^ Helping user decide on stuck item
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Phase-specific data
--
-- Each phase has different fields that are valid/required.
-- This sum type makes invalid states unrepresentable.
data PhaseData
  = SurveyingData
      { sdGatheredFunction :: Maybe SpaceFunction
        -- ^ Function being gathered (Nothing until user provides it)
      , sdGatheredAnchors  :: [ItemName]
        -- ^ Anchors gathered so far
      }
  | SortingData
      { soFunction    :: SpaceFunction
        -- ^ Required! Can't be sorting without knowing the function
      , soAnchors     :: [ItemName]
      , soCurrentItem :: Maybe ItemName
        -- ^ Item currently being discussed
      , soLastAnxiety :: Maybe AnxietyTrigger
      }
  | SplittingData
      { spFunction    :: SpaceFunction
      , spAnchors     :: [ItemName]
      , spCategories  :: NonEmpty CategoryName
        -- ^ Categories to split into (NonEmpty - must have at least one)
      , spLastAnxiety :: Maybe AnxietyTrigger
      }
  | RefiningData
      { rfFunction        :: SpaceFunction
      , rfAnchors         :: [ItemName]
      , rfEmergentCats    :: Map CategoryName [ItemName]
        -- ^ Categories and their items
      , rfCurrentCategory :: CategoryName
        -- ^ Required! Can't be refining without a category
      , rfCurrentItem     :: Maybe ItemName
      , rfLastAnxiety     :: Maybe AnxietyTrigger
      }
  | DecisionSupportData
      { dsFunction    :: SpaceFunction
      , dsAnchors     :: [ItemName]
      , dsStuckItem   :: ItemName
        -- ^ Required! Can't be in decision support without a stuck item
      , dsReturnPhase :: Phase
        -- ^ Which phase to return to after decision
      , dsLastAnxiety :: Maybe AnxietyTrigger
      }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Pile tracking
data Piles = Piles
  { belongs :: [ItemName]  -- ^ Items that belong in the space
  , out     :: [ItemName]  -- ^ Items to remove (trash/donate)
  , unsure  :: [ItemName]  -- ^ Items not yet classified
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

emptyPiles :: Piles
emptyPiles = Piles [] [] []

unsureCount :: Piles -> Int
unsureCount p = length p.unsure

-- | Main session state
data SessionState = SessionState
  { phase          :: Phase
    -- ^ Current phase (redundant with phaseData constructor, but convenient)
  , phaseData      :: PhaseData
    -- ^ Phase-specific data
  , piles          :: Piles
    -- ^ Current pile contents
  , itemsProcessed :: Int
    -- ^ Count for progress tracking
  , sessionStart   :: Maybe UTCTime
    -- ^ When session started
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Fresh session
newSession :: SessionState
newSession = SessionState
  { phase = Surveying
  , phaseData = SurveyingData Nothing []
  , piles = emptyPiles
  , itemsProcessed = 0
  , sessionStart = Nothing
  }

-- | Photo wrapper (base64 or URL)
data Photo = Photo
  { photoData :: Text  -- ^ Base64 encoded or URL
  , photoMime :: Text  -- ^ MIME type
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | What the user can send
data UserInput = UserInput
  { inputPhotos :: [Photo]
  , inputText   :: Maybe Text
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- ══════════════════════════════════════════════════════════════
-- ACCESSORS FOR PHASE-SPECIFIC DATA
-- ══════════════════════════════════════════════════════════════

-- | Get function from any phase (Nothing only in Surveying before set)
getFunction :: SessionState -> Maybe SpaceFunction
getFunction st = case st.phaseData of
  SurveyingData mf _      -> mf
  SortingData f _ _ _     -> Just f
  SplittingData f _ _ _   -> Just f
  RefiningData f _ _ _ _ _ -> Just f
  DecisionSupportData f _ _ _ _ -> Just f

-- | Get anchors from any phase
getAnchors :: SessionState -> [ItemName]
getAnchors st = case st.phaseData of
  SurveyingData _ a       -> a
  SortingData _ a _ _     -> a
  SplittingData _ a _ _   -> a
  RefiningData _ a _ _ _ _ -> a
  DecisionSupportData _ a _ _ _ -> a

-- | Get current item being discussed (if any)
getCurrentItem :: SessionState -> Maybe ItemName
getCurrentItem st = case st.phaseData of
  SurveyingData _ _       -> Nothing
  SortingData _ _ i _     -> i
  SplittingData _ _ _ _   -> Nothing
  RefiningData _ _ _ _ i _ -> i
  DecisionSupportData _ _ i _ _ -> Just i  -- stuck item is the current item

-- | Get last anxiety trigger (if any)
getLastAnxiety :: SessionState -> Maybe AnxietyTrigger
getLastAnxiety st = case st.phaseData of
  SurveyingData _ _       -> Nothing
  SortingData _ _ _ a     -> a
  SplittingData _ _ _ a   -> a
  RefiningData _ _ _ _ _ a -> a
  DecisionSupportData _ _ _ _ a -> a

-- | Get current category being refined (only valid in Refining phase)
getCurrentCategory :: SessionState -> Maybe CategoryName
getCurrentCategory st = case st.phaseData of
  RefiningData _ _ _ c _ _ -> Just c
  _                       -> Nothing

-- | Get emergent categories (only valid in Refining phase)
getEmergentCats :: SessionState -> Map CategoryName [ItemName]
getEmergentCats st = case st.phaseData of
  RefiningData _ _ cats _ _ _ -> cats
  _                          -> Map.empty

-- ══════════════════════════════════════════════════════════════
-- HELPERS
-- ══════════════════════════════════════════════════════════════

hasFunction :: SessionState -> Bool
hasFunction st = case getFunction st of
  Just (SpaceFunction t) -> not (T.null t)
  Nothing -> False

hasAnchors :: SessionState -> Bool
hasAnchors st = not (null (getAnchors st))

-- | Detect overwhelm signals in user text
isOverwhelmedSignal :: Maybe Text -> Bool
isOverwhelmedSignal Nothing = False
isOverwhelmedSignal (Just t) = any (`T.isInfixOf` T.toLower t) signals
  where
    signals =
      [ "idk"
      , "don't know"
      , "no idea"
      , "where to start"
      , "overwhelm"
      , "too much"
      , "help"
      ]
