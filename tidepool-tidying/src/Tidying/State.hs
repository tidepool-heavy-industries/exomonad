{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tidying.State
  ( -- * Core types
    Phase(..)
  , PhaseData(..)
  , ActiveState(..)
  , SessionState(..)
  , newSession

    -- * Phase (derived from PhaseData, not stored)
  , phase

    -- * Piles
  , Piles(..)
  , emptyPiles
  , unsureCount

    -- * User input
  , UserInput(..)
  , Photo(..)

    -- * Accessors for phase-specific data
  , getActiveState
  , getFunction
  , getAnchors
  , getCurrentItem
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
  ( ItemName, SpaceFunction(..), CategoryName )

-- | Common state present after surveying completes
--
-- Every phase except Surveying has these fields.
-- Factored out to eliminate duplication.
data ActiveState = ActiveState
  { asFunction :: SpaceFunction
    -- ^ What this space is FOR (required post-surveying)
  , asAnchors  :: [ItemName]
    -- ^ Things that definitely stay
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

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
--
-- Every phase except Surveying has an 'ActiveState' containing
-- the common fields (function, anchors).
data PhaseData
  = SurveyingData
      { sdGatheredFunction :: Maybe SpaceFunction
        -- ^ Function being gathered (Nothing until user provides it)
      , sdGatheredAnchors  :: [ItemName]
        -- ^ Anchors gathered so far
      }
  | SortingData
      { soActive      :: ActiveState
        -- ^ Common state (function, anchors)
      , soCurrentItem :: Maybe ItemName
        -- ^ Item currently being discussed
      }
  | SplittingData
      { spActive     :: ActiveState
        -- ^ Common state (function, anchors)
      , spCategories :: NonEmpty CategoryName
        -- ^ Categories to split into (NonEmpty - must have at least one)
      }
  | RefiningData
      { rfActive          :: ActiveState
        -- ^ Common state (function, anchors)
      , rfEmergentCats    :: Map CategoryName [ItemName]
        -- ^ Categories and their items
      , rfCurrentCategory :: CategoryName
        -- ^ Required! Can't be refining without a category
      , rfCurrentItem     :: Maybe ItemName
      }
  | DecisionSupportData
      { dsActive      :: ActiveState
        -- ^ Common state (function, anchors)
      , dsStuckItem   :: ItemName
        -- ^ Required! Can't be in decision support without a stuck item
      , dsReturnPhase :: Phase
        -- ^ Which phase to return to after decision
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
--
-- Note: The 'phase' is not stored; it's derived from 'phaseData'.
-- Use the 'phase' function to get the current phase.
data SessionState = SessionState
  { phaseData      :: PhaseData
    -- ^ Phase-specific data (phase is derived from this)
  , piles          :: Piles
    -- ^ Current pile contents
  , itemsProcessed :: Int
    -- ^ Count for progress tracking
  , sessionStart   :: Maybe UTCTime
    -- ^ When session started
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Derive phase from PhaseData (not stored, computed)
--
-- This ensures phase and phaseData can never be inconsistent.
phase :: SessionState -> Phase
phase st = case st.phaseData of
  SurveyingData {}        -> Surveying
  SortingData {}          -> Sorting
  SplittingData {}        -> Splitting
  RefiningData {}         -> Refining
  DecisionSupportData {}  -> DecisionSupport

-- | Fresh session
newSession :: SessionState
newSession = SessionState
  { phaseData = SurveyingData Nothing []
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

-- | Get ActiveState from any post-surveying phase
--
-- Returns Nothing only during Surveying.
getActiveState :: SessionState -> Maybe ActiveState
getActiveState st = case st.phaseData of
  SurveyingData {}        -> Nothing
  SortingData a _         -> Just a
  SplittingData a _       -> Just a
  RefiningData a _ _ _    -> Just a
  DecisionSupportData a _ _ -> Just a

-- | Get function from any phase (Nothing only in Surveying before set)
getFunction :: SessionState -> Maybe SpaceFunction
getFunction st = case st.phaseData of
  SurveyingData mf _ -> mf
  _                  -> (.asFunction) <$> getActiveState st

-- | Get anchors from any phase
getAnchors :: SessionState -> [ItemName]
getAnchors st = case st.phaseData of
  SurveyingData _ a -> a
  _                 -> maybe [] (.asAnchors) (getActiveState st)

-- | Get current item being discussed (if any)
getCurrentItem :: SessionState -> Maybe ItemName
getCurrentItem st = case st.phaseData of
  SurveyingData {}        -> Nothing
  SortingData _ i         -> i
  SplittingData {}        -> Nothing
  RefiningData _ _ _ i    -> i
  DecisionSupportData _ i _ -> Just i  -- stuck item is the current item

-- | Get current category being refined (only valid in Refining phase)
getCurrentCategory :: SessionState -> Maybe CategoryName
getCurrentCategory st = case st.phaseData of
  RefiningData _ _ c _ -> Just c
  _                    -> Nothing

-- | Get emergent categories (only valid in Refining phase)
getEmergentCats :: SessionState -> Map CategoryName [ItemName]
getEmergentCats st = case st.phaseData of
  RefiningData _ cats _ _ -> cats
  _                       -> Map.empty

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
