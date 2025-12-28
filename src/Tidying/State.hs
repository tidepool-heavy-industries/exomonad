{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tidying.State
  ( -- * Core types
    Phase(..)
  , SessionState(..)
  , newSession

    -- * Piles
  , Piles(..)
  , emptyPiles
  , unsureCount

    -- * User input
  , UserInput(..)
  , Photo(..)

    -- * Helpers
  , hasFunction
  , hasAnchors
  , isOverwhelmedSignal
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- | Session phases
data Phase
  = Surveying        -- ^ Gathering photos, function, anchors
  | Sorting          -- ^ Main sorting loop: belongs/out/unsure
  | Splitting        -- ^ Breaking unsure pile into categories
  | Refining         -- ^ Working through sub-piles
  | DecisionSupport  -- ^ Helping user decide on stuck item
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Pile tracking
data Piles = Piles
  { belongs :: [Text]  -- ^ Items that belong in the space
  , out     :: [Text]  -- ^ Items to remove (trash/donate)
  , unsure  :: [Text]  -- ^ Items not yet classified
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

emptyPiles :: Piles
emptyPiles = Piles [] [] []

unsureCount :: Piles -> Int
unsureCount p = length p.unsure

-- | Main session state
data SessionState = SessionState
  { phase           :: Phase
  , function        :: Maybe Text
    -- ^ What the space is FOR ("designer, needs to work")
  , anchors         :: [Text]
    -- ^ Things that definitely STAY
  , piles           :: Piles
    -- ^ Current pile contents
  , emergentCats    :: Map Text [Text]
    -- ^ Emergent categories from splitting ("cables" -> [items])
  , currentCategory :: Maybe Text
    -- ^ Which sub-category we're refining (if any)
  , itemsProcessed  :: Int
    -- ^ Count for progress tracking
  , lastAnxiety     :: Maybe Text
    -- ^ Thing user expressed anxiety about ("boxes")
  , sessionStart    :: Maybe UTCTime
    -- ^ When session started
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Fresh session
newSession :: SessionState
newSession = SessionState
  { phase = Surveying
  , function = Nothing
  , anchors = []
  , piles = emptyPiles
  , emergentCats = Map.empty
  , currentCategory = Nothing
  , itemsProcessed = 0
  , lastAnxiety = Nothing
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

-- Helpers

hasFunction :: SessionState -> Bool
hasFunction st = maybe False (not . T.null) st.function

hasAnchors :: SessionState -> Bool
hasAnchors st = not (null st.anchors)

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
