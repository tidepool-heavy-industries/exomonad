{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Context building for Tidying agent
--
-- The context is what gets rendered into the system prompt.
-- It includes the current state and any photo analysis.

module Tidying.Context
  ( -- * Main context type
    TidyingContext(..)
  , buildTidyingContext

    -- * Piles summary
  , PilesSummary(..)

    -- * Photo analysis
  , PhotoAnalysis(..)
  , stubPhotoAnalysis

    -- * Photo conversion
  , photoToImageSource
  ) where

import Data.Aeson (ToJSON(..), (.=), object)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Map.Strict qualified as Map
import GHC.Generics (Generic)

import Tidepool.Anthropic.Http (ImageSource(..))
import Tidying.State

-- | Context for tidying prompts
-- This is what gets rendered into the system prompt template
data TidyingContext = TidyingContext
  { tcPhase :: Phase
    -- ^ Current phase of the session
  , tcFunction :: Maybe Text
    -- ^ What the space is FOR (if known)
  , tcAnchors :: [Text]
    -- ^ Things that definitely stay
  , tcPiles :: PilesSummary
    -- ^ Summary of current piles
  , tcEmergentCategories :: [Text]
    -- ^ Names of emergent categories from splits
  , tcCurrentCategory :: Maybe Text
    -- ^ Category being refined (if any)
  , tcItemsProcessed :: Int
    -- ^ Progress count
  , tcLastAnxiety :: Maybe Text
    -- ^ Thing user was anxious about (to avoid)
  , tcPhotoAnalysis :: Maybe PhotoAnalysis
    -- ^ LLM analysis of photos (if any)
  , tcUserText :: Maybe Text
    -- ^ User's text message (if any)
  } deriving (Show, Eq, Generic)

-- | Simplified pile summary for context
data PilesSummary = PilesSummary
  { psBelongsCount :: Int
  , psOutCount :: Int
  , psUnsureCount :: Int
  , psUnsurePreview :: [Text]  -- first 5 items
  } deriving (Show, Eq, Generic)

-- | Photo analysis result (stubbed)
data PhotoAnalysis = PhotoAnalysis
  { paRoomType :: Text        -- "office", "bedroom", "closet"
  , paChaosLevel :: Text      -- "clear", "moderate", "cluttered", "buried"
  , paVisibleItems :: [Text]  -- what's visible
  , paBlockedFunction :: Maybe Text  -- "can't sit", "can't reach desk"
  , paFirstTarget :: Maybe Text      -- "chair with clothes"
  } deriving (Show, Eq, Generic)

instance ToJSON TidyingContext where
  toJSON TidyingContext{..} = object
    [ "phase" .= show tcPhase
    , "function" .= tcFunction
    , "anchors" .= tcAnchors
    , "piles" .= toJSON tcPiles
    , "emergent_categories" .= tcEmergentCategories
    , "current_category" .= tcCurrentCategory
    , "items_processed" .= tcItemsProcessed
    , "last_anxiety" .= tcLastAnxiety
    , "photo_analysis" .= fmap toJSON tcPhotoAnalysis
    , "user_text" .= tcUserText
    ]

instance ToJSON PilesSummary where
  toJSON PilesSummary{..} = object
    [ "belongs_count" .= psBelongsCount
    , "out_count" .= psOutCount
    , "unsure_count" .= psUnsureCount
    , "unsure_preview" .= psUnsurePreview
    ]

instance ToJSON PhotoAnalysis where
  toJSON PhotoAnalysis{..} = object
    [ "room_type" .= paRoomType
    , "chaos_level" .= paChaosLevel
    , "visible_items" .= paVisibleItems
    , "blocked_function" .= paBlockedFunction
    , "first_target" .= paFirstTarget
    ]

-- | Build context from session state and optional photo analysis
buildTidyingContext
  :: SessionState
  -> Maybe PhotoAnalysis
  -> Maybe Text          -- user text
  -> TidyingContext
buildTidyingContext st mPhotoAnalysis userText = TidyingContext
  { tcPhase = st.phase
  , tcFunction = st.function
  , tcAnchors = st.anchors
  , tcPiles = summarizePiles st.piles
  , tcEmergentCategories = Map.keys st.emergentCats
  , tcCurrentCategory = st.currentCategory
  , tcItemsProcessed = st.itemsProcessed
  , tcLastAnxiety = st.lastAnxiety
  , tcPhotoAnalysis = mPhotoAnalysis
  , tcUserText = userText
  }

-- | Summarize piles for context
summarizePiles :: Piles -> PilesSummary
summarizePiles Piles{..} = PilesSummary
  { psBelongsCount = length belongs
  , psOutCount = length out
  , psUnsureCount = length unsure
  , psUnsurePreview = take 5 unsure
  }

-- | Stub photo analysis for testing
-- Real implementation would call LLM with vision
stubPhotoAnalysis :: [Photo] -> Maybe PhotoAnalysis
stubPhotoAnalysis [] = Nothing
stubPhotoAnalysis _ = Just PhotoAnalysis
  { paRoomType = "office"
  , paChaosLevel = "moderate"
  , paVisibleItems = ["desk", "chair", "papers", "mugs"]
  , paBlockedFunction = Just "chair has clothes on it"
  , paFirstTarget = Just "chair"
  }

-- | Convert Photo to ImageSource for vision API
-- Detects whether data is a URL or base64 encoded
photoToImageSource :: Photo -> ImageSource
photoToImageSource photo
  | "http://" `T.isPrefixOf` photo.photoData = UrlImage photo.photoData
  | "https://" `T.isPrefixOf` photo.photoData = UrlImage photo.photoData
  | otherwise = Base64Image
      { isMediaType = photo.photoMime
      , isData = photo.photoData
      }
