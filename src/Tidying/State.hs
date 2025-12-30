{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Session state for the Tidying agent.
--
-- Key insight: Mode is a SUM TYPE with data, not an enum.
-- Each mode carries its own context/data fields.
--
-- The LLM navigates between modes via transition tools.
-- Haskell only structures the available tools/templates/schemas per mode.
module Tidying.State
  ( -- * Mode (sum type with mode-specific data)
    Mode(..)
  , SurveyingData(..)
  , SortingData(..)
  , ClarifyingData(..)
  , DecisionSupportData(..)
  , WindingDownData(..)

    -- * Session state
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
  , modeName
  ) where

import Data.Aeson (ToJSON(..), FromJSON, object, (.=))
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

import Tidying.Types
  ( ItemName, SpaceFunction )

-- ══════════════════════════════════════════════════════════════
-- MODE (sum type with mode-specific data)
-- ══════════════════════════════════════════════════════════════

-- | Mode = sum type with mode-specific data
--
-- Each mode has its own:
--   - Data fields (carried in the constructor)
--   - Jinja template (selected by templateForMode)
--   - Tool set (selected by toolsForMode)
--   - Output schema (selected by schemaForMode)
--
-- The LLM navigates between modes via transition tools.
-- When a transition tool is called:
--   1. Tool args become the NEW mode's initial data
--   2. ToolBreak ends the current turn
--   3. Synthetic message injected
--   4. New turn starts with new mode's template/tools
data Mode
  = Surveying SurveyingData
    -- ^ Curious, orienting - "What is this space?"
  | Sorting SortingData
    -- ^ Terse, directive - "Keep moving. Next."
  | Clarifying ClarifyingData
    -- ^ Patient, descriptive - "Let me show you..."
  | DecisionSupport DecisionSupportData
    -- ^ Gentle, reframing - "What does this space need?"
  | WindingDown WindingDownData
    -- ^ Warm, factual - "Good stopping point."
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Surveying mode data
--
-- Empty because discoveries (function, anchors) go to session-level fields.
-- This keeps them persistent across mode transitions.
data SurveyingData = SurveyingData
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Sorting mode data
--
-- Tracks the current item being discussed.
data SortingData = SortingData
  { sdCurrentItem  :: Maybe Text
    -- ^ Item currently being discussed (name/description)
  , sdItemLocation :: Maybe Text
    -- ^ Where the item is in the photo (spatial context)
  } deriving (Eq, Show, Generic, FromJSON)

-- Custom ToJSON with snake_case field names for Jinja templates
instance ToJSON SortingData where
  toJSON SortingData{..} = object
    [ "current_item" .= sdCurrentItem
    , "item_location" .= sdItemLocation
    ]

-- | Clarifying mode data
--
-- When user can't identify an item, the LLM calls need_to_clarify
-- which creates this data from the tool's arguments.
data ClarifyingData = ClarifyingData
  { cdItem         :: Text
    -- ^ What item we're clarifying
  , cdPhotoContext :: Text
    -- ^ What we observed in the photo (location, nearby objects)
  , cdReason       :: Text
    -- ^ Why user is confused (their response)
  } deriving (Eq, Show, Generic, FromJSON)

-- Custom ToJSON with snake_case field names for Jinja templates
instance ToJSON ClarifyingData where
  toJSON ClarifyingData{..} = object
    [ "item" .= cdItem
    , "photo_context" .= cdPhotoContext
    , "reason" .= cdReason
    ]

-- | Decision support mode data
--
-- When user seems stuck on an item, the LLM calls user_seems_stuck
-- which creates this data from the tool's arguments.
data DecisionSupportData = DecisionSupportData
  { dsdStuckItem :: Text
    -- ^ Which item they're stuck on
  } deriving (Eq, Show, Generic, FromJSON)

-- Custom ToJSON with snake_case field names for Jinja templates
instance ToJSON DecisionSupportData where
  toJSON DecisionSupportData{..} = object
    [ "stuck_item" .= dsdStuckItem
    ]

-- | Winding down mode data
--
-- When LLM detects it's time to wrap up, it calls time_to_wrap
-- which transitions to this mode.
data WindingDownData = WindingDownData
  { wdSessionSummary :: Maybe Text
    -- ^ Summary of what was accomplished
  , wdNextTime       :: [Text]
    -- ^ Suggestions for next session
  } deriving (Eq, Show, Generic, FromJSON)

-- Custom ToJSON with snake_case field names for Jinja templates
instance ToJSON WindingDownData where
  toJSON WindingDownData{..} = object
    [ "session_summary" .= wdSessionSummary
    , "next_time" .= wdNextTime
    ]

-- ══════════════════════════════════════════════════════════════
-- SESSION STATE
-- ══════════════════════════════════════════════════════════════

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
-- Mode carries mode-specific data (sum type).
-- Session-level fields persist across mode transitions.
data SessionState = SessionState
  { mode           :: Mode
    -- ^ Current mode (sum type with mode-specific data)
  , piles          :: Piles
    -- ^ Current pile contents
  , itemsProcessed :: Int
    -- ^ Count for progress tracking
  , sessionStart   :: Maybe UTCTime
    -- ^ When session started
  -- Session-level fields (persist across modes):
  , spaceFunction  :: Maybe SpaceFunction
    -- ^ What this space is FOR (discovered in Surveying)
  , anchors        :: [ItemName]
    -- ^ Things that definitely stay (discovered in Surveying)
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Fresh session starts in Surveying mode
newSession :: SessionState
newSession = SessionState
  { mode = Surveying SurveyingData
  , piles = emptyPiles
  , itemsProcessed = 0
  , sessionStart = Nothing
  , spaceFunction = Nothing
  , anchors = []
  }

-- ══════════════════════════════════════════════════════════════
-- USER INPUT
-- ══════════════════════════════════════════════════════════════

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
-- HELPERS
-- ══════════════════════════════════════════════════════════════

-- | Get mode name for display/debugging
modeName :: Mode -> Text
modeName (Surveying _)       = "Surveying"
modeName (Sorting _)         = "Sorting"
modeName (Clarifying _)      = "Clarifying"
modeName (DecisionSupport _) = "DecisionSupport"
modeName (WindingDown _)     = "WindingDown"
