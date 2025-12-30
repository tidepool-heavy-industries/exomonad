{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Tidying Mode-Specific Output Types
--
-- Each mode has its own structured output schema.
-- The LLM produces mode-specific fields that update mode data.

module Tidying.Output
  ( -- * Mode-specific outputs
    SurveyingOutput(..)
  , surveyingOutputSchema
  , SortingOutput(..)
  , sortingOutputSchema
  , ClarifyingOutput(..)
  , clarifyingOutputSchema
  , DecisionSupportOutput(..)
  , decisionSupportOutputSchema
  , WindingDownOutput(..)
  , windingDownOutputSchema

    -- * Schema selection
  , schemaForMode

    -- * Photo analysis output
  , PhotoAnalysisOutput(..)
  , photoAnalysisSchema

    -- * Legacy exports (temporary, for compilation)
  , ActOutput(..)
  , actOutputSchema
  ) where

import Data.Aeson
import Data.Aeson.Types (Options(..), defaultOptions, camelTo2)
import Data.Text (Text)
import GHC.Generics (Generic)

import Tidepool.Template (Schema(..))
import Tidying.State (Mode(..))
import Tidying.Types
  ( ItemName(..), ChaosLevel )

-- ══════════════════════════════════════════════════════════════
-- MODE-SPECIFIC OUTPUT TYPES
-- ══════════════════════════════════════════════════════════════

-- | Surveying mode output
--
-- Discovers what the space is FOR and what stays.
data SurveyingOutput = SurveyingOutput
  { svResponse          :: Text
    -- ^ Response to user
  , svDiscoveredFunction :: Maybe Text
    -- ^ What the space is for (if discovered)
  , svDiscoveredAnchors  :: Maybe [Text]
    -- ^ Items that definitely stay (if discovered)
  } deriving (Show, Eq, Generic)

instance FromJSON SurveyingOutput where
  parseJSON = withObject "SurveyingOutput" $ \v -> SurveyingOutput
    <$> v .: "response"
    <*> v .:? "discovered_function"
    <*> v .:? "discovered_anchors"

instance ToJSON SurveyingOutput where
  toJSON SurveyingOutput{..} = object
    [ "response" .= svResponse
    , "discovered_function" .= svDiscoveredFunction
    , "discovered_anchors" .= svDiscoveredAnchors
    ]

surveyingOutputSchema :: Schema SurveyingOutput
surveyingOutputSchema = Schema
  { schemaDescription = "Surveying mode response"
  , schemaJSON = object
      [ "type" .= ("object" :: Text)
      , "additionalProperties" .= False
      , "properties" .= object
          [ "response" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Response to user. Be curious and orienting." :: Text)
              ]
          , "discovered_function" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("What this space is FOR, if user mentioned it (workspace, bedroom, etc.)" :: Text)
              ]
          , "discovered_anchors" .= object
              [ "type" .= ("array" :: Text)
              , "items" .= object ["type" .= ("string" :: Text)]
              , "description" .= ("Items that definitely stay in this space, if user mentioned them" :: Text)
              ]
          ]
      , "required" .= (["response"] :: [Text])
      ]
  }

-- | Sorting mode output
--
-- Processes items quickly, tracks current item context.
data SortingOutput = SortingOutput
  { sortResponse     :: Text
    -- ^ Response to user (terse, directive)
  , sortCurrentItem  :: Maybe Text
    -- ^ Item currently being discussed
  , sortItemLocation :: Maybe Text
    -- ^ Where the item is in the photo (spatial context)
  } deriving (Show, Eq, Generic)

instance FromJSON SortingOutput where
  parseJSON = withObject "SortingOutput" $ \v -> SortingOutput
    <$> v .: "response"
    <*> v .:? "current_item"
    <*> v .:? "item_location"

instance ToJSON SortingOutput where
  toJSON SortingOutput{..} = object
    [ "response" .= sortResponse
    , "current_item" .= sortCurrentItem
    , "item_location" .= sortItemLocation
    ]

sortingOutputSchema :: Schema SortingOutput
sortingOutputSchema = Schema
  { schemaDescription = "Sorting mode response"
  , schemaJSON = object
      [ "type" .= ("object" :: Text)
      , "additionalProperties" .= False
      , "properties" .= object
          [ "response" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Response to user. Be terse and directive - keep them moving." :: Text)
              ]
          , "current_item" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Item you're discussing (name/description)" :: Text)
              ]
          , "item_location" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Where item is in the photo (for reference)" :: Text)
              ]
          ]
      , "required" .= (["response"] :: [Text])
      ]
  }

-- | Clarifying mode output
--
-- Helps user identify an item they can't recognize.
data ClarifyingOutput = ClarifyingOutput
  { clResponse      :: Text
    -- ^ Response to user (patient, descriptive)
  , clDescribingItem :: Maybe Text
    -- ^ Item being described
  , clSpatialRefs    :: Maybe [Text]
    -- ^ Spatial references ("between the desk leg and wall")
  , clPhysicalTraits :: Maybe [Text]
    -- ^ Physical traits ("green", "rectangular", "has wires")
  } deriving (Show, Eq, Generic)

instance FromJSON ClarifyingOutput where
  parseJSON = withObject "ClarifyingOutput" $ \v -> ClarifyingOutput
    <$> v .: "response"
    <*> v .:? "describing_item"
    <*> v .:? "spatial_refs"
    <*> v .:? "physical_traits"

instance ToJSON ClarifyingOutput where
  toJSON ClarifyingOutput{..} = object
    [ "response" .= clResponse
    , "describing_item" .= clDescribingItem
    , "spatial_refs" .= clSpatialRefs
    , "physical_traits" .= clPhysicalTraits
    ]

clarifyingOutputSchema :: Schema ClarifyingOutput
clarifyingOutputSchema = Schema
  { schemaDescription = "Clarifying mode response"
  , schemaJSON = object
      [ "type" .= ("object" :: Text)
      , "additionalProperties" .= False
      , "properties" .= object
          [ "response" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Response to user. Be patient and descriptive - help them identify the item." :: Text)
              ]
          , "describing_item" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("What item you're describing" :: Text)
              ]
          , "spatial_refs" .= object
              [ "type" .= ("array" :: Text)
              , "items" .= object ["type" .= ("string" :: Text)]
              , "description" .= ("Spatial references to help locate ('next to the desk leg', 'under the papers')" :: Text)
              ]
          , "physical_traits" .= object
              [ "type" .= ("array" :: Text)
              , "items" .= object ["type" .= ("string" :: Text)]
              , "description" .= ("Physical traits ('green', 'rectangular', 'has a cord')" :: Text)
              ]
          ]
      , "required" .= (["response"] :: [Text])
      ]
  }

-- | Decision support mode output
--
-- Helps user decide on a stuck item.
data DecisionSupportOutput = DecisionSupportOutput
  { dsResponse        :: Text
    -- ^ Response to user (gentle, reframing)
  , dsStuckItem       :: Maybe Text
    -- ^ Item they're stuck on
  , dsReframeQuestion :: Maybe Text
    -- ^ Reframed question to help decide
  } deriving (Show, Eq, Generic)

instance FromJSON DecisionSupportOutput where
  parseJSON = withObject "DecisionSupportOutput" $ \v -> DecisionSupportOutput
    <$> v .: "response"
    <*> v .:? "stuck_item"
    <*> v .:? "reframe_question"

instance ToJSON DecisionSupportOutput where
  toJSON DecisionSupportOutput{..} = object
    [ "response" .= dsResponse
    , "stuck_item" .= dsStuckItem
    , "reframe_question" .= dsReframeQuestion
    ]

decisionSupportOutputSchema :: Schema DecisionSupportOutput
decisionSupportOutputSchema = Schema
  { schemaDescription = "Decision support mode response"
  , schemaJSON = object
      [ "type" .= ("object" :: Text)
      , "additionalProperties" .= False
      , "properties" .= object
          [ "response" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Response to user. Be gentle and reframe around the space's function." :: Text)
              ]
          , "stuck_item" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Item they're stuck on" :: Text)
              ]
          , "reframe_question" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Reframed question: 'Does this help with [function]?'" :: Text)
              ]
          ]
      , "required" .= (["response"] :: [Text])
      ]
  }

-- | Winding down mode output
--
-- Wraps up the session warmly.
data WindingDownOutput = WindingDownOutput
  { wdoResponse       :: Text
    -- ^ Response to user (warm, factual)
  , wdoSessionSummary :: Maybe Text
    -- ^ Summary of what was accomplished
  , wdoNextTime       :: Maybe [Text]
    -- ^ Suggestions for next session
  } deriving (Show, Eq, Generic)

instance FromJSON WindingDownOutput where
  parseJSON = withObject "WindingDownOutput" $ \v -> WindingDownOutput
    <$> v .: "response"
    <*> v .:? "session_summary"
    <*> v .:? "next_time"

instance ToJSON WindingDownOutput where
  toJSON WindingDownOutput{..} = object
    [ "response" .= wdoResponse
    , "session_summary" .= wdoSessionSummary
    , "next_time" .= wdoNextTime
    ]

windingDownOutputSchema :: Schema WindingDownOutput
windingDownOutputSchema = Schema
  { schemaDescription = "Winding down mode response"
  , schemaJSON = object
      [ "type" .= ("object" :: Text)
      , "additionalProperties" .= False
      , "properties" .= object
          [ "response" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Response to user. Be warm and factual - acknowledge progress." :: Text)
              ]
          , "session_summary" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Brief summary of what was accomplished" :: Text)
              ]
          , "next_time" .= object
              [ "type" .= ("array" :: Text)
              , "items" .= object ["type" .= ("string" :: Text)]
              , "description" .= ("Suggestions for next session (no pressure)" :: Text)
              ]
          ]
      , "required" .= (["response"] :: [Text])
      ]
  }

-- ══════════════════════════════════════════════════════════════
-- SCHEMA SELECTION
-- ══════════════════════════════════════════════════════════════

-- | Select output schema based on mode
schemaForMode :: Mode -> Value
schemaForMode (Surveying _)       = surveyingOutputSchema.schemaJSON
schemaForMode (Sorting _)         = sortingOutputSchema.schemaJSON
schemaForMode (Clarifying _)      = clarifyingOutputSchema.schemaJSON
schemaForMode (DecisionSupport _) = decisionSupportOutputSchema.schemaJSON
schemaForMode (WindingDown _)     = windingDownOutputSchema.schemaJSON

-- ══════════════════════════════════════════════════════════════
-- PHOTO ANALYSIS OUTPUT
-- ══════════════════════════════════════════════════════════════

-- | Output from photo analysis via vision
data PhotoAnalysisOutput = PhotoAnalysisOutput
  { paoRoomType :: Text
    -- ^ "office", "bedroom", "closet", "kitchen", "living_room", "garage", "other"
  , paoChaosLevel :: ChaosLevel
    -- ^ Parsed at JSON boundary (see Types.hs)
  , paoVisibleItems :: [ItemName]
    -- ^ Main visible items in the photo
  , paoBlockedFunction :: Maybe Text
    -- ^ What function is blocked? "can't sit", "can't reach desk"
  , paoFirstTarget :: Maybe ItemName
    -- ^ Best first thing to address
  } deriving (Show, Eq, Generic)

instance FromJSON PhotoAnalysisOutput where
  parseJSON = withObject "PhotoAnalysisOutput" $ \v -> PhotoAnalysisOutput
    <$> v .: "room_type"
    <*> v .: "chaos_level"
    <*> (map ItemName <$> v .: "visible_items")
    <*> v .:? "blocked_function"
    <*> (fmap ItemName <$> v .:? "first_target")

instance ToJSON PhotoAnalysisOutput where
  toJSON PhotoAnalysisOutput{..} = object
    [ "room_type" .= paoRoomType
    , "chaos_level" .= paoChaosLevel
    , "visible_items" .= map (\(ItemName n) -> n) paoVisibleItems
    , "blocked_function" .= paoBlockedFunction
    , "first_target" .= fmap (\(ItemName n) -> n) paoFirstTarget
    ]

photoAnalysisSchema :: Schema PhotoAnalysisOutput
photoAnalysisSchema = Schema
  { schemaDescription = "Analysis of room photo for tidying"
  , schemaJSON = object
      [ "type" .= ("object" :: Text)
      , "additionalProperties" .= False
      , "properties" .= object
          [ "room_type" .= object
              [ "type" .= ("string" :: Text)
              , "enum" .= (["office", "bedroom", "closet", "kitchen", "living_room", "garage", "other"] :: [Text])
              , "description" .= ("What type of space is this?" :: Text)
              ]
          , "chaos_level" .= object
              [ "type" .= ("string" :: Text)
              , "enum" .= (["clear", "moderate", "cluttered", "buried"] :: [Text])
              , "description" .= ("How messy is the space?" :: Text)
              ]
          , "visible_items" .= object
              [ "type" .= ("array" :: Text)
              , "items" .= object ["type" .= ("string" :: Text)]
              , "description" .= ("Main visible items/categories of items (max 10)" :: Text)
              ]
          , "blocked_function" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("What can't be done because of clutter? e.g. 'can't sit', 'can't reach desk'" :: Text)
              ]
          , "first_target" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Best first thing to clear - something quick and impactful" :: Text)
              ]
          ]
      , "required" .= (["room_type", "chaos_level", "visible_items"] :: [Text])
      ]
  }

-- ══════════════════════════════════════════════════════════════
-- LEGACY (temporary, for compilation)
-- ══════════════════════════════════════════════════════════════

-- | Output from ACT phase - response to user (DEPRECATED)
--
-- Kept for compilation during refactor. Will be removed.
data ActOutput = ActOutput
  { aoResponse :: Text
  , aoSuggestedSplits :: [Text]
  } deriving (Show, Eq, Generic)

actJsonOptions :: Options
actJsonOptions = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop 2
  , omitNothingFields = True
  }

instance FromJSON ActOutput where
  parseJSON = withObject "ActOutput" $ \v -> ActOutput
    <$> v .: "response"
    <*> v .:? "suggested_splits" .!= []

instance ToJSON ActOutput where
  toJSON = genericToJSON actJsonOptions

actOutputSchema :: Schema ActOutput
actOutputSchema = Schema
  { schemaDescription = "Response to send to user"
  , schemaJSON = object
      [ "type" .= ("object" :: Text)
      , "additionalProperties" .= False
      , "properties" .= object
          [ "response" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("The message to send to user." :: Text)
              ]
          , "suggested_splits" .= object
              [ "type" .= ("array" :: Text)
              , "items" .= object ["type" .= ("string" :: Text)]
              , "description" .= ("Category names if suggesting a pile split" :: Text)
              ]
          ]
      , "required" .= (["response"] :: [Text])
      ]
  }
