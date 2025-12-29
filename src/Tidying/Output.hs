{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Tidying turn output types
--
-- ORIENT produces a situation classification.
-- ACT produces a response to send to the user.
-- Both are LLM outputs with JSON schemas.

module Tidying.Output
  ( -- * EXTRACT output (new extraction-first approach)
    Extract(..)
  , Intent(..)
  , Choice(..)
  , extractSchema

    -- * ORIENT output (legacy)
  , OrientOutput(..)
  , orientOutputSchema
  , parseOrientToSituation

    -- * ACT output
  , ActOutput(..)
  , actOutputSchema

    -- * PHOTO ANALYSIS output
  , PhotoAnalysisOutput(..)
  , photoAnalysisSchema

    -- * Apply output to state
  , applyOrientOutput
  , applyActOutput
  ) where

import Data.Aeson
import Data.Aeson.Types (Options(..), defaultOptions, camelTo2)
import Data.Text (Text)
import GHC.Generics (Generic)

import Data.Map.Strict qualified as Map

import Tidepool.Template (Schema(..))
import Tidying.State
import Tidying.Situation
import Tidying.Action

-- ══════════════════════════════════════════════════════════════
-- EXTRACT OUTPUT (new extraction-first approach)
-- ══════════════════════════════════════════════════════════════

-- | User intent - what they want to do right now
data Intent
  = IntentStart      -- ^ Beginning or describing space
  | IntentContinue   -- ^ Ready for next item
  | IntentItem       -- ^ Describing an item
  | IntentDecided    -- ^ Made a decision about item
  | IntentHelp       -- ^ Stuck, needs help
  | IntentStop       -- ^ Done for now
  deriving (Show, Eq, Generic)

instance FromJSON Intent where
  parseJSON = withText "Intent" $ \case
    "start"    -> pure IntentStart
    "continue" -> pure IntentContinue
    "item"     -> pure IntentItem
    "decided"  -> pure IntentDecided
    "help"     -> pure IntentHelp
    "stop"     -> pure IntentStop
    other      -> fail $ "Unknown intent: " <> show other

instance ToJSON Intent where
  toJSON = \case
    IntentStart    -> "start"
    IntentContinue -> "continue"
    IntentItem     -> "item"
    IntentDecided  -> "decided"
    IntentHelp     -> "help"
    IntentStop     -> "stop"

-- | Choice about an item
data Choice
  = ChoiceTrash   -- ^ Throw it away
  | ChoiceKeep    -- ^ Keep it
  | ChoicePlace   -- ^ Put it somewhere specific
  | ChoiceUnsure  -- ^ Not sure yet
  deriving (Show, Eq, Generic)

instance FromJSON Choice where
  parseJSON = withText "Choice" $ \case
    "trash"  -> pure ChoiceTrash
    "keep"   -> pure ChoiceKeep
    "place"  -> pure ChoicePlace
    "unsure" -> pure ChoiceUnsure
    other    -> fail $ "Unknown choice: " <> show other

instance ToJSON Choice where
  toJSON = \case
    ChoiceTrash  -> "trash"
    ChoiceKeep   -> "keep"
    ChoicePlace  -> "place"
    ChoiceUnsure -> "unsure"

-- | Extraction from user input (replaces OrientOutput)
data Extract = Extract
  { exIntent :: Intent
    -- ^ What user wants to do (required)
  , exItem   :: Maybe Text
    -- ^ Item name if mentioned
  , exChoice :: Maybe Choice
    -- ^ Decision about item if made
  , exPlace  :: Maybe Text
    -- ^ Where to put it if choice=place
  } deriving (Show, Eq, Generic)

instance FromJSON Extract where
  parseJSON = withObject "Extract" $ \v -> Extract
    <$> v .:  "intent"
    <*> v .:? "item"
    <*> v .:? "choice"
    <*> v .:? "place"

instance ToJSON Extract where
  toJSON Extract{..} = object $
    [ "intent" .= exIntent ]
    <> maybe [] (\x -> ["item" .= x]) exItem
    <> maybe [] (\x -> ["choice" .= x]) exChoice
    <> maybe [] (\x -> ["place" .= x]) exPlace

-- | Schema for extraction
extractSchema :: Schema Extract
extractSchema = Schema
  { schemaDescription = "Extract information from user message"
  , schemaJSON = object
      [ "type" .= ("object" :: Text)
      , "additionalProperties" .= False
      , "properties" .= object
          [ "intent" .= object
              [ "type" .= ("string" :: Text)
              , "enum" .= (["start", "continue", "item", "decided", "help", "stop"] :: [Text])
              , "description" .= ("start=begin/describe space, continue=next, item=describing something, decided=made choice, help=stuck, stop=done" :: Text)
              ]
          , "item" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Item name if mentioned" :: Text)
              ]
          , "choice" .= object
              [ "type" .= ("string" :: Text)
              , "enum" .= (["trash", "keep", "place", "unsure"] :: [Text])
              , "description" .= ("If decided: what to do with item" :: Text)
              ]
          , "place" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Where to put it if choice=place" :: Text)
              ]
          ]
      , "required" .= (["intent"] :: [Text])
      ]
  }

-- ══════════════════════════════════════════════════════════════
-- ORIENT OUTPUT (legacy)
-- ══════════════════════════════════════════════════════════════

-- | Output from ORIENT phase - LLM classifies the situation
data OrientOutput = OrientOutput
  { ooSituation :: Text
    -- ^ One of: need_function, need_anchors, overwhelmed,
    --   item_trash, item_belongs, item_unsure, item_stuck,
    --   action_done, unsure_growing, main_done,
    --   anxious, flagging, wants_stop, wants_continue, all_done
  , ooItemName :: Maybe Text
    -- ^ For item classifications, the item name
  , ooItemLocation :: Maybe Text
    -- ^ For "belongs" classification, where it goes
  , ooAnxietyTrigger :: Maybe Text
    -- ^ For "anxious" classification, what triggered it
  , ooFunctionExtracted :: Maybe Text
    -- ^ If user provided function, extract it
  , ooAnchorsExtracted :: [Text]
    -- ^ If user mentioned anchors, extract them
  } deriving (Show, Eq, Generic)

-- | JSON options: ooItemName -> item_name
orientJsonOptions :: Options
orientJsonOptions = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop 2  -- drop "oo" prefix
  , omitNothingFields = True
  }

instance FromJSON OrientOutput where
  parseJSON = withObject "OrientOutput" $ \v -> OrientOutput
    <$> v .: "situation"
    <*> v .:? "item_name"
    <*> v .:? "item_location"
    <*> v .:? "anxiety_trigger"
    <*> v .:? "function_extracted"
    <*> v .:? "anchors_extracted" .!= []  -- Default to empty list

instance ToJSON OrientOutput where
  toJSON = genericToJSON orientJsonOptions

-- | Parse OrientOutput into Situation
parseOrientToSituation :: OrientOutput -> Situation
parseOrientToSituation OrientOutput{..} = case ooSituation of
  "need_function" -> NeedFunction
  "need_anchors" -> NeedAnchors
  "overwhelmed" -> OverwhelmedNeedMomentum
  "item_trash" -> ItemDescribed (maybe "unknown" id ooItemName) Trash
  "item_belongs" -> ItemDescribed (maybe "unknown" id ooItemName)
                                   (Belongs (maybe "its place" id ooItemLocation))
  "item_unsure" -> ItemDescribed (maybe "unknown" id ooItemName) Unsure
  "item_stuck" -> ItemDescribed (maybe "unknown" id ooItemName) NeedsDecisionSupport
  "action_done" -> ActionDone
  "unsure_growing" -> UnsureGrowing
  "main_done" -> MainPileDone
  "anxious" -> Anxious (maybe "something" id ooAnxietyTrigger)
  "flagging" -> Flagging
  "wants_stop" -> WantsToStop
  "wants_continue" -> WantsToContinue
  "all_done" -> AllDone
  _ -> ActionDone  -- fallback

orientOutputSchema :: Schema OrientOutput
orientOutputSchema = Schema
  { schemaDescription = "Situation classification from user input"
  , schemaJSON = object
      [ "type" .= ("object" :: Text)
      , "additionalProperties" .= False
      , "properties" .= object
          [ "situation" .= object
              [ "type" .= ("string" :: Text)
              , "enum" .= ([ "need_function", "need_anchors", "overwhelmed"
                           , "item_trash", "item_belongs", "item_unsure", "item_stuck"
                           , "action_done", "unsure_growing", "main_done"
                           , "anxious", "flagging", "wants_stop", "wants_continue", "all_done"
                           ] :: [Text])
              , "description" .= ("What situation is the user in?" :: Text)
              ]
          , "item_name" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Name of item if classifying an item" :: Text)
              ]
          , "item_location" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Where item belongs if situation is item_belongs" :: Text)
              ]
          , "anxiety_trigger" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("What triggered anxiety if situation is anxious" :: Text)
              ]
          , "function_extracted" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Function of space - what it's FOR. Extract from descriptions like 'computer desk' → 'computer work', 'home office' → 'office work', 'where I sleep' → 'sleeping'. Always extract if user describes the space's purpose." :: Text)
              ]
          , "anchors_extracted" .= object
              [ "type" .= ("array" :: Text)
              , "items" .= object ["type" .= ("string" :: Text)]
              , "description" .= ("Anchors user mentioned (things that stay)" :: Text)
              ]
          ]
      , "required" .= (["situation"] :: [Text])
      ]
  }

-- | Apply orient output to state (extract function/anchors)
applyOrientOutput :: OrientOutput -> SessionState -> SessionState
applyOrientOutput OrientOutput{..} st = st
  { function = ooFunctionExtracted <|> st.function
  , anchors = ooAnchorsExtracted <> st.anchors
  , lastAnxiety = ooAnxietyTrigger <|> st.lastAnxiety
  }
  where
    (<|>) :: Maybe a -> Maybe a -> Maybe a
    (<|>) (Just x) _ = Just x
    (<|>) Nothing y = y

-- ══════════════════════════════════════════════════════════════
-- ACT OUTPUT
-- ══════════════════════════════════════════════════════════════

-- | Output from ACT phase - response to user
data ActOutput = ActOutput
  { aoResponse :: Text
    -- ^ The message to send to user
  , aoSuggestedSplits :: [Text]
    -- ^ For split instructions, suggested category names
  } deriving (Show, Eq, Generic)

-- | JSON options: aoResponse -> response
actJsonOptions :: Options
actJsonOptions = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop 2  -- drop "ao" prefix
  , omitNothingFields = True
  }

instance FromJSON ActOutput where
  parseJSON = genericParseJSON actJsonOptions

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
              , "description" .= ("The message to send to user. Keep it terse and directive." :: Text)
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

-- | Apply act output to state (minimal - mostly just for splits)
applyActOutput :: ActOutput -> Action -> SessionState -> SessionState
applyActOutput ActOutput{..} action st = case action of
  InstructSplit _ -> st { emergentCats = foldr addCat st.emergentCats aoSuggestedSplits }
  _ -> st
  where
    addCat name cats = Map.insertWith (++) name [] cats

-- ══════════════════════════════════════════════════════════════
-- PHOTO ANALYSIS OUTPUT
-- ══════════════════════════════════════════════════════════════

-- | Output from photo analysis via vision
data PhotoAnalysisOutput = PhotoAnalysisOutput
  { paoRoomType :: Text
    -- ^ "office", "bedroom", "closet", "kitchen", "living_room", "garage", "other"
  , paoChaosLevel :: Text
    -- ^ "clear", "moderate", "cluttered", "buried"
  , paoVisibleItems :: [Text]
    -- ^ Main visible items in the photo
  , paoBlockedFunction :: Maybe Text
    -- ^ What function is blocked? "can't sit", "can't reach desk"
  , paoFirstTarget :: Maybe Text
    -- ^ Best first thing to address
  } deriving (Show, Eq, Generic)

-- | JSON options: paoRoomType -> room_type
photoAnalysisJsonOptions :: Options
photoAnalysisJsonOptions = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop 3  -- drop "pao" prefix
  , omitNothingFields = True
  }

instance FromJSON PhotoAnalysisOutput where
  parseJSON = genericParseJSON photoAnalysisJsonOptions

instance ToJSON PhotoAnalysisOutput where
  toJSON = genericToJSON photoAnalysisJsonOptions

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
