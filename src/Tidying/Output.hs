{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Tidying turn output types
--
-- ORIENT produces a situation classification.
-- ACT produces a response to send to the user.
-- Both are LLM outputs with JSON schemas.

module Tidying.Output
  ( -- * ORIENT output
    OrientOutput(..)
  , orientOutputSchema
  , parseOrientToSituation

    -- * ACT output
  , ActOutput(..)
  , actOutputSchema

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
-- ORIENT OUTPUT
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
  parseJSON = genericParseJSON orientJsonOptions

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
              , "description" .= ("Function of space if user mentioned it" :: Text)
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
