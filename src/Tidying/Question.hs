{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- | Question Tree DSL for Tidying Agent
--
-- The agent emits structured questions, GUI backends interpret them.
-- Inspired by popup-mcp's conditional reveal pattern.
--
-- = Design Principles
--
-- 1. **Agent proposes, user confirms** - First choice is agent's best guess
-- 2. **Graceful fallback** - Text input when agent is wrong
-- 3. **Conditional reveals** - Follow-up questions based on answers
-- 4. **No unsure pile** - Skip items, don't accumulate shame
--
-- = Example
--
-- @
-- ProposeDisposition
--   { pdItem = "the mug on the chair"
--   , pdChoices =
--       [ Choice "Kitchen counter" (PlaceAt "kitchen") Nothing
--       , Choice "Trash" Trash Nothing
--       , Choice "Desk" (PlaceAt "desk") Nothing
--       ]
--   , pdFallback = Just "Where does it actually go?"
--   }
-- @
--
module Tidying.Question
  ( -- * Question Tree
    Question(..)
  , Choice(..)
  , ChoiceOption(..)

    -- * Item Disposition (no Unsure!)
  , ItemDisposition(..)

    -- * Answers
  , Answer(..)

    -- * Conditions
  , Condition(..)

    -- * Smart Constructors
  , proposeItem
  , confirm
  , askFunction
  , askLocation
  ) where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Key (fromText)
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)

-- ══════════════════════════════════════════════════════════════
-- ITEM DISPOSITION
-- ══════════════════════════════════════════════════════════════

-- | Where an item goes
--
-- Note: No "Unsure" option! The unsure pile is an antipattern that
-- accumulates shame. Instead:
--
-- - 'SkipForNow' - item goes back where it was, agent notes for later
-- - 'NeedMoreInfo' - triggers follow-up question to help decide
data ItemDisposition
  = PlaceAt Text        -- ^ Specific location: "kitchen counter", "desk drawer"
  | Trash               -- ^ Garbage
  | Donate              -- ^ Give away
  | Recycle             -- ^ Recycling bin
  | SkipForNow          -- ^ Put it back, come back when warmed up
  | NeedMoreInfo        -- ^ Agent needs more context to help
  deriving (Show, Eq, Generic)

instance ToJSON ItemDisposition where
  toJSON (PlaceAt loc) = object ["placeAt" .= loc]
  toJSON Trash = String "trash"
  toJSON Donate = String "donate"
  toJSON Recycle = String "recycle"
  toJSON SkipForNow = String "skip"
  toJSON NeedMoreInfo = String "needMoreInfo"

instance FromJSON ItemDisposition where
  parseJSON (String "trash") = pure Trash
  parseJSON (String "donate") = pure Donate
  parseJSON (String "recycle") = pure Recycle
  parseJSON (String "skip") = pure SkipForNow
  parseJSON (String "needMoreInfo") = pure NeedMoreInfo
  parseJSON (Object o) = PlaceAt <$> o .: "placeAt"
  parseJSON _ = fail "Invalid ItemDisposition"

-- ══════════════════════════════════════════════════════════════
-- QUESTION TREE
-- ══════════════════════════════════════════════════════════════

-- | A question the agent asks the user
--
-- Questions can have conditional follow-ups via 'choiceReveals' or
-- the 'chReveals' map for option-as-key nesting.
data Question
  -- | Propose item disposition with ranked choices
  -- First choice is agent's best guess (highlighted in UI)
  = ProposeDisposition
      { pdItem :: Text                    -- ^ "the mug on the chair"
      , pdChoices :: [Choice]             -- ^ Ranked, first = recommended
      , pdFallback :: Maybe Text          -- ^ Text input placeholder if all wrong
      }

  -- | Binary confirmation with default
  | Confirm
      { cfPrompt :: Text                  -- ^ "Computer stays?"
      , cfDefault :: Bool                 -- ^ Pre-selected answer
      }

  -- | Multi-choice with optional per-choice follow-ups
  | Choose
      { chPrompt :: Text                  -- ^ "What does this space need to DO?"
      , chId :: Text                      -- ^ For referencing in conditions
      , chChoices :: [ChoiceOption]       -- ^ Available options
      , chReveals :: Map Text [Question]  -- ^ Option-as-key nesting
      }

  -- | Free text input (last resort)
  | FreeText
      { ftPrompt :: Text
      , ftPlaceholder :: Maybe Text
      }

  -- | Group of questions (all visible together)
  | QuestionGroup
      { qgLabel :: Maybe Text
      , qgQuestions :: [Question]
      }

  -- | Conditional visibility
  | ConditionalQ
      { cqWhen :: Condition
      , cqThen :: Question
      }
  deriving (Show, Eq, Generic)

-- | A choice for ProposeDisposition (item classification)
data Choice = Choice
  { choiceLabel :: Text                   -- ^ "Kitchen counter"
  , choiceValue :: ItemDisposition        -- ^ PlaceAt "kitchen"
  , choiceReveals :: Maybe [Question]     -- ^ Follow-up if selected
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | A choice option for Choose (general multi-choice)
data ChoiceOption = ChoiceOption
  { optionLabel :: Text                   -- ^ Display text
  , optionValue :: Text                   -- ^ Value for condition matching
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- Custom JSON for Question to match popup-mcp style
instance ToJSON Question where
  toJSON (ProposeDisposition item choices fallback) = object
    [ "proposeDisposition" .= object
        [ "item" .= item
        , "choices" .= choices
        , "fallback" .= fallback
        ]
    ]
  toJSON (Confirm prompt def) = object
    [ "confirm" .= object
        [ "prompt" .= prompt
        , "default" .= def
        ]
    ]
  toJSON (Choose prompt qid choices reveals) = object $
    [ "choose" .= object
        ([ "prompt" .= prompt
         , "id" .= qid
         , "choices" .= choices
         ] ++ revealEntries)
    ]
    where
      revealEntries = map (\(k, v) -> fromText k .= v) (Map.toList reveals)
  toJSON (FreeText prompt placeholder) = object
    [ "freeText" .= object
        [ "prompt" .= prompt
        , "placeholder" .= placeholder
        ]
    ]
  toJSON (QuestionGroup label qs) = object
    [ "group" .= object
        [ "label" .= label
        , "questions" .= qs
        ]
    ]
  toJSON (ConditionalQ cond q) = object
    [ "when" .= cond
    , "then" .= q
    ]

instance FromJSON Question where
  parseJSON = withObject "Question" $ \o ->
    (parseProposeDisposition o)
    <|> (parseConfirm o)
    <|> (parseChoose o)
    <|> (parseFreeText o)
    <|> (parseGroup o)
    <|> (parseConditional o)
    where
      parseProposeDisposition o = do
        pd <- o .: "proposeDisposition"
        ProposeDisposition
          <$> pd .: "item"
          <*> pd .: "choices"
          <*> pd .:? "fallback"

      parseConfirm o = do
        cf <- o .: "confirm"
        Confirm
          <$> cf .: "prompt"
          <*> cf .:? "default" .!= True

      parseChoose o = do
        ch <- o .: "choose"
        Choose
          <$> ch .: "prompt"
          <*> ch .: "id"
          <*> ch .: "choices"
          <*> pure Map.empty  -- TODO: parse reveals from remaining keys

      parseFreeText o = do
        ft <- o .: "freeText"
        FreeText
          <$> ft .: "prompt"
          <*> ft .:? "placeholder"

      parseGroup o = do
        g <- o .: "group"
        QuestionGroup
          <$> g .:? "label"
          <*> g .: "questions"

      parseConditional o =
        ConditionalQ
          <$> o .: "when"
          <*> o .: "then"

-- ══════════════════════════════════════════════════════════════
-- CONDITIONS
-- ══════════════════════════════════════════════════════════════

-- | Conditions for visibility (simplified from popup-mcp)
data Condition
  = RefEquals Text Text           -- ^ @id == "value"
  | RefTrue Text                  -- ^ @id (truthy check)
  | CondAnd Condition Condition   -- ^ &&
  | CondOr Condition Condition    -- ^ ||
  | CondNot Condition             -- ^ !
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- ══════════════════════════════════════════════════════════════
-- ANSWERS
-- ══════════════════════════════════════════════════════════════

-- | User's answer to a question
data Answer
  = DispositionAnswer ItemDisposition     -- ^ Response to ProposeDisposition
  | ConfirmAnswer Bool                    -- ^ Response to Confirm
  | ChoiceAnswer Text                     -- ^ Response to Choose (option value)
  | TextAnswer Text                       -- ^ Response to FreeText or fallback
  | GroupAnswer (Map Text Answer)         -- ^ Response to QuestionGroup
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- ══════════════════════════════════════════════════════════════
-- SMART CONSTRUCTORS
-- ══════════════════════════════════════════════════════════════

-- | Create item disposition question from agent's analysis
--
-- @
-- proposeItem "mug on chair"
--   [ ("Kitchen counter", PlaceAt "kitchen")
--   , ("Trash", Trash)
--   , ("Desk", PlaceAt "desk")
--   ]
-- @
proposeItem :: Text -> [(Text, ItemDisposition)] -> Question
proposeItem item choices = ProposeDisposition
  { pdItem = item
  , pdChoices = map (\(lbl, val) -> Choice lbl val Nothing) choices
  , pdFallback = Just "Where does it actually go?"
  }

-- | Simple yes/no confirmation
confirm :: Text -> Bool -> Question
confirm prompt def = Confirm prompt def

-- | Ask about space function with common options
askFunction :: Question
askFunction = Choose
  { chPrompt = "What does this space need to DO?"
  , chId = "function"
  , chChoices =
      [ ChoiceOption "Work - sit and focus" "workspace"
      , ChoiceOption "Create - art/music/crafts" "creative"
      , ChoiceOption "Sleep - rest and recover" "bedroom"
      , ChoiceOption "Store - keep things organized" "storage"
      , ChoiceOption "Live - hang out and relax" "living"
      ]
  , chReveals = Map.empty
  }

-- | Ask for specific location with common options
askLocation :: Text -> Question
askLocation item = ProposeDisposition
  { pdItem = item
  , pdChoices =
      [ Choice "Desk" (PlaceAt "desk") Nothing
      , Choice "Shelf" (PlaceAt "shelf") Nothing
      , Choice "Drawer" (PlaceAt "drawer") Nothing
      , Choice "Closet" (PlaceAt "closet") Nothing
      ]
  , pdFallback = Just "Where exactly?"
  }
