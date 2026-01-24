{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | TUI-interactive MCP tools for human decisions.
--
-- These tools use the TUI effect to show interactive popups and wait for
-- user response before returning to the LLM agent.
module Tidepool.Control.TUITools
  ( -- * Confirm Action
    ConfirmActionGraph(..)
  , confirmActionLogic
  , ConfirmArgs(..)
  , ConfirmResult(..)

    -- * Select Option
  , SelectOptionGraph(..)
  , selectOptionLogic
  , SelectArgs(..)
  , SelectResult(..)

    -- * Request Guidance
  , RequestGuidanceGraph(..)
  , requestGuidanceLogic
  , GuidanceArgs(..)
  , GuidanceResult(..)
  ) where

import Control.Monad.Freer (Eff, Member)
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), (.:), (.:?), (.=), object, withObject, (.!=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Text (Text)
import GHC.Generics (Generic)

import Tidepool.Effect.TUI
import Tidepool.Effect.Types (Return, returnValue)
import Tidepool.Role (Role(..))
import Tidepool.Graph.Generic (type (:-))
import Tidepool.Graph.Generic.Core (LogicNode)
import Tidepool.Graph.Types (type (:@), Input, UsesEffects, GraphEntries, GraphEntry(..))
import Tidepool.Schema (HasJSONSchema(..), objectSchema, arraySchema, emptySchema, SchemaType(..), describeField)

-- ════════════════════════════════════════════════════════════════════════════
-- JSON VALUE HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Extract a text value from a JSON object by key, or empty string if missing.
getText :: Text -> Value -> Text
getText key = \case
  Object obj -> case KM.lookup (Key.fromText key) obj of
    Just (String t) -> t
    _ -> ""
  _ -> ""

-- | Extract a non-empty text value from a JSON object.
getTextMaybe :: Text -> Value -> Maybe Text
getTextMaybe key val =
  case getText key val of
    "" -> Nothing
    t -> Just t

-- ════════════════════════════════════════════════════════════════════════════
-- CONFIRM-ACTION TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for confirm_action tool.
data ConfirmArgs = ConfirmArgs
  { caAction :: Text      -- ^ High-level description of the action
  , caDetails :: Text     -- ^ Additional context or specific details
  }
  deriving stock (Show, Eq, Generic)

instance HasJSONSchema ConfirmArgs where
  jsonSchema = objectSchema
    [ ("action", describeField "action" "High-level description of the action (e.g. 'Delete 15 files')" (emptySchema TString))
    , ("details", describeField "details" "Additional context or specific details to help the user decide" (emptySchema TString))
    ]
    ["action", "details"]

instance FromJSON ConfirmArgs where
  parseJSON = withObject "ConfirmArgs" $ \v ->
    ConfirmArgs <$> v .: "action" <*> v .: "details"

instance ToJSON ConfirmArgs where
  toJSON args = object
    [ "action" .= caAction args
    , "details" .= caDetails args
    ]

-- | Result of confirm_action tool.
newtype ConfirmResult = ConfirmResult
  { crConfirmed :: Bool
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ConfirmResult where
  toJSON res = object ["confirmed" .= crConfirmed res]

-- | Graph definition for confirm_action tool.
newtype ConfirmActionGraph mode = ConfirmActionGraph
  { caRun :: mode :- LogicNode
      :@ Input ConfirmArgs
      :@ UsesEffects '[TUI, Return ConfirmResult]
  }
  deriving Generic

-- | MCP tool entry point declaration for confirm_action.
type instance GraphEntries ConfirmActionGraph =
  '[ "confirm_action" ':~> '("caRun", ConfirmArgs, "Show a confirmation dialog to the user for a potentially destructive or important action", '[ 'Dev, 'TL, 'PM]) ]

-- | Core logic for confirm_action.
confirmActionLogic
  :: (Member TUI es, Member (Return ConfirmResult) es)
  => ConfirmArgs
  -> Eff es ConfirmResult
confirmActionLogic args = do
  let popup = PopupDefinition
        { pdTitle = "Confirm Action"
        , pdComponents =
            [ Component "action" (Text $ "Action: " <> caAction args) Nothing
            , Component "details" (Text $ caDetails args) Nothing
            ]
        }

  PopupResult button _ <- showUI popup
  returnValue $ ConfirmResult (button == "submit")

-- ════════════════════════════════════════════════════════════════════════════
-- SELECT-OPTION TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for select_option tool.
data SelectArgs = SelectArgs
  { saPrompt :: Text
  , saOptions :: [(Text, Text)]  -- ^ (id, label) pairs
  }
  deriving stock (Show, Eq, Generic)

instance HasJSONSchema SelectArgs where
  jsonSchema = objectSchema
    [ ("prompt", describeField "prompt" "Instruction for the user" (emptySchema TString))
    , ("options", describeField "options" "List of options as [id, label] pairs"
        (arraySchema $ arraySchema $ emptySchema TString))
    ]
    ["prompt", "options"]

instance FromJSON SelectArgs where
  parseJSON = withObject "SelectArgs" $ \v ->
    SelectArgs <$> v .: "prompt" <*> v .: "options"

instance ToJSON SelectArgs where
  toJSON args = object
    [ "prompt" .= saPrompt args
    , "options" .= saOptions args
    ]

-- | Result of select_option tool.
data SelectResult = SelectResult
  { srSelected :: Text
  , srCustom :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON SelectResult where
  toJSON res = object
    [ "selected" .= srSelected res
    , "custom" .= srCustom res
    ]

-- | Graph definition for select_option tool.
newtype SelectOptionGraph mode = SelectOptionGraph
  { soRun :: mode :- LogicNode
      :@ Input SelectArgs
      :@ UsesEffects '[TUI, Return SelectResult]
  }
  deriving Generic

-- | MCP tool entry point declaration for select_option.
type instance GraphEntries SelectOptionGraph =
  '[ "select_option" ':~> '("soRun", SelectArgs, "Ask the user to select from a list of predefined options, or provide a custom response", '[ 'Dev, 'TL, 'PM]) ]

-- | Core logic for select_option.
selectOptionLogic
  :: (Member TUI es, Member (Return SelectResult) es)
  => SelectArgs
  -> Eff es SelectResult
selectOptionLogic args = do
  let labels = map snd (saOptions args)
      popup = PopupDefinition
        { pdTitle = "Select Option"
        , pdComponents =
            [ Component "prompt" (Text $ saPrompt args) Nothing
            , Component "choice" (Choice "Choose an option" labels (Just 0)) Nothing
            , Component "custom" (Textbox "Other / Custom Response" Nothing Nothing) Nothing
            ]
        }

  PopupResult button values <- showUI popup

  let choiceText = getText "choice" values
      customText = getTextMaybe "custom" values
      selected
        | button == "decline" = "cancelled"
        | Just _ <- customText = "custom"
        | otherwise = choiceText

  returnValue $ SelectResult selected customText

-- ════════════════════════════════════════════════════════════════════════════
-- REQUEST-GUIDANCE TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for request_guidance tool.
data GuidanceArgs = GuidanceArgs
  { gaContext :: Text        -- ^ What the agent is stuck on
  , gaSuggestions :: [Text]  -- ^ Optional suggestions
  }
  deriving stock (Show, Eq, Generic)

instance HasJSONSchema GuidanceArgs where
  jsonSchema = objectSchema
    [ ("context", describeField "context" "Description of what the agent needs help with" (emptySchema TString))
    , ("suggestions", describeField "suggestions" "Optional suggested directions for the user to choose from"
        (arraySchema $ emptySchema TString))
    ]
    ["context"]

instance FromJSON GuidanceArgs where
  parseJSON = withObject "GuidanceArgs" $ \v ->
    GuidanceArgs <$> v .: "context" <*> v .:? "suggestions" .!= []

instance ToJSON GuidanceArgs where
  toJSON args = object
    [ "context" .= gaContext args
    , "suggestions" .= gaSuggestions args
    ]

-- | Result of request_guidance tool.
newtype GuidanceResult = GuidanceResult
  { grGuidance :: Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON GuidanceResult where
  toJSON res = object ["guidance" .= grGuidance res]

-- | Graph definition for request_guidance tool.
newtype RequestGuidanceGraph mode = RequestGuidanceGraph
  { rgRun :: mode :- LogicNode
      :@ Input GuidanceArgs
      :@ UsesEffects '[TUI, Return GuidanceResult]
  }
  deriving Generic

-- | MCP tool entry point declaration for request_guidance.
type instance GraphEntries RequestGuidanceGraph =
  '[ "request_guidance" ':~> '("rgRun", GuidanceArgs, "Ask the user for free-form guidance or to select from suggestions when the agent is stuck", '[ 'Dev, 'TL, 'PM]) ]

-- | Core logic for request_guidance.
requestGuidanceLogic
  :: (Member TUI es, Member (Return GuidanceResult) es)
  => GuidanceArgs
  -> Eff es GuidanceResult
requestGuidanceLogic args = do
  let suggestionComponent
        | null (gaSuggestions args) = []
        | otherwise = [Component "suggestions" (Choice "Suggestions" (gaSuggestions args) (Just 0)) Nothing]

      popup = PopupDefinition
        { pdTitle = "Request Guidance"
        , pdComponents =
            [ Component "context" (Text $ gaContext args) Nothing ]
            <> suggestionComponent <>
            [ Component "guidance" (Textbox "Your Guidance" Nothing Nothing) Nothing ]
        }

  PopupResult _ values <- showUI popup

  let guidance = case getTextMaybe "guidance" values of
        Just t -> t
        Nothing -> case getTextMaybe "suggestions" values of
          Just t -> t
          Nothing -> "No guidance provided"

  returnValue $ GuidanceResult guidance
