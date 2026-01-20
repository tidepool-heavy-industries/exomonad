{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | TUI-interactive MCP tools for human decisions.
--
-- These tools use the TUI effect to show interactive UI elements to the user
-- and wait for their response before returning to the LLM agent.
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
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.:?), (.=), object, withObject, (.!=))
import Data.Text (Text)
import GHC.Generics (Generic)

import Tidepool.Effect.TUI
import Tidepool.Effect.Types (Return, returnValue)
import Tidepool.Graph.Generic (type (:-))
import Tidepool.Graph.Generic.Core (LogicNode)
import Tidepool.Graph.Types (type (:@), Input, UsesEffects, GraphEntries, GraphEntry(..))
import Tidepool.Schema (HasJSONSchema(..), objectSchema, arraySchema, emptySchema, SchemaType(..), describeField)

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
data ConfirmResult = ConfirmResult
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
  '[ "confirm_action" ':~> '("caRun", ConfirmArgs, "Show a confirmation dialog to the user for a potentially destructive or important action") ]

-- | Core logic for confirm_action.
confirmActionLogic
  :: (Member TUI es, Member (Return ConfirmResult) es)
  => ConfirmArgs
  -> Eff es ConfirmResult
confirmActionLogic args = do
  let uiSpec = UISpec
        { uiId = "confirm-action"
        , uiLayout = Vertical
            [ EText "title" "Confirm Action"
            , EText "action" $ "Action: " <> caAction args
            , EText "details" $ caDetails args
            , EButton "confirm" "Confirm"
            , EButton "cancel" "Cancel"
            ]
        }
  
  interaction <- showUI uiSpec
  closeUI
  
  let confirmed = case interaction of
        ButtonClicked _ "confirm" -> True
        _ -> False
  
  returnValue $ ConfirmResult confirmed

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
  '[ "select_option" ':~> '("soRun", SelectArgs, "Ask the user to select from a list of predefined options, or provide a custom response") ]

-- | Core logic for select_option.
selectOptionLogic
  :: (Member TUI es, Member (Return SelectResult) es)
  => SelectArgs
  -> Eff es SelectResult
selectOptionLogic args = do
  let optionButtons = map (\(oid, label) -> EButton oid label) (saOptions args)
      uiSpec = UISpec
        { uiId = "select-option"
        , uiLayout = Vertical $
            [ EText "prompt" (saPrompt args)
            ] ++ optionButtons ++
            [ EInput "custom" "Other / Custom Response" ""
            , EButton "submit_custom" "Submit Custom"
            ]
        }
  
  interaction <- showUI uiSpec
  closeUI
  
  let result = case interaction of
        ButtonClicked _ "submit_custom" ->
          SelectResult "custom" Nothing
        ButtonClicked _ oid ->
          SelectResult oid Nothing
        InputSubmitted _ "custom" val ->
          SelectResult "custom" (Just val)
        _ ->
          SelectResult "cancelled" Nothing
  
  returnValue result

-- ════════════════════════════════════════════════════════════════════════════
-- REQUEST-GUIDANCE TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for request_guidance tool.
data GuidanceArgs = GuidanceArgs
  { gaContext :: Text     -- ^ What the agent is stuck on
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
data GuidanceResult = GuidanceResult
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
  '[ "request_guidance" ':~> '("rgRun", GuidanceArgs, "Ask the user for free-form guidance or to select from suggestions when the agent is stuck") ]

-- | Core logic for request_guidance.
requestGuidanceLogic
  :: (Member TUI es, Member (Return GuidanceResult) es)
  => GuidanceArgs
  -> Eff es GuidanceResult
requestGuidanceLogic args = do
  let suggestionButtons = map (\s -> EButton s s) (gaSuggestions args)
      uiSpec = UISpec
        { uiId = "request-guidance"
        , uiLayout = Vertical $
            [ EText "title" "Request Guidance"
            , EText "context" (gaContext args)
            ] ++ suggestionButtons ++
            [ EInput "guidance" "Your Guidance" ""
            , EButton "submit" "Submit"
            ]
        }
  
  interaction <- showUI uiSpec
  closeUI
  
  let guidance = case interaction of
        ButtonClicked _ "submit" -> "No guidance provided"
        ButtonClicked _ oid -> oid
        InputSubmitted _ "guidance" val -> val
        _ -> "No guidance provided"
  
  returnValue $ GuidanceResult guidance