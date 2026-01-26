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

-- | General-purpose popup MCP tool.
--
-- This module provides a single 'popup' tool that replaces the previous
-- specialized TUI tools (confirm_action, select_option, request_guidance).
-- The popup tool accepts a flat list of UI elements and returns a flat
-- list with values filled in.
--
-- == Element Types
--
-- | Type | Input Fields | Output Adds |
-- |------|--------------|-------------|
-- | text | content | (unchanged) |
-- | slider | label, min, max, default? | value: number |
-- | checkbox | label, default? | value: bool |
-- | textbox | label, placeholder? | value: string |
-- | choice | label, options, default? | value: string (selected option) |
-- | multiselect | label, options | value: [string] (selected options) |
-- | group | label | (unchanged, visual separator) |
--
-- == Example Usage
--
-- @
-- result <- popup PopupArgs
--   { paTitle = Just "Configure Exploration"
--   , paElements =
--       [ Slider "Budget" 10 100 (Just 50)
--       , Checkbox "Include tests" (Just False)
--       , Choice "Method" ["BFS", "DFS", "Random"] (Just 0)
--       ]
--   }
--
-- case prStatus result of
--   "completed" -> ... -- user pressed submit
--   "cancelled" -> ... -- user pressed cancel
-- @
module Tidepool.Control.TUITools
  ( -- * Popup Tool
    PopupGraph(..)
  , popupLogic
  , PopupArgs(..)
  , PopupResult(..)

    -- * Element Types
  , PopupElement(..)
  , PopupResultElement(..)
  ) where

import Control.Monad.Freer (Eff, Member)
import Data.Aeson
  ( FromJSON(..), ToJSON(..), Value(..), (.:), (.:?), (.=)
  , object, withObject
  )
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Tidepool.Effect.TUI
  ( TUI, showUI, PopupDefinition(..), Component(..), ComponentSpec(..)
  )
import qualified Tidepool.Effect.TUI as TUI (PopupResult(..))
import Tidepool.Effect.Types (Return, returnValue)
import Tidepool.Role (Role(..))
import Tidepool.Graph.Generic (type (:-))
import Tidepool.Graph.Generic.Core (LogicNode)
import Tidepool.Graph.Types (type (:@), Input, UsesEffects, GraphEntries, GraphEntry(..))
import Tidepool.Schema (HasJSONSchema(..), JSONSchema, objectSchema, arraySchema, oneOfSchema, emptySchema, SchemaType(..), describeField, enumSchema)

-- ════════════════════════════════════════════════════════════════════════════
-- POPUP ELEMENT TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Input element specification (what Claude sends).
--
-- Each variant has an id and type-specific fields.
-- The JSON serialization uses a "type" tag with lowercase variant names.
data PopupElement
  = PText Text Text
    -- ^ Static text display (id, content)
  | PSlider Text Text Double Double (Maybe Double)
    -- ^ Numeric slider (id, label, min, max, default)
  | PCheckbox Text Text (Maybe Bool)
    -- ^ Boolean checkbox (id, label, default)
  | PTextbox Text Text (Maybe Text)
    -- ^ Text input (id, label, placeholder)
  | PChoice Text Text [Text] (Maybe Int)
    -- ^ Single-select dropdown (id, label, options, default index)
  | PMultiselect Text Text [Text]
    -- ^ Multiple selection list (id, label, options)
  | PGroup Text Text
    -- ^ Section header (id, label)
  deriving stock (Show, Eq, Generic)

-- | Output element with value (what we return).
--
-- Same structure as input but with value instead of default.
data PopupResultElement
  = RText Text Text
    -- ^ Static text (id, content)
  | RSlider Text Text Double
    -- ^ Slider result (id, label, value)
  | RCheckbox Text Text Bool
    -- ^ Checkbox result (id, label, value)
  | RTextbox Text Text Text
    -- ^ Textbox result (id, label, value)
  | RChoice Text Text Text
    -- ^ Choice result (id, label, selected option text)
  | RMultiselect Text Text [Text]
    -- ^ Multiselect result (id, label, selected option texts)
  | RGroup Text Text
    -- ^ Group header (id, label)
  deriving stock (Show, Eq, Generic)

-- ════════════════════════════════════════════════════════════════════════════
-- POPUP ARGS AND RESULT
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for the popup tool.
data PopupArgs = PopupArgs
  { paTitle :: Maybe Text
    -- ^ Optional popup title
  , paElements :: [PopupElement]
    -- ^ Flat list of UI elements
  }
  deriving stock (Show, Eq, Generic)

-- | Result of the popup tool.
data PopupResult = PopupResult
  { prStatus :: Text
    -- ^ "completed" or "cancelled"
  , prButton :: Text
    -- ^ "submit" or "cancel"
  , prElements :: [PopupResultElement]
    -- ^ Elements with values filled in
  }
  deriving stock (Show, Eq, Generic)

-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH DEFINITION
-- ════════════════════════════════════════════════════════════════════════════

-- | Graph definition for popup tool.
newtype PopupGraph mode = PopupGraph
  { popupRun :: mode :- LogicNode
      :@ Input PopupArgs
      :@ UsesEffects '[TUI, Return PopupResult]
  }
  deriving Generic

-- | MCP tool entry point declaration for popup.
type instance GraphEntries PopupGraph =
  '[ "popup" ':~> '("popupRun", PopupArgs, "Show a general-purpose popup dialog with configurable UI elements. Returns user input as structured data.", '[ 'Dev, 'TL, 'PM]) ]

-- ════════════════════════════════════════════════════════════════════════════
-- POPUP LOGIC
-- ════════════════════════════════════════════════════════════════════════════

-- | Core logic for popup tool.
--
-- Converts the input elements to the internal PopupDefinition format,
-- shows the UI, and then zips values back into the result elements.
popupLogic
  :: (Member TUI es, Member (Return PopupResult) es)
  => PopupArgs
  -> Eff es PopupResult
popupLogic args = do
  -- Convert to internal TUI format
  let internalDef = toPopupDefinition args

  -- Show UI and get result
  TUI.PopupResult button valuesMap <- showUI internalDef

  -- Zip values back into element structure
  let resultElements = zipWithValues (paElements args) valuesMap

  returnValue $ PopupResult
    { prStatus = if button == "submit" then "completed" else "cancelled"
    , prButton = button
    , prElements = resultElements
    }

-- | Convert PopupArgs to internal PopupDefinition.
toPopupDefinition :: PopupArgs -> PopupDefinition
toPopupDefinition args = PopupDefinition
  { pdTitle = maybe "Popup" id (paTitle args)
  , pdComponents = map toComponent (paElements args)
  }
  where
    toComponent :: PopupElement -> Component
    toComponent = \case
      PText eid content ->
        Component eid (Text content) Nothing
      PSlider eid label minV maxV defV ->
        Component eid (Slider label minV maxV (maybe ((minV + maxV) / 2) id defV)) Nothing
      PCheckbox eid label defV ->
        Component eid (Checkbox label (maybe False id defV)) Nothing
      PTextbox eid label placeholder ->
        Component eid (Textbox label placeholder Nothing) Nothing
      PChoice eid label opts defIdx ->
        Component eid (Choice label opts defIdx) Nothing
      PMultiselect eid label opts ->
        Component eid (Multiselect label opts) Nothing
      PGroup eid label ->
        Component eid (Group label) Nothing

-- | Zip values from the result map back into elements.
zipWithValues :: [PopupElement] -> Value -> [PopupResultElement]
zipWithValues elements valuesObj = map zipOne elements
  where
    valuesMap = case valuesObj of
      Object km -> km
      _ -> KM.empty

    getValue :: Text -> Maybe Value
    getValue eid = KM.lookup (Key.fromText eid) valuesMap

    getTextValue :: Text -> Text
    getTextValue eid = case getValue eid of
      Just (String t) -> t
      _ -> ""

    getDoubleValue :: Text -> Double -> Double
    getDoubleValue eid def = case getValue eid of
      Just (Number n) -> realToFrac n
      _ -> def

    getBoolValue :: Text -> Bool -> Bool
    getBoolValue eid def = case getValue eid of
      Just (Bool b) -> b
      _ -> def

    getTextArrayValue :: Text -> [Text]
    getTextArrayValue eid = case getValue eid of
      Just (Array arr) -> [t | String t <- toList arr]
      _ -> []
      where
        toList = foldr (:) []

    zipOne :: PopupElement -> PopupResultElement
    zipOne = \case
      PText eid content ->
        RText eid content
      PSlider eid label minV maxV defV ->
        RSlider eid label (getDoubleValue eid (maybe ((minV + maxV) / 2) id defV))
      PCheckbox eid label defV ->
        RCheckbox eid label (getBoolValue eid (maybe False id defV))
      PTextbox eid label _ ->
        RTextbox eid label (getTextValue eid)
      PChoice eid label opts defIdx ->
        let defaultOpt = case defIdx of
              Just idx | idx >= 0 && idx < length opts -> opts !! idx
              _ -> ""
            selectedText = case getTextValue eid of
              "" -> defaultOpt
              t -> t
        in RChoice eid label selectedText
      PMultiselect eid label _ ->
        RMultiselect eid label (getTextArrayValue eid)
      PGroup eid label ->
        RGroup eid label

-- ════════════════════════════════════════════════════════════════════════════
-- JSON INSTANCES
-- ════════════════════════════════════════════════════════════════════════════

-- PopupElement: TaggedObject with "type" tag, lowercase constructor names
instance ToJSON PopupElement where
  toJSON = \case
    PText eid content -> object
      [ "type" .= ("text" :: Text)
      , "id" .= eid
      , "content" .= content
      ]
    PSlider eid label minV maxV defV -> object $
      [ "type" .= ("slider" :: Text)
      , "id" .= eid
      , "label" .= label
      , "min" .= minV
      , "max" .= maxV
      ] ++ maybe [] (\d -> ["default" .= d]) defV
    PCheckbox eid label defV -> object $
      [ "type" .= ("checkbox" :: Text)
      , "id" .= eid
      , "label" .= label
      ] ++ maybe [] (\d -> ["default" .= d]) defV
    PTextbox eid label placeholder -> object $
      [ "type" .= ("textbox" :: Text)
      , "id" .= eid
      , "label" .= label
      ] ++ maybe [] (\p -> ["placeholder" .= p]) placeholder
    PChoice eid label opts defIdx -> object $
      [ "type" .= ("choice" :: Text)
      , "id" .= eid
      , "label" .= label
      , "options" .= opts
      ] ++ maybe [] (\i -> ["default" .= i]) defIdx
    PMultiselect eid label opts -> object
      [ "type" .= ("multiselect" :: Text)
      , "id" .= eid
      , "label" .= label
      , "options" .= opts
      ]
    PGroup eid label -> object
      [ "type" .= ("group" :: Text)
      , "id" .= eid
      , "label" .= label
      ]

instance FromJSON PopupElement where
  parseJSON = withObject "PopupElement" $ \o -> do
    elemType <- o .: "type"
    eid <- o .: "id"
    case elemType :: Text of
      "text" -> PText eid <$> o .: "content"
      "slider" -> PSlider eid
        <$> o .: "label"
        <*> o .: "min"
        <*> o .: "max"
        <*> o .:? "default"
      "checkbox" -> PCheckbox eid
        <$> o .: "label"
        <*> o .:? "default"
      "textbox" -> PTextbox eid
        <$> o .: "label"
        <*> o .:? "placeholder"
      "choice" -> PChoice eid
        <$> o .: "label"
        <*> o .: "options"
        <*> o .:? "default"
      "multiselect" -> PMultiselect eid
        <$> o .: "label"
        <*> o .: "options"
      "group" -> PGroup eid
        <$> o .: "label"
      _ -> fail $ "Unknown element type: " <> T.unpack elemType

-- PopupResultElement
instance ToJSON PopupResultElement where
  toJSON = \case
    RText eid content -> object
      [ "type" .= ("text" :: Text)
      , "id" .= eid
      , "content" .= content
      ]
    RSlider eid label val -> object
      [ "type" .= ("slider" :: Text)
      , "id" .= eid
      , "label" .= label
      , "value" .= val
      ]
    RCheckbox eid label val -> object
      [ "type" .= ("checkbox" :: Text)
      , "id" .= eid
      , "label" .= label
      , "value" .= val
      ]
    RTextbox eid label val -> object
      [ "type" .= ("textbox" :: Text)
      , "id" .= eid
      , "label" .= label
      , "value" .= val
      ]
    RChoice eid label val -> object
      [ "type" .= ("choice" :: Text)
      , "id" .= eid
      , "label" .= label
      , "value" .= val
      ]
    RMultiselect eid label vals -> object
      [ "type" .= ("multiselect" :: Text)
      , "id" .= eid
      , "label" .= label
      , "value" .= vals
      ]
    RGroup eid label -> object
      [ "type" .= ("group" :: Text)
      , "id" .= eid
      , "label" .= label
      ]

instance FromJSON PopupResultElement where
  parseJSON = withObject "PopupResultElement" $ \o -> do
    elemType <- o .: "type"
    eid <- o .: "id"
    case elemType :: Text of
      "text" -> RText eid <$> o .: "content"
      "slider" -> RSlider eid <$> o .: "label" <*> o .: "value"
      "checkbox" -> RCheckbox eid <$> o .: "label" <*> o .: "value"
      "textbox" -> RTextbox eid <$> o .: "label" <*> o .: "value"
      "choice" -> RChoice eid <$> o .: "label" <*> o .: "value"
      "multiselect" -> RMultiselect eid <$> o .: "label" <*> o .: "value"
      "group" -> RGroup eid <$> o .: "label"
      _ -> fail $ "Unknown result element type: " <> T.unpack elemType

-- PopupArgs
instance ToJSON PopupArgs where
  toJSON args = object $
    maybe [] (\t -> ["title" .= t]) (paTitle args)
    ++ ["elements" .= paElements args]

instance FromJSON PopupArgs where
  parseJSON = withObject "PopupArgs" $ \o -> PopupArgs
    <$> o .:? "title"
    <*> o .: "elements"

-- PopupResult
instance ToJSON PopupResult where
  toJSON res = object
    [ "status" .= prStatus res
    , "button" .= prButton res
    , "elements" .= prElements res
    ]

instance FromJSON PopupResult where
  parseJSON = withObject "PopupResult" $ \o -> PopupResult
    <$> o .: "status"
    <*> o .: "button"
    <*> o .: "elements"

-- ════════════════════════════════════════════════════════════════════════════
-- JSON SCHEMA
-- ════════════════════════════════════════════════════════════════════════════

-- | Schema for a single popup element (oneOf the 7 types)
elementSchema :: JSONSchema
elementSchema = oneOfSchema
  [ -- text element
    describeField "text" "Static text display" $ objectSchema
      [ ("type", enumSchema ["text"])
      , ("id", describeField "id" "Unique element identifier" (emptySchema TString))
      , ("content", describeField "content" "Text content to display" (emptySchema TString))
      ]
      ["type", "id", "content"]

  , -- slider element
    describeField "slider" "Numeric slider input" $ objectSchema
      [ ("type", enumSchema ["slider"])
      , ("id", describeField "id" "Unique element identifier" (emptySchema TString))
      , ("label", describeField "label" "Label shown above slider" (emptySchema TString))
      , ("min", describeField "min" "Minimum value" (emptySchema TNumber))
      , ("max", describeField "max" "Maximum value" (emptySchema TNumber))
      , ("default", describeField "default" "Default value (optional)" (emptySchema TNumber))
      ]
      ["type", "id", "label", "min", "max"]

  , -- checkbox element
    describeField "checkbox" "Boolean checkbox input" $ objectSchema
      [ ("type", enumSchema ["checkbox"])
      , ("id", describeField "id" "Unique element identifier" (emptySchema TString))
      , ("label", describeField "label" "Label shown next to checkbox" (emptySchema TString))
      , ("default", describeField "default" "Default checked state (optional)" (emptySchema TBoolean))
      ]
      ["type", "id", "label"]

  , -- textbox element
    describeField "textbox" "Text input field" $ objectSchema
      [ ("type", enumSchema ["textbox"])
      , ("id", describeField "id" "Unique element identifier" (emptySchema TString))
      , ("label", describeField "label" "Label shown above textbox" (emptySchema TString))
      , ("placeholder", describeField "placeholder" "Placeholder text (optional)" (emptySchema TString))
      ]
      ["type", "id", "label"]

  , -- choice element
    describeField "choice" "Single-select dropdown" $ objectSchema
      [ ("type", enumSchema ["choice"])
      , ("id", describeField "id" "Unique element identifier" (emptySchema TString))
      , ("label", describeField "label" "Label shown above dropdown" (emptySchema TString))
      , ("options", describeField "options" "List of option strings" (arraySchema $ emptySchema TString))
      , ("default", describeField "default" "Default selected index (optional)" (emptySchema TInteger))
      ]
      ["type", "id", "label", "options"]

  , -- multiselect element
    describeField "multiselect" "Multiple selection list" $ objectSchema
      [ ("type", enumSchema ["multiselect"])
      , ("id", describeField "id" "Unique element identifier" (emptySchema TString))
      , ("label", describeField "label" "Label shown above list" (emptySchema TString))
      , ("options", describeField "options" "List of option strings" (arraySchema $ emptySchema TString))
      ]
      ["type", "id", "label", "options"]

  , -- group element
    describeField "group" "Section header/separator" $ objectSchema
      [ ("type", enumSchema ["group"])
      , ("id", describeField "id" "Unique element identifier" (emptySchema TString))
      , ("label", describeField "label" "Section header text" (emptySchema TString))
      ]
      ["type", "id", "label"]
  ]

instance HasJSONSchema PopupArgs where
  jsonSchema = objectSchema
    [ ("title", describeField "title" "Optional popup window title" (emptySchema TString))
    , ("elements", describeField "elements" "List of UI elements" (arraySchema elementSchema))
    ]
    ["elements"]

instance HasJSONSchema PopupResult where
  jsonSchema = objectSchema
    [ ("status", describeField "status" "Result status: 'completed' or 'cancelled'" (emptySchema TString))
    , ("button", describeField "button" "Button pressed: 'submit' or 'cancel'" (emptySchema TString))
    , ("elements", describeField "elements" "Elements with values filled in" (arraySchema $ emptySchema TObject))
    ]
    ["status", "button", "elements"]
