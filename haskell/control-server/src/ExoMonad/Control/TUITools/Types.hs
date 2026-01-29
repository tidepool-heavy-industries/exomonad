{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module ExoMonad.Control.TUITools.Types
  ( PopupElement(..)
  , PopupResultElement(..)
  , PopupArgs(..)
  , PopupResult(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), (.:), (.:?))
import Data.Aeson.Types (withObject)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Language.Haskell.TH (mkName)

import ExoMonad.Schema (deriveMCPTypeWith, defaultMCPOptions, (??), HasJSONSchema(..), objectSchema, arraySchema, oneOfSchema, emptySchema, SchemaType(..), describeField, enumSchema)

-- ════════════════════════════════════════════════════════════════════════════
-- POPUP ELEMENT TYPES
-- ════════════════════════════════════════════════════════════════════════════

data PopupElement
  = PText Text Text
  | PSlider Text Text Double Double (Maybe Double)
  | PCheckbox Text Text (Maybe Bool)
  | PTextbox Text Text (Maybe Text)
  | PChoice Text Text [Text] (Maybe Int)
  | PMultiselect Text Text [Text]
  | PGroup Text Text
  deriving stock (Show, Eq, Generic)

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

instance HasJSONSchema PopupElement where
  jsonSchema = oneOfSchema
    [ describeField "Static text display - Renders as plain text, no interaction. Use for instructions or context." $ objectSchema
      [ ("type", enumSchema ["text"])
      , ("id", describeField "Unique element identifier" (emptySchema TString))
      , ("content", describeField "Text content to display" (emptySchema TString))
      ]
      ["type", "id", "content"]
    , describeField "Numeric slider input - Renders as: [Label] <-- [value] -->. Navigate with left/right arrows." $ objectSchema
      [ ("type", enumSchema ["slider"])
      , ("id", describeField "Unique element identifier" (emptySchema TString))
      , ("label", describeField "Label shown above slider" (emptySchema TString))
      , ("min", describeField "Minimum value" (emptySchema TNumber))
      , ("max", describeField "Maximum value" (emptySchema TNumber))
      , ("default", describeField "Default value (optional)" (emptySchema TNumber))
      ]
      ["type", "id", "label", "min", "max"]
    , describeField "Boolean checkbox input - Renders as: [ ] Label or [x] Label. Toggle with Space key." $ objectSchema
      [ ("type", enumSchema ["checkbox"])
      , ("id", describeField "Unique element identifier" (emptySchema TString))
      , ("label", describeField "Label shown next to checkbox" (emptySchema TString))
      , ("default", describeField "Default checked state (optional)" (emptySchema TBoolean))
      ]
      ["type", "id", "label"]
    , describeField "Text input field - Renders as editable box with cursor. Type normally, backspace to delete." $ objectSchema
      [ ("type", enumSchema ["textbox"])
      , ("id", describeField "Unique element identifier" (emptySchema TString))
      , ("label", describeField "Label shown above textbox" (emptySchema TString))
      , ("placeholder", describeField "Placeholder text shown when empty (optional)" (emptySchema TString))
      ]
      ["type", "id", "label"]
    , describeField "Single-select dropdown - Renders options vertically. Navigate with up/down arrows, current selection highlighted." $ objectSchema
      [ ("type", enumSchema ["choice"])
      , ("id", describeField "Unique element identifier" (emptySchema TString))
      , ("label", describeField "Label shown above dropdown" (emptySchema TString))
      , ("options", describeField "List of option strings (displayed vertically)" (arraySchema $ emptySchema TString))
      , ("default", describeField "Default selected index, 0-based (optional)" (emptySchema TInteger))
      ]
      ["type", "id", "label", "options"]
    , describeField "Multiple selection list - Renders with checkboxes for each option. Space toggles current item, up/down navigates." $ objectSchema
      [ ("type", enumSchema ["multiselect"])
      , ("id", describeField "Unique element identifier" (emptySchema TString))
      , ("label", describeField "Label shown above list" (emptySchema TString))
      , ("options", describeField "List of option strings (each with checkbox)" (arraySchema $ emptySchema TString))
      ]
      ["type", "id", "label", "options"]
    , describeField "Section header/separator - Renders as bold text separator. Use to organize sections visually." $ objectSchema
      [ ("type", enumSchema ["group"])
      , ("id", describeField "Unique element identifier" (emptySchema TString))
      , ("label", describeField "Section header text (rendered bold)" (emptySchema TString))
      ]
      ["type", "id", "label"]
    ]

data PopupResultElement
  = RText Text Text
  | RSlider Text Text Double
  | RCheckbox Text Text Bool
  | RTextbox Text Text Text
  | RChoice Text Text Text
  | RMultiselect Text Text [Text]
  | RGroup Text Text
  deriving stock (Show, Eq, Generic)

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

instance HasJSONSchema PopupResultElement where
  jsonSchema = emptySchema TObject

-- | Arguments for the popup tool.
data PopupArgs = PopupArgs
  { title :: Maybe Text
    -- ^ Optional popup title
  , elements :: [PopupElement]
    -- ^ Flat list of UI elements
  }
  deriving stock (Show, Eq, Generic)

$(deriveMCPTypeWith defaultMCPOptions ''PopupArgs
  [ mkName "title"    ?? "Optional popup title"
  , mkName "elements" ?? "Flat list of UI elements"
  ])

-- | Result of the popup tool.
data PopupResult = PopupResult
  { status :: Text
    -- ^ "completed" or "cancelled"
  , button :: Text
    -- ^ "submit" or "cancel"
  , elements :: [PopupResultElement]
    -- ^ Elements with values filled in
  , timeSpentSeconds :: Maybe Double
    -- ^ Time user spent interacting with popup (seconds)
  }
  deriving stock (Show, Eq, Generic)

$(deriveMCPTypeWith defaultMCPOptions ''PopupResult
  [ mkName "status"   ?? "Result status: 'completed' or 'cancelled'"
  , mkName "button"   ?? "Button pressed: 'submit' or 'cancel'"
  , mkName "elements" ?? "Elements with values filled in"
  , mkName "timeSpentSeconds" ?? "Time spent interacting with popup in seconds. Indicates user engagement: <2s suggests defaults acceptable, >10s may indicate confusion or difficult decisions."
  ])
