{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module ExoMonad.Guest.TUI where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, (.=), (.:), (.:?))
import Data.Aeson.Types (Parser)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import ExoMonad.Guest.FFI (FFIBoundary)
import Data.Word (Word32)

-- | Popup definition matches Rust PopupDefinition
data PopupDefinition = PopupDefinition
  { title :: Text
  , components :: [Component]
  } deriving (Show, Eq, Generic)

instance ToJSON PopupDefinition
instance FromJSON PopupDefinition
instance FFIBoundary PopupDefinition

-- | Visibility rule
data VisibilityRule
  = Checked Text           -- ^ Visible when checkbox ID is checked
  | Equals (Map Text Text) -- ^ Visible when choice ID equals value
  | GreaterThan Text Float -- ^ Visible when number ID >= value
  | LessThan Text Float    -- ^ Visible when number ID <= value
  | CountEquals Text Int   -- ^ Visible when count (e.g. multiselect) == value
  | CountGreaterThan Text Int -- ^ Visible when count >= value
  deriving (Show, Eq, Generic)

instance ToJSON VisibilityRule where
  toJSON (Checked elementId) = Aeson.String elementId
  toJSON (Equals ruleMap) = toJSON ruleMap
  toJSON (GreaterThan elementId val) = object ["id" .= elementId, "min_value" .= val]
  toJSON (LessThan elementId val) = object ["id" .= elementId, "max_value" .= val]
  toJSON (CountEquals elementId count) = object ["id" .= elementId, "exact_count" .= count]
  toJSON (CountGreaterThan elementId count) = object ["id" .= elementId, "min_count" .= count]

instance FromJSON VisibilityRule where
  parseJSON v = 
    (Checked <$> parseJSON v) <|>
    (Equals <$> parseJSON v) <|>
    (Aeson.withObject "VisibilityRule" $ \o -> 
        (GreaterThan <$> o .: "id" <*> o .: "min_value") <|>
        (LessThan <$> o .: "id" <*> o .: "max_value") <|>
        (CountEquals <$> o .: "id" <*> o .: "exact_count") <|>
        (CountGreaterThan <$> o .: "id" <*> o .: "min_count")
    ) v

-- | UI Component
data Component
  = TextComponent 
      { id :: Text
      , content :: Text 
      , visible_when :: Maybe VisibilityRule
      }
  | SliderComponent
      { id :: Text
      , label :: Text
      , min :: Float
      , max :: Float
      , sliderDefault :: Float
      , visible_when :: Maybe VisibilityRule
      }
  | CheckboxComponent
      { id :: Text
      , label :: Text
      , checkboxDefault :: Bool
      , visible_when :: Maybe VisibilityRule
      }
  | TextboxComponent
      { id :: Text
      , label :: Text
      , placeholder :: Maybe Text
      , rows :: Maybe Word32
      , visible_when :: Maybe VisibilityRule
      }
  | ChoiceComponent
      { id :: Text
      , label :: Text
      , options :: [Text]
      , choiceDefault :: Maybe Word32
      , visible_when :: Maybe VisibilityRule
      }
  | MultiselectComponent
      { id :: Text
      , label :: Text
      , options :: [Text]
      , multiselectDefault :: Maybe Word32
      , visible_when :: Maybe VisibilityRule
      }
  | GroupComponent
      { id :: Text
      , label :: Text
      , visible_when :: Maybe VisibilityRule
      }
  deriving (Show, Eq, Generic)

instance ToJSON Component where
  toJSON (TextComponent elementId content vw) = object ["type" .= ("text" :: Text), "id" .= elementId, "content" .= content, "visible_when" .= vw]
  toJSON (SliderComponent elementId label minVal maxVal def vw) = object ["type" .= ("slider" :: Text), "id" .= elementId, "label" .= label, "min" .= minVal, "max" .= maxVal, "default" .= def, "visible_when" .= vw]
  toJSON (CheckboxComponent elementId label def vw) = object ["type" .= ("checkbox" :: Text), "id" .= elementId, "label" .= label, "default" .= def, "visible_when" .= vw]
  toJSON (TextboxComponent elementId label ph rows vw) = object ["type" .= ("textbox" :: Text), "id" .= elementId, "label" .= label, "placeholder" .= ph, "rows" .= rows, "visible_when" .= vw]
  toJSON (ChoiceComponent elementId label opts def vw) = object ["type" .= ("choice" :: Text), "id" .= elementId, "label" .= label, "options" .= opts, "default" .= def, "visible_when" .= vw]
  toJSON (MultiselectComponent elementId label opts def vw) = object ["type" .= ("multiselect" :: Text), "id" .= elementId, "label" .= label, "options" .= opts, "default" .= def, "visible_when" .= vw]
  toJSON (GroupComponent elementId label vw) = object ["type" .= ("group" :: Text), "id" .= elementId, "label" .= label, "visible_when" .= vw]

instance FromJSON Component where
  parseJSON = Aeson.withObject "Component" $ \v -> do
    t <- v .: "type" :: Parser Text
    case t of
      "text" -> TextComponent <$> v .: "id" <*> v .: "content" <*> v .:? "visible_when"
      "slider" -> SliderComponent <$> v .: "id" <*> v .: "label" <*> v .: "min" <*> v .: "max" <*> v .: "default" <*> v .:? "visible_when"
      "checkbox" -> CheckboxComponent <$> v .: "id" <*> v .: "label" <*> v .: "default" <*> v .:? "visible_when"
      "textbox" -> TextboxComponent <$> v .: "id" <*> v .: "label" <*> v .:? "placeholder" <*> v .:? "rows" <*> v .:? "visible_when"
      "choice" -> ChoiceComponent <$> v .: "id" <*> v .: "label" <*> v .: "options" <*> v .:? "default" <*> v .:? "visible_when"
      "multiselect" -> MultiselectComponent <$> v .: "id" <*> v .: "label" <*> v .: "options" <*> v .:? "default" <*> v .:? "visible_when"
      "group" -> GroupComponent <$> v .: "id" <*> v .: "label" <*> v .:? "visible_when"
      _ -> fail $ "Unknown component type: " ++ T.unpack t

-- | Result from popup
data PopupResult = PopupResult
  { button :: Text
  , values :: Value
  , time_spent_seconds :: Maybe Double
  } deriving (Show, Eq, Generic)

instance ToJSON PopupResult
instance FromJSON PopupResult
instance FFIBoundary PopupResult

-- | Helper to define components easily
text :: Text -> Text -> Component
text elementId content = TextComponent elementId content Nothing

slider :: Text -> Text -> Float -> Float -> Float -> Component
slider elementId label minVal maxVal def = SliderComponent elementId label minVal maxVal def Nothing

checkbox :: Text -> Text -> Bool -> Component
checkbox elementId label def = CheckboxComponent elementId label def Nothing

textbox :: Text -> Text -> Maybe Text -> Component
textbox elementId label ph = TextboxComponent elementId label ph Nothing Nothing

choice :: Text -> Text -> [Text] -> Component
choice elementId label opts = ChoiceComponent elementId label opts (Just 0) Nothing

multiselect :: Text -> Text -> [Text] -> Component
multiselect elementId label opts = MultiselectComponent elementId label opts Nothing Nothing

group :: Text -> Text -> Component
group elementId label = GroupComponent elementId label Nothing

-- | With visibility modifier
visibleWhen :: VisibilityRule -> Component -> Component
visibleWhen rule c = case c of
  TextComponent i co _ -> TextComponent i co (Just rule)
  SliderComponent i l mn mx d _ -> SliderComponent i l mn mx d (Just rule)
  CheckboxComponent i l d _ -> CheckboxComponent i l d (Just rule)
  TextboxComponent i l ph r _ -> TextboxComponent i l ph r (Just rule)
  ChoiceComponent i l o d _ -> ChoiceComponent i l o d (Just rule)
  MultiselectComponent i l o d _ -> MultiselectComponent i l o d (Just rule)
  GroupComponent i l _ -> GroupComponent i l (Just rule)
