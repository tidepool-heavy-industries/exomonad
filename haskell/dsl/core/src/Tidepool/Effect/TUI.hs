{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
-- | TUI effect for showing popup forms and waiting for user responses.
--
-- This effect uses the popup-tui pattern: send a complete PopupDefinition,
-- receive a PopupResult with all form values. No streaming interactions.
--
-- == Example Usage
--
-- @
-- data TeachGraphUI mode = TeachGraphUI
--   { showConfigForm :: mode :- LogicNode
--       :@ Input TeachQuery
--       :@ UsesEffects '[TUI, State FormState]
--       :@ Goto "runExploration" ExplorationConfig
--       :@ Goto Exit CancelledResult
--   ...
--   }
--
-- handleShowConfigForm :: Members '[TUI, State FormState] r
--   => TeachQuery
--   -> Sem r (GotoChoice '["runExploration", Exit] ...)
-- handleShowConfigForm query = do
--   result <- showUI $ PopupDefinition
--     { pdTitle = "Configure Exploration"
--     , pdComponents =
--         [ mkComponent "budget" (Slider "Budget" 10 100 50) Nothing
--         , mkComponent "includeTests" (Checkbox "Include test files" False) Nothing
--         ]
--     }
--   case result.prButton of
--     "submit" -> ... return $ gotoChoice \@"runExploration" config
--     _ -> return $ gotoChoice \@Exit (Left CancelledResult)
-- @
--
-- The TUI effect is **graph-first**: graphs define the state machine structure,
-- and TUI effect provides I/O primitives used within node handlers.
module Tidepool.Effect.TUI
  ( -- * TUI Effect
    TUI(..)
  , showUI

    -- * Protocol Types (popup-tui pattern)
  , PopupDefinition(..)
  , Component(..)
  , ComponentSpec(..)
  , VisibilityRule(..)
  , PopupResult(..)

    -- * Component Constructors
  , mkComponent
  , mkText
  , mkSlider
  , mkCheckbox
  , mkTextbox
  , mkChoice
  , mkMultiselect
  , mkGroup
  ) where

import Control.Applicative ((<|>))
import Control.Monad.Freer (Eff, Member, send)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (Value, ToJSON(..), FromJSON(..), object, (.=), (.:), (.:?), withObject, (.!=))
import Data.Aeson.Types (Parser)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.HashMap.Strict as HM
import GHC.Generics (Generic)

-- ══════════════════════════════════════════════════════════════
-- TUI EFFECT
-- ══════════════════════════════════════════════════════════════

-- | TUI effect for showing popup forms and waiting for user responses.
--
-- Uses the popup-tui pattern: send PopupDefinition, receive PopupResult.
-- No streaming interactions or dynamic updates.
data TUI r where
  -- | Show a popup and block until the user submits or cancels.
  --   Returns the form result with button pressed and all component values.
  ShowUI :: PopupDefinition -> TUI PopupResult

-- | Show a popup definition and wait for user response.
--
-- This is the primary TUI operation. The effect interpreter will send the
-- PopupDefinition to the connected TUI sidebar, which renders it and waits
-- for user input. When the user submits (Enter) or cancels (Esc), the
-- PopupResult is returned with all form values.
--
-- @
-- result <- showUI $ PopupDefinition
--   { pdTitle = "Config"
--   , pdComponents =
--       [ mkSlider "budget" "Budget" 10 100 50 Nothing
--       , mkCheckbox "tests" "Include tests" False Nothing
--       ]
--   }
--
-- case result.prButton of
--   "submit" -> ...
--   _ -> ...
-- @
showUI :: Member TUI effs => PopupDefinition -> Eff effs PopupResult
showUI = send . ShowUI

-- ══════════════════════════════════════════════════════════════
-- PROTOCOL TYPES (popup-tui pattern)
-- ══════════════════════════════════════════════════════════════
--
-- These types define the popup form specification and result protocol.
-- They must match the Rust types exactly for JSON serialization.

-- | Complete popup definition sent to the TUI sidebar.
data PopupDefinition = PopupDefinition
  { pdTitle :: Text
    -- ^ Popup window title
  , pdComponents :: [Component]
    -- ^ Flat list of form components
  }
  deriving (Show, Eq, Generic)

-- | A single form component with optional visibility rule.
data Component = Component
  { cId :: Text
    -- ^ Unique component identifier
  , cSpec :: ComponentSpec
    -- ^ Component type and configuration
  , cVisibleWhen :: Maybe VisibilityRule
    -- ^ Optional visibility condition
  }
  deriving (Show, Eq, Generic)

-- | Component type specifications.
data ComponentSpec
  = Text { csContent :: Text }
    -- ^ Static text display
  | Slider
      { csSliderLabel :: Text
      , csMin :: Double
      , csMax :: Double
      , csSliderDefault :: Double
      }
    -- ^ Numeric slider (min, max, default)
  | Checkbox
      { csCheckboxLabel :: Text
      , csCheckboxDefault :: Bool
      }
    -- ^ Boolean checkbox
  | Textbox
      { csTextboxLabel :: Text
      , csPlaceholder :: Maybe Text
      , csRows :: Maybe Int
      }
    -- ^ Text input (optional multiline)
  | Choice
      { csChoiceLabel :: Text
      , csChoiceOptions :: [Text]
      , csChoiceDefault :: Maybe Int
      }
    -- ^ Single-select dropdown
  | Multiselect
      { csMultiselectLabel :: Text
      , csMultiselectOptions :: [Text]
      }
    -- ^ Multiple selection list
  | Group
      { csGroupLabel :: Text
      }
    -- ^ Section header
  deriving (Show, Eq, Generic)

-- | Visibility rules for conditional component display.
data VisibilityRule
  = Checked Text
    -- ^ Show if checkbox with given ID is checked
  | Equals (KM.KeyMap Text)
    -- ^ Show if choice equals value ({"choiceId": "expectedValue"})
  | GreaterThan { vrId :: Text, vrValue :: Double }
    -- ^ Show if slider value > threshold
  | LessThan { vrId :: Text, vrValue :: Double }
    -- ^ Show if slider value < threshold
  | CountEquals { vrId :: Text, vrCount :: Int }
    -- ^ Show if multiselect has exactly N items selected
  | CountGreaterThan { vrId :: Text, vrCount :: Int }
    -- ^ Show if multiselect has > N items selected
  deriving (Show, Eq, Generic)

-- | Result returned when user submits or cancels the popup.
data PopupResult = PopupResult
  { prButton :: Text
    -- ^ Button pressed: "submit" or "decline"
  , prValues :: Value
    -- ^ JSON object with all visible component values
  }
  deriving (Show, Eq, Generic)

-- ══════════════════════════════════════════════════════════════
-- COMPONENT CONSTRUCTORS (Helpers)
-- ══════════════════════════════════════════════════════════════

-- | Generic component constructor.
mkComponent :: Text -> ComponentSpec -> Maybe VisibilityRule -> Component
mkComponent = Component

-- | Static text component.
mkText :: Text -> Text -> Maybe VisibilityRule -> Component
mkText cid content vis = Component cid (Text content) vis

-- | Slider component.
mkSlider :: Text -> Text -> Double -> Double -> Double -> Maybe VisibilityRule -> Component
mkSlider cid label minVal maxVal defVal vis =
  Component cid (Slider label minVal maxVal defVal) vis

-- | Checkbox component.
mkCheckbox :: Text -> Text -> Bool -> Maybe VisibilityRule -> Component
mkCheckbox cid label defVal vis =
  Component cid (Checkbox label defVal) vis

-- | Textbox component.
mkTextbox :: Text -> Text -> Maybe Text -> Maybe Int -> Maybe VisibilityRule -> Component
mkTextbox cid label placeholder rows vis =
  Component cid (Textbox label placeholder rows) vis

-- | Choice (dropdown) component.
mkChoice :: Text -> Text -> [Text] -> Maybe Int -> Maybe VisibilityRule -> Component
mkChoice cid label options defIdx vis =
  Component cid (Choice label options defIdx) vis

-- | Multiselect component.
mkMultiselect :: Text -> Text -> [Text] -> Maybe VisibilityRule -> Component
mkMultiselect cid label options vis =
  Component cid (Multiselect label options) vis

-- | Group (section header) component.
mkGroup :: Text -> Text -> Maybe VisibilityRule -> Component
mkGroup cid label vis =
  Component cid (Group label) vis

-- ══════════════════════════════════════════════════════════════
-- JSON INSTANCES (must match Rust types exactly)
-- ══════════════════════════════════════════════════════════════

instance ToJSON PopupDefinition where
  toJSON (PopupDefinition title components) = object
    [ "title" .= title
    , "components" .= components
    ]

instance FromJSON PopupDefinition where
  parseJSON = withObject "PopupDefinition" $ \o -> PopupDefinition
    <$> o .: "title"
    <*> o .: "components"

instance ToJSON Component where
  toJSON (Component cid spec vis) = object $
    [ "id" .= cid
    ] ++ specFields spec ++ visFields vis
    where
      specFields s = case s of
        Text content -> [ "type" .= ("text" :: Text), "content" .= content ]
        Slider label minVal maxVal defVal ->
          [ "type" .= ("slider" :: Text)
          , "label" .= label
          , "min" .= minVal
          , "max" .= maxVal
          , "default" .= defVal
          ]
        Checkbox label defVal ->
          [ "type" .= ("checkbox" :: Text)
          , "label" .= label
          , "default" .= defVal
          ]
        Textbox label placeholder rows ->
          [ "type" .= ("textbox" :: Text)
          , "label" .= label
          ] ++ maybe [] (\p -> ["placeholder" .= p]) placeholder
            ++ maybe [] (\r -> ["rows" .= r]) rows
        Choice label options defIdx ->
          [ "type" .= ("choice" :: Text)
          , "label" .= label
          , "options" .= options
          ] ++ maybe [] (\i -> ["default" .= i]) defIdx
        Multiselect label options ->
          [ "type" .= ("multiselect" :: Text)
          , "label" .= label
          , "options" .= options
          ]
        Group label ->
          [ "type" .= ("group" :: Text)
          , "label" .= label
          ]

      visFields Nothing = []
      visFields (Just rule) = ["visible_when" .= rule]

instance FromJSON Component where
  parseJSON = withObject "Component" $ \o -> do
    cid <- o .: "id"
    ctype <- o .: "type"
    spec <- case ctype :: Text of
      "text" -> Text <$> o .: "content"
      "slider" -> Slider
        <$> o .: "label"
        <*> o .: "min"
        <*> o .: "max"
        <*> o .: "default"
      "checkbox" -> Checkbox
        <$> o .: "label"
        <*> o .: "default"
      "textbox" -> Textbox
        <$> o .: "label"
        <*> o .:? "placeholder"
        <*> o .:? "rows"
      "choice" -> Choice
        <$> o .: "label"
        <*> o .: "options"
        <*> o .:? "default"
      "multiselect" -> Multiselect
        <$> o .: "label"
        <*> o .: "options"
      "group" -> Group <$> o .: "label"
      _ -> fail $ "Unknown component type: " <> T.unpack ctype
    vis <- o .:? "visible_when"
    pure $ Component cid spec vis

-- VisibilityRule uses untagged serialization (Rust's #[serde(untagged)])
instance ToJSON VisibilityRule where
  toJSON = \case
    Checked cid -> A.String cid
    Equals conditions -> toJSON conditions
    GreaterThan cid val -> object ["id" .= cid, "value" .= val]
    LessThan cid val -> object ["id" .= cid, "value" .= val]
    CountEquals cid count -> object ["id" .= cid, "count" .= count]
    CountGreaterThan cid count -> object ["id" .= cid, "count" .= count]

instance FromJSON VisibilityRule where
  parseJSON v = parseChecked v <|> parseEquals v <|> parseComparison v
    where
      parseChecked (A.String s) = pure $ Checked s
      parseChecked _ = fail "Not a Checked rule"

      parseEquals val@(A.Object o)
        | KM.size o > 0 && all isString (KM.elems o) =
            pure $ Equals $ KM.map (\(A.String s) -> s) o
        | otherwise = fail "Not an Equals rule"
      parseEquals _ = fail "Not an Equals rule"

      isString (A.String _) = True
      isString _ = False

      parseComparison = withObject "VisibilityRule" $ \o -> do
        -- Disambiguate by checking which fields exist
        hasValue <- (o .: "value" :: Parser (Maybe Double)) >>= \case
          Just _ -> pure True
          Nothing -> pure False
        hasCount <- (o .: "count" :: Parser (Maybe Int)) >>= \case
          Just _ -> pure True
          Nothing -> pure False

        case (hasValue, hasCount) of
          (True, False) -> do
            -- Could be GreaterThan or LessThan - use heuristic or accept both
            cid <- o .: "id"
            val <- o .: "value"
            -- Default to GreaterThan (Rust matches by field order)
            pure $ GreaterThan cid val
          (False, True) -> do
            cid <- o .: "id"
            count <- o .: "count"
            pure $ CountEquals cid count
          _ -> fail "Invalid VisibilityRule"

instance ToJSON PopupResult where
  toJSON (PopupResult button values) = object
    [ "button" .= button
    , "values" .= values
    ]

instance FromJSON PopupResult where
  parseJSON = withObject "PopupResult" $ \o -> PopupResult
    <$> o .: "button"
    <*> o .: "values"
