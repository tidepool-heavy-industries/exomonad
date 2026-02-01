{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
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
module ExoMonad.Effect.TUI
  ( -- * TUI Effect
    TUI (..),
    showUI,

    -- * Protocol Types (popup-tui pattern)
    PopupDefinition (..),
    Component (..),
    ComponentSpec (..),
    VisibilityRule (..),
    PopupResult (..),

    -- * Component Constructors
    mkComponent,
    mkText,
    mkSlider,
    mkCheckbox,
    mkTextbox,
    mkChoice,
    mkMultiselect,
    mkGroup,
  )
where

import Control.Monad.Freer (Eff, Member, send)
import Data.Aeson (FromJSON, ToJSON)

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
showUI :: (Member TUI effs) => PopupDefinition -> Eff effs PopupResult
showUI = send . ShowUI

-- ══════════════════════════════════════════════════════════════
-- PROTOCOL TYPES (popup-tui pattern)
-- ══════════════════════════════════════════════════════════════
--
-- These types define the popup form specification and result protocol.
-- They must match the Rust types exactly for JSON serialization.

-- | Complete popup definition sent to the TUI sidebar.
data PopupDefinition = PopupDefinition
  { -- | Popup window title
    pdTitle :: Text,
    -- | Flat list of form components
    pdComponents :: [Component]
  }
  deriving (Show, Eq, Generic)

-- | A single form component with optional visibility rule.
data Component = Component
  { -- | Unique component identifier
    cId :: Text,
    -- | Component type and configuration
    cSpec :: ComponentSpec,
    -- | Optional visibility condition
    cVisibleWhen :: Maybe VisibilityRule
  }
  deriving (Show, Eq, Generic)

-- | Component type specifications.
data ComponentSpec
  = -- | Static text display
    Text {csContent :: Text}
  | -- | Numeric slider (min, max, default)
    Slider
      { csSliderLabel :: Text,
        csMin :: Double,
        csMax :: Double,
        csSliderDefault :: Double
      }
  | -- | Boolean checkbox
    Checkbox
      { csCheckboxLabel :: Text,
        csCheckboxDefault :: Bool
      }
  | -- | Text input (optional multiline)
    Textbox
      { csTextboxLabel :: Text,
        csPlaceholder :: Maybe Text,
        csRows :: Maybe Int
      }
  | -- | Single-select dropdown
    Choice
      { csChoiceLabel :: Text,
        csChoiceOptions :: [Text],
        csChoiceDefault :: Maybe Int
      }
  | -- | Multiple selection list
    Multiselect
      { csMultiselectLabel :: Text,
        csMultiselectOptions :: [Text]
      }
  | -- | Section header
    Group
      { csGroupLabel :: Text
      }
  deriving (Show, Eq, Generic)

-- | Visibility rules for conditional component display.
--
-- Note: Uses untagged serialization. Field names are deliberately different
-- to avoid ambiguity during deserialization.
data VisibilityRule
  = -- | Show if checkbox with given ID is checked
    Checked Text
  | -- | Show if choice equals value ({"choiceId": "expectedValue"})
    Equals (KM.KeyMap Text)
  | -- | Show if slider value > min_value
    GreaterThan {vrId :: Text, vrMinValue :: Double}
  | -- | Show if slider value < max_value
    LessThan {vrId :: Text, vrMaxValue :: Double}
  | -- | Show if multiselect has exactly N items selected
    CountEquals {vrId :: Text, vrExactCount :: Int}
  | -- | Show if multiselect has > N items selected
    CountGreaterThan {vrId :: Text, vrMinCount :: Int}
  deriving (Show, Eq, Generic)

-- | Result returned when user submits or cancels the popup.
data PopupResult = PopupResult
  { -- | Button pressed: "submit" or "decline"
    prButton :: Text,
    -- | JSON object with all visible component values
    prValues :: Value,
    -- | Time spent interacting with popup in seconds
    prTimeSpent :: Maybe Double
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
mkText cid content = Component cid (Text content)

-- | Slider component.
mkSlider :: Text -> Text -> Double -> Double -> Double -> Maybe VisibilityRule -> Component
mkSlider cid label minVal maxVal defVal =
  Component cid (Slider label minVal maxVal defVal)

-- | Checkbox component.
mkCheckbox :: Text -> Text -> Bool -> Maybe VisibilityRule -> Component
mkCheckbox cid label defVal =
  Component cid (Checkbox label defVal)

-- | Textbox component.
mkTextbox :: Text -> Text -> Maybe Text -> Maybe Int -> Maybe VisibilityRule -> Component
mkTextbox cid label placeholder rows =
  Component cid (Textbox label placeholder rows)

-- | Choice (dropdown) component.
mkChoice :: Text -> Text -> [Text] -> Maybe Int -> Maybe VisibilityRule -> Component
mkChoice cid label options defIdx =
  Component cid (Choice label options defIdx)

-- | Multiselect component.
mkMultiselect :: Text -> Text -> [Text] -> Maybe VisibilityRule -> Component
mkMultiselect cid label options =
  Component cid (Multiselect label options)

-- | Group (section header) component.
mkGroup :: Text -> Text -> Maybe VisibilityRule -> Component
mkGroup cid label =
  Component cid (Group label)

-- ══════════════════════════════════════════════════════════════
-- JSON INSTANCES (must match Rust types exactly)
-- ══════════════════════════════════════════════════════════════

instance ToJSON PopupDefinition where
  toJSON (PopupDefinition title components) =
    object
      [ "title" .= title,
        "components" .= components
      ]

instance FromJSON PopupDefinition where
  parseJSON = withObject "PopupDefinition" $ \o ->
    PopupDefinition
      <$> o .: "title"
      <*> o .: "components"

instance ToJSON Component where
  toJSON (Component cid spec vis) =
    object $
      [ "id" .= cid
      ]
        ++ specFields spec
        ++ visFields vis
    where
      specFields s = case s of
        Text content -> ["type" .= ("text" :: Text), "content" .= content]
        Slider label minVal maxVal defVal ->
          [ "type" .= ("slider" :: Text),
            "label" .= label,
            "min" .= minVal,
            "max" .= maxVal,
            "default" .= defVal
          ]
        Checkbox label defVal ->
          [ "type" .= ("checkbox" :: Text),
            "label" .= label,
            "default" .= defVal
          ]
        Textbox label placeholder rows ->
          [ "type" .= ("textbox" :: Text),
            "label" .= label
          ]
            ++ maybe [] (\p -> ["placeholder" .= p]) placeholder
            ++ maybe [] (\r -> ["rows" .= r]) rows
        Choice label options defIdx ->
          [ "type" .= ("choice" :: Text),
            "label" .= label,
            "options" .= options
          ]
            ++ maybe [] (\i -> ["default" .= i]) defIdx
        Multiselect label options ->
          [ "type" .= ("multiselect" :: Text),
            "label" .= label,
            "options" .= options
          ]
        Group label ->
          [ "type" .= ("group" :: Text),
            "label" .= label
          ]

      visFields Nothing = []
      visFields (Just rule) = ["visible_when" .= rule]

instance FromJSON Component where
  parseJSON = withObject "Component" $ \o -> do
    cid <- o .: "id"
    ctype <- o .: "type"
    spec <- case ctype :: Text of
      "text" -> Text <$> o .: "content"
      "slider" ->
        Slider
          <$> o .: "label"
          <*> o .: "min"
          <*> o .: "max"
          <*> o .: "default"
      "checkbox" ->
        Checkbox
          <$> o .: "label"
          <*> o .: "default"
      "textbox" ->
        Textbox
          <$> o .: "label"
          <*> o .:? "placeholder"
          <*> o .:? "rows"
      "choice" ->
        Choice
          <$> o .: "label"
          <*> o .: "options"
          <*> o .:? "default"
      "multiselect" ->
        Multiselect
          <$> o .: "label"
          <*> o .: "options"
      "group" -> Group <$> o .: "label"
      _ -> fail $ "Unknown component type: " <> T.unpack ctype
    vis <- o .:? "visible_when"
    pure $ Component cid spec vis

-- VisibilityRule uses untagged serialization (Rust's #[serde(untagged)])
-- Field names are deliberately different to avoid ambiguity
instance ToJSON VisibilityRule where
  toJSON = \case
    Checked cid -> A.String cid
    Equals conditions -> toJSON conditions
    GreaterThan cid val -> object ["id" .= cid, "min_value" .= val]
    LessThan cid val -> object ["id" .= cid, "max_value" .= val]
    CountEquals cid count -> object ["id" .= cid, "exact_count" .= count]
    CountGreaterThan cid count -> object ["id" .= cid, "min_count" .= count]

instance FromJSON VisibilityRule where
  parseJSON v = parseChecked v <|> parseEquals v <|> parseComparison v
    where
      parseChecked (A.String s) = pure $ Checked s
      parseChecked _ = fail "Not a Checked rule"

      parseEquals (A.Object o)
        | KM.size o > 0 = case traverse asString o of
            Just strMap -> pure $ Equals strMap
            Nothing -> fail "Not an Equals rule: values must be strings"
        | otherwise = fail "Not an Equals rule"
      parseEquals _ = fail "Not an Equals rule"

      asString (A.String s) = Just s
      asString _ = Nothing

      parseComparison = withObject "VisibilityRule" $ \o -> do
        -- Disambiguate by checking which fields exist (now uses unique field names)
        cid <- o .: "id"
        minValue <- o .:? "min_value"
        maxValue <- o .:? "max_value"
        exactCount <- o .:? "exact_count"
        minCount <- o .:? "min_count"

        case (minValue, maxValue, exactCount, minCount) of
          (Just val, Nothing, Nothing, Nothing) -> pure $ GreaterThan cid val
          (Nothing, Just val, Nothing, Nothing) -> pure $ LessThan cid val
          (Nothing, Nothing, Just count, Nothing) -> pure $ CountEquals cid count
          (Nothing, Nothing, Nothing, Just count) -> pure $ CountGreaterThan cid count
          _ -> fail "Invalid VisibilityRule: ambiguous or missing fields"

instance ToJSON PopupResult where
  toJSON (PopupResult button values timeSpent) =
    object $
      [ "button" .= button,
        "values" .= values
      ]
        ++ maybe [] (\t -> ["time_spent_seconds" .= t]) timeSpent

instance FromJSON PopupResult where
  parseJSON = withObject "PopupResult" $ \o ->
    PopupResult
      <$> o .: "button"
      <*> o .: "values"
      <*> o .:? "time_spent_seconds"
