{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
-- | TUI effect for showing structured UIs and waiting for user interaction.
--
-- This effect is designed to be used within LogicNode handlers in graphs.
-- The TUI effect allows graphs to spawn structured UI popups (forms, progress
-- bars, tables) and receive user interactions, integrating with the graph's
-- state machine flow.
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
--   let formSpec = UISpec {...}
--   interaction <- showUI formSpec  -- TUI effect: show and wait
--   case interaction of
--     ButtonClicked _ "cancel" -> return $ gotoChoice \@Exit (Left CancelledResult)
--     ButtonClicked _ "run" -> ... return $ gotoChoice \@"runExploration" config
-- @
--
-- The TUI effect is **graph-first**: graphs define the state machine structure,
-- and TUI effect provides I/O primitives used within node handlers.
module Tidepool.Effect.TUI
  ( -- * TUI Effect
    TUI(..)
  , showUI
  , updateUI
  , closeUI

    -- * Protocol Types (re-exports for convenience)
  , UISpec(..)
  , Layout(..)
  , Element(..)
  , Interaction(..)
  , UIUpdate(..)
  , ElementUpdate(..)
  ) where

import Control.Monad.Freer (Eff, Member, send)
import Data.Text (Text)
import Data.Aeson (Value, ToJSON(..), FromJSON(..), object, (.=), (.:), (.:?), withObject, (.!=))
import GHC.Generics (Generic)

-- ══════════════════════════════════════════════════════════════
-- TUI EFFECT
-- ══════════════════════════════════════════════════════════════

-- | TUI effect for showing structured UIs and waiting for user interaction.
--
-- This is the core effect used by graphs for interactive UI flows.
-- Interpreters connect to Rust TUI sidebars via Unix socket or TCP.
data TUI r where
  -- | Show a UI and block until the user interacts with it.
  --   Returns the interaction event (button click, input submit, etc.).
  ShowUI :: UISpec -> TUI Interaction

  -- | Update the currently shown UI (non-blocking).
  --   Used for progress bars, dynamic content updates, etc.
  UpdateUI :: UIUpdate -> TUI ()

  -- | Close the currently shown UI.
  CloseUI :: TUI ()

-- | Show a UI specification and wait for user interaction.
--
-- This is the primary TUI operation. The effect interpreter will send the
-- UISpec to the connected TUI sidebar, which renders it and waits for user
-- input. When the user interacts (clicks button, submits form, etc.), the
-- interaction is returned.
--
-- @
-- interaction <- showUI $ UISpec
--   { uiId = "config-form"
--   , uiLayout = Vertical
--       [ EInput "topic" "Topic" defaultTopic
--       , EButton "submit" "Submit"
--       , EButton "cancel" "Cancel"
--       ]
--   }
--
-- case interaction of
--   ButtonClicked _ "submit" -> ...
--   ButtonClicked _ "cancel" -> ...
-- @
showUI :: Member TUI effs => UISpec -> Eff effs Interaction
showUI = send . ShowUI

-- | Update an element in the currently shown UI.
--
-- Non-blocking operation for dynamic updates like progress bars, status text,
-- etc. The TUI sidebar will update the specified element without waiting for
-- user interaction.
--
-- @
-- updateUI $ UIUpdate
--   { uuUiId = "progress"
--   , uuElementId = "bar"
--   , uuUpdate = SetProgress 50 100
--   }
-- @
updateUI :: Member TUI effs => UIUpdate -> Eff effs ()
updateUI = send . UpdateUI

-- | Close the currently shown UI.
--
-- Removes the UI from the TUI sidebar. Typically called after receiving a
-- final interaction or when transitioning to a new graph node.
closeUI :: Member TUI effs => Eff effs ()
closeUI = send CloseUI

-- ══════════════════════════════════════════════════════════════
-- PROTOCOL TYPES
-- ══════════════════════════════════════════════════════════════
--
-- These types define the UI specification and interaction protocol.
-- They must match the Rust types exactly for JSON serialization.

-- | Complete UI specification sent to the TUI sidebar.
data UISpec = UISpec
  { uiId :: Text
    -- ^ Unique identifier for this UI instance
  , uiLayout :: Layout
    -- ^ Layout and element tree
  }
  deriving (Show, Eq, Generic)

-- | Layout structure for UI elements.
data Layout
  = Vertical [Element]
    -- ^ Stack elements vertically
  | Horizontal [Element]
    -- ^ Stack elements horizontally
  | Split
      { splitLeft :: Layout
      , splitRight :: Layout
      , splitRatio :: Int  -- ^ Percentage for left side (0-100)
      }
    -- ^ Split layout with ratio
  deriving (Show, Eq, Generic)

-- | UI elements (widgets).
data Element
  = EText
      { eId :: Text
      , eContent :: Text
      }
    -- ^ Static text display
  | EInput
      { eId :: Text
      , eLabel :: Text
      , eValue :: Text
      }
    -- ^ Text input field
  | EButton
      { eId :: Text
      , eLabel :: Text
      }
    -- ^ Clickable button
  | ETable
      { eId :: Text
      , eColumns :: [Text]
      , eRows :: [[Text]]
      }
    -- ^ Data table
  | ESelect
      { eId :: Text
      , eLabel :: Text
      , eOptions :: [Text]
      , eSelected :: Maybe Int
      }
    -- ^ Dropdown/list selection
  | EList
      { eId :: Text
      , eItems :: [Text]
      }
    -- ^ Simple list display
  | EProgress
      { eId :: Text
      , eLabel :: Text
      , pValue :: Int  -- ^ Progress value (renamed from eValue to avoid collision with EInput)
      , pMax :: Int    -- ^ Maximum progress value
      }
    -- ^ Progress bar
  deriving (Show, Eq, Generic)

-- ══════════════════════════════════════════════════════════════
-- JSON INSTANCES (must match Rust types exactly)
-- ══════════════════════════════════════════════════════════════

instance ToJSON UISpec where
  toJSON (UISpec uId layout) = object
    [ "id" .= uId
    , "layout" .= layout
    ]

instance FromJSON UISpec where
  parseJSON = withObject "UISpec" $ \o -> UISpec
    <$> o .: "id"
    <*> o .: "layout"

instance ToJSON Layout where
  toJSON = \case
    Vertical elements -> object
      [ "type" .= ("Vertical" :: Text)
      , "elements" .= elements
      ]
    Horizontal elements -> object
      [ "type" .= ("Horizontal" :: Text)
      , "elements" .= elements
      ]
    Split left right ratio -> object
      [ "type" .= ("Split" :: Text)
      , "left" .= left
      , "right" .= right
      , "ratio" .= ratio
      ]

instance FromJSON Layout where
  parseJSON = withObject "Layout" $ \o -> do
    layoutType <- o .: "type"
    case layoutType of
      "Vertical" -> Vertical <$> o .: "elements"
      "Horizontal" -> Horizontal <$> o .: "elements"
      "Split" -> Split <$> o .: "left" <*> o .: "right" <*> o .: "ratio"
      _ -> fail $ "Unknown layout type: " <> layoutType

instance ToJSON Element where
  toJSON = \case
    EText eid content -> object
      [ "type" .= ("Text" :: Text)
      , "id" .= eid
      , "content" .= content
      ]
    EInput eid label value -> object
      [ "type" .= ("Input" :: Text)
      , "id" .= eid
      , "label" .= label
      , "value" .= value
      ]
    EButton eid label -> object
      [ "type" .= ("Button" :: Text)
      , "id" .= eid
      , "label" .= label
      ]
    ETable eid columns rows -> object
      [ "type" .= ("Table" :: Text)
      , "id" .= eid
      , "columns" .= columns
      , "rows" .= rows
      ]
    ESelect eid label options selected -> object
      [ "type" .= ("Select" :: Text)
      , "id" .= eid
      , "label" .= label
      , "options" .= options
      , "selected" .= selected
      ]
    EList eid items -> object
      [ "type" .= ("List" :: Text)
      , "id" .= eid
      , "items" .= items
      ]
    EProgress eid label value maxVal -> object
      [ "type" .= ("Progress" :: Text)
      , "id" .= eid
      , "label" .= label
      , "value" .= value  -- JSON field stays "value"
      , "max" .= maxVal   -- JSON field stays "max"
      ]

instance FromJSON Element where
  parseJSON = withObject "Element" $ \o -> do
    elemType <- o .: "type"
    case elemType of
      "Text" -> EText <$> o .: "id" <*> o .: "content"
      "Input" -> EInput <$> o .: "id" <*> o .: "label" <*> o .: "value"
      "Button" -> EButton <$> o .: "id" <*> o .: "label"
      "Table" -> ETable <$> o .: "id" <*> o .: "columns" <*> o .: "rows"
      "Select" -> ESelect <$> o .: "id" <*> o .: "label" <*> o .: "options" <*> o .:? "selected"
      "List" -> EList <$> o .: "id" <*> o .: "items"
      "Progress" -> EProgress <$> o .: "id" <*> o .: "label" <*> o .: "value" <*> o .: "max"
      _ -> fail $ "Unknown element type: " <> elemType

instance ToJSON Interaction where
  toJSON = \case
    ButtonClicked uiId buttonId -> object
      [ "type" .= ("ButtonClicked" :: Text)
      , "ui_id" .= uiId
      , "button_id" .= buttonId
      ]
    InputSubmitted uiId inputId value -> object
      [ "type" .= ("InputSubmitted" :: Text)
      , "ui_id" .= uiId
      , "input_id" .= inputId
      , "value" .= value
      ]
    InputChanged uiId inputId value -> object
      [ "type" .= ("InputChanged" :: Text)
      , "ui_id" .= uiId
      , "input_id" .= inputId
      , "value" .= value
      ]
    TableRowSelected uiId tableId row -> object
      [ "type" .= ("TableRowSelected" :: Text)
      , "ui_id" .= uiId
      , "table_id" .= tableId
      , "row" .= row
      ]
    SelectionChanged uiId selectId index -> object
      [ "type" .= ("SelectionChanged" :: Text)
      , "ui_id" .= uiId
      , "select_id" .= selectId
      , "index" .= index
      ]

instance FromJSON Interaction where
  parseJSON = withObject "Interaction" $ \o -> do
    interactionType <- o .: "type"
    case interactionType of
      "ButtonClicked" -> ButtonClicked <$> o .: "ui_id" <*> o .: "button_id"
      "InputSubmitted" -> InputSubmitted <$> o .: "ui_id" <*> o .: "input_id" <*> o .: "value"
      "InputChanged" -> InputChanged <$> o .: "ui_id" <*> o .: "input_id" <*> o .: "value"
      "TableRowSelected" -> TableRowSelected <$> o .: "ui_id" <*> o .: "table_id" <*> o .: "row"
      "SelectionChanged" -> SelectionChanged <$> o .: "ui_id" <*> o .: "select_id" <*> o .: "index"
      _ -> fail $ "Unknown interaction type: " <> interactionType

instance ToJSON UIUpdate where
  toJSON (UIUpdate uId elemId upd) = object
    [ "ui_id" .= uId
    , "element_id" .= elemId
    , "update" .= upd
    ]

instance FromJSON UIUpdate where
  parseJSON = withObject "UIUpdate" $ \o -> UIUpdate
    <$> o .: "ui_id"
    <*> o .: "element_id"
    <*> o .: "update"

instance ToJSON ElementUpdate where
  toJSON = \case
    SetText text -> object
      [ "type" .= ("SetText" :: Text)
      , "text" .= text
      ]
    SetValue value -> object
      [ "type" .= ("SetValue" :: Text)
      , "value" .= value
      ]
    SetProgress value maxVal -> object
      [ "type" .= ("SetProgress" :: Text)
      , "value" .= value
      , "max" .= maxVal
      ]
    SetOptions options selected -> object
      [ "type" .= ("SetOptions" :: Text)
      , "options" .= options
      , "selected" .= selected
      ]

instance FromJSON ElementUpdate where
  parseJSON = withObject "ElementUpdate" $ \o -> do
    updateType <- o .: "type"
    case updateType of
      "SetText" -> SetText <$> o .: "text"
      "SetValue" -> SetValue <$> o .: "value"
      "SetProgress" -> SetProgress <$> o .: "value" <*> o .: "max"
      "SetOptions" -> SetOptions <$> o .: "options" <*> o .:? "selected"
      _ -> fail $ "Unknown update type: " <> updateType

-- | User interaction events from the TUI sidebar.
data Interaction
  = ButtonClicked
      { iUiId :: Text
      , iButtonId :: Text
      }
    -- ^ User clicked a button
  | InputSubmitted
      { iUiId :: Text
      , iInputId :: Text
      , iValue :: Text
      }
    -- ^ User submitted an input field (Enter key)
  | InputChanged
      { iUiId :: Text
      , iInputId :: Text
      , iValue :: Text
      }
    -- ^ Input field value changed (keystroke)
  | TableRowSelected
      { iUiId :: Text
      , iTableId :: Text
      , iRow :: Int
      }
    -- ^ User selected a table row
  | SelectionChanged
      { iUiId :: Text
      , iSelectId :: Text
      , iIndex :: Int
      }
    -- ^ Dropdown/list selection changed
  deriving (Show, Eq, Generic)

-- | Update message for modifying existing UI elements.
data UIUpdate = UIUpdate
  { uuUiId :: Text
    -- ^ ID of the UI containing the element
  , uuElementId :: Text
    -- ^ ID of the element to update
  , uuUpdate :: ElementUpdate
    -- ^ The update to apply
  }
  deriving (Show, Eq, Generic)

-- | Specific update operations for elements.
data ElementUpdate
  = SetText Text
    -- ^ Update text content
  | SetValue Text
    -- ^ Update input field value
  | SetProgress
      { epValue :: Int
      , epMax :: Int
      }
    -- ^ Update progress bar
  | SetOptions
      { eoOptions :: [Text]
      , eoSelected :: Maybe Int
      }
    -- ^ Update select options and selection
  deriving (Show, Eq, Generic)
