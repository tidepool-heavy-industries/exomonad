-- | Simple flexible popup MCP tool.
--
-- One flexible popup tool - theory-of-mind emerges from how I use it, not from schema complexity.
module ExoMonad.Guest.Tools.Popup
  ( -- * Tool type
    Popup,

    -- * Argument types
    PopupArgs (..),
    PopupElement (..),
    PopupElementType (..),
    PopupResponse (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withObject, withText, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Guest.FFI (FFIBoundary)
import ExoMonad.Guest.HostCall (callHost, host_show_popup)
import ExoMonad.Guest.Tool.Class (MCPCallOutput, MCPTool (..), errorResult, successResult)
import GHC.Generics (Generic)

-- ============================================================================
-- Popup Tool
-- ============================================================================

-- | Show interactive popup form and get user response.
data Popup

-- | Arguments for the popup tool.
data PopupArgs = PopupArgs
  { paTitle :: Maybe Text,
    paElements :: [PopupElement]
  }
  deriving (Show, Eq, Generic)

instance FromJSON PopupArgs where
  parseJSON = withObject "PopupArgs" $ \v ->
    PopupArgs
      <$> v .:? "title"
      <*> v .: "elements"

-- | A single UI element in the popup.
data PopupElement = PopupElement
  { peType :: PopupElementType,
    peId :: Maybe Text,
    peLabel :: Maybe Text,
    peContent :: Maybe Text,
    peOptions :: Maybe [Text],
    peDefault :: Maybe Value,
    pePlaceholder :: Maybe Text,
    peMin :: Maybe Double,
    peMax :: Maybe Double,
    peRows :: Maybe Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON PopupElement where
  parseJSON = withObject "PopupElement" $ \v ->
    PopupElement
      <$> v .: "type"
      <*> v .:? "id"
      <*> v .:? "label"
      <*> v .:? "content"
      <*> v .:? "options"
      <*> v .:? "default"
      <*> v .:? "placeholder"
      <*> v .:? "min"
      <*> v .:? "max"
      <*> v .:? "rows"

-- | Element type (matches MCP schema enum).
data PopupElementType
  = PEText
  | PEChoice
  | PECheckbox
  | PETextbox
  | PESlider
  | PEMultiselect
  deriving (Show, Eq, Generic)

instance FromJSON PopupElementType where
  parseJSON = withText "PopupElementType" $ \case
    "text" -> pure PEText
    "choice" -> pure PEChoice
    "checkbox" -> pure PECheckbox
    "textbox" -> pure PETextbox
    "slider" -> pure PESlider
    "multiselect" -> pure PEMultiselect
    other -> fail $ "Unknown element type: " <> T.unpack other

-- | Response from the popup (returned by host function).
data PopupResponse = PopupResponse
  { prButton :: Text,
    prValues :: Value
  }
  deriving (Show, Eq, Generic)

instance ToJSON PopupResponse where
  toJSON (PopupResponse btn vals) =
    object
      [ "button" .= btn,
        "values" .= vals
      ]

instance FromJSON PopupResponse where
  parseJSON = withObject "PopupResponse" $ \v ->
    PopupResponse
      <$> v .: "button"
      <*> v .: "values"

instance FFIBoundary PopupResponse

-- ============================================================================
-- Request type sent to host function
-- ============================================================================

-- | Request type for the host function (matches Rust PopupDefinition).
data PopupRequest = PopupRequest
  { prqTitle :: Text,
    prqComponents :: [PopupComponent]
  }
  deriving (Show, Eq, Generic)

instance ToJSON PopupRequest where
  toJSON (PopupRequest title comps) =
    object
      [ "title" .= title,
        "components" .= comps
      ]

-- Not needed since we only serialize (toFFI), but FFIBoundary requires FromJSON
instance FromJSON PopupRequest where
  parseJSON = withObject "PopupRequest" $ \v ->
    PopupRequest
      <$> v .: "title"
      <*> v .: "components"

instance FFIBoundary PopupRequest

-- | Component in the format expected by Rust (flat with "type" field).
data PopupComponent = PopupComponent
  { pcType :: Text,
    pcId :: Text,
    pcLabel :: Maybe Text,
    pcContent :: Maybe Text,
    pcOptions :: Maybe [Text],
    pcDefault :: Maybe Value,
    pcPlaceholder :: Maybe Text,
    pcMin :: Maybe Double,
    pcMax :: Maybe Double,
    pcRows :: Maybe Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON PopupComponent where
  toJSON (PopupComponent ty cid lbl cont opts def ph mn mx rows) =
    object $
      [ "type" .= ty,
        "id" .= cid
      ]
        ++ maybe [] (\l -> ["label" .= l]) lbl
        ++ maybe [] (\c -> ["content" .= c]) cont
        ++ maybe [] (\o -> ["options" .= o]) opts
        ++ maybe [] (\d -> ["default" .= d]) def
        ++ maybe [] (\p -> ["placeholder" .= p]) ph
        ++ maybe [] (\m -> ["min" .= m]) mn
        ++ maybe [] (\m -> ["max" .= m]) mx
        ++ maybe [] (\r -> ["rows" .= r]) rows

-- Not needed since we only serialize, but FFIBoundary requires FromJSON
instance FromJSON PopupComponent where
  parseJSON = withObject "PopupComponent" $ \v ->
    PopupComponent
      <$> v .: "type"
      <*> v .: "id"
      <*> v .:? "label"
      <*> v .:? "content"
      <*> v .:? "options"
      <*> v .:? "default"
      <*> v .:? "placeholder"
      <*> v .:? "min"
      <*> v .:? "max"
      <*> v .:? "rows"

-- ============================================================================
-- Conversion helpers
-- ============================================================================

-- | Convert element type to text for JSON.
elementTypeToText :: PopupElementType -> Text
elementTypeToText = \case
  PEText -> "text"
  PEChoice -> "choice"
  PECheckbox -> "checkbox"
  PETextbox -> "textbox"
  PESlider -> "slider"
  PEMultiselect -> "multiselect"

-- | Generate a component ID if none provided.
generateId :: Int -> PopupElementType -> Text
generateId idx ty = elementTypeToText ty <> "_" <> T.pack (show idx)

-- | Convert PopupArgs to PopupRequest (host function format).
toPopupRequest :: PopupArgs -> PopupRequest
toPopupRequest args =
  PopupRequest
    { prqTitle = maybe "" id (paTitle args),
      prqComponents = zipWith convertElement [0 ..] (paElements args)
    }
  where
    convertElement :: Int -> PopupElement -> PopupComponent
    convertElement idx el =
      PopupComponent
        { pcType = elementTypeToText (peType el),
          pcId = maybe (generateId idx (peType el)) id (peId el),
          pcLabel = peLabel el,
          pcContent = peContent el,
          pcOptions = peOptions el,
          pcDefault = peDefault el,
          pcPlaceholder = pePlaceholder el,
          pcMin = peMin el,
          pcMax = peMax el,
          pcRows = peRows el
        }

-- ============================================================================
-- MCPTool instance
-- ============================================================================

instance MCPTool Popup where
  type ToolArgs Popup = PopupArgs
  toolName = "popup"
  toolDescription = "Show interactive popup form and get user response"
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "title"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Optional popup title" :: Text)
                  ],
              "elements"
                .= object
                  [ "type" .= ("array" :: Text),
                    "description" .= ("UI elements to display" :: Text),
                    "items"
                      .= object
                        [ "type" .= ("object" :: Text),
                          "required" .= (["type"] :: [Text]),
                          "properties"
                            .= object
                              [ "type"
                                  .= object
                                    [ "type" .= ("string" :: Text),
                                      "enum" .= (["text", "choice", "checkbox", "textbox", "slider", "multiselect"] :: [Text])
                                    ],
                                "id"
                                  .= object
                                    [ "type" .= ("string" :: Text),
                                      "description" .= ("Element ID for result lookup" :: Text)
                                    ],
                                "label"
                                  .= object
                                    [ "type" .= ("string" :: Text)
                                    ],
                                "content"
                                  .= object
                                    [ "type" .= ("string" :: Text),
                                      "description" .= ("For text elements" :: Text)
                                    ],
                                "options"
                                  .= object
                                    [ "type" .= ("array" :: Text),
                                      "items" .= object ["type" .= ("string" :: Text)],
                                      "description" .= ("For choice/multiselect" :: Text)
                                    ],
                                "default"
                                  .= object
                                    [ "description" .= ("Default value (type varies)" :: Text)
                                    ],
                                "placeholder"
                                  .= object
                                    [ "type" .= ("string" :: Text),
                                      "description" .= ("For textbox" :: Text)
                                    ],
                                "min"
                                  .= object
                                    [ "type" .= ("number" :: Text),
                                      "description" .= ("For slider" :: Text)
                                    ],
                                "max"
                                  .= object
                                    [ "type" .= ("number" :: Text),
                                      "description" .= ("For slider" :: Text)
                                    ],
                                "rows"
                                  .= object
                                    [ "type" .= ("integer" :: Text),
                                      "description" .= ("For multiline textbox" :: Text)
                                    ]
                              ]
                        ]
                  ]
            ],
        "required" .= (["elements"] :: [Text])
      ]
  toolHandler args = do
    let request = toPopupRequest args
    result <- callHost host_show_popup request :: IO (Either Text PopupResponse)
    case result of
      Left err -> pure $ errorResult err
      Right resp -> pure $ successResult $ Aeson.toJSON resp
