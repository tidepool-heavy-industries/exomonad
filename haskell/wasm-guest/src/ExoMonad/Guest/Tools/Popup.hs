{-# LANGUAGE TypeApplications #-}

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
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Effects.Popup qualified as PP
import ExoMonad.Effects.Popup (showPopup)
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

-- | Convert PopupArgs to the proto ShowPopupRequest.
-- The components are serialized as JSON bytes in the proto message
-- because the proto schema uses a bytes field for flexible component data.
toShowPopupRequest :: PopupArgs -> PP.ShowPopupRequest
toShowPopupRequest args =
  PP.ShowPopupRequest
    { PP.showPopupRequestTitle = TL.fromStrict (maybe "" id (paTitle args)),
      PP.showPopupRequestComponents = BL.toStrict $ Aeson.encode (zipWith convertElement [0 ..] (paElements args))
    }
  where
    convertElement :: Int -> PopupElement -> Value
    convertElement idx el =
      object $
        [ "type" .= elementTypeToText (peType el),
          "id" .= maybe (generateId idx (peType el)) id (peId el)
        ]
          ++ maybe [] (\l -> ["label" .= l]) (peLabel el)
          ++ maybe [] (\c -> ["content" .= c]) (peContent el)
          ++ maybe [] (\o -> ["options" .= o]) (peOptions el)
          ++ maybe [] (\d -> ["default" .= d]) (peDefault el)
          ++ maybe [] (\p -> ["placeholder" .= p]) (pePlaceholder el)
          ++ maybe [] (\m -> ["min" .= m]) (peMin el)
          ++ maybe [] (\m -> ["max" .= m]) (peMax el)
          ++ maybe [] (\r -> ["rows" .= r]) (peRows el)

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
    let request = toShowPopupRequest args
    result <- showPopup request
    case result of
      Left err -> pure $ errorResult (T.pack (show err))
      Right resp ->
        -- The response contains the button clicked and form values as JSON bytes
        let responseJson = Aeson.decode (BL.fromStrict (PP.showPopupResponseValues resp))
            button = TL.toStrict (PP.showPopupResponseButton resp)
         in case responseJson of
              Just vals ->
                pure $ successResult $ Aeson.toJSON $ PopupResponse button vals
              Nothing ->
                pure $ successResult $ Aeson.toJSON $ PopupResponse button (object [])
