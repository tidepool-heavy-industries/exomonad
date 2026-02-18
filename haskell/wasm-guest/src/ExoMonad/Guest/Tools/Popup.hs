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
    WizardPane (..),
  )
where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withObject, withText, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict (HashMap)
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
    paElements :: [PopupElement],
    paPanes :: Maybe (HashMap Text WizardPane),
    paStart :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON PopupArgs where
  parseJSON = withObject "PopupArgs" $ \v ->
    PopupArgs
      <$> v .:? "title"
      <*> (v .: "elements" <|> pure [])
      <*> v .:? "panes"
      <*> v .:? "start"

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

-- | A pane in a wizard popup.
data WizardPane = WizardPane
  { wpTitle :: Text,
    wpElements :: [PopupElement],
    wpThen :: Maybe Value
  }
  deriving (Show, Eq, Generic)

instance FromJSON WizardPane where
  parseJSON = withObject "WizardPane" $ \v ->
    WizardPane
      <$> v .: "title"
      <*> v .: "elements"
      <*> v .:? "then"

instance ToJSON WizardPane where
  toJSON (WizardPane title elems mThen) =
    object $
      [ "title" .= title,
        "elements" .= zipWith convertWizardElement [0 ..] elems
      ]
        ++ maybe [] (\t -> ["then_transition" .= t]) mThen
    where
      convertWizardElement :: Int -> PopupElement -> Value
      convertWizardElement idx el =
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
  case paPanes args of
    Just panes ->
      let wizardJson =
            object
              [ "title" .= maybe "Wizard" id (paTitle args),
                "panes" .= fmap toJSON panes,
                "start" .= maybe "" id (paStart args)
              ]
       in PP.ShowPopupRequest
            { PP.showPopupRequestTitle = TL.fromStrict (maybe "Wizard" id (paTitle args)),
              PP.showPopupRequestComponents = BL.toStrict (Aeson.encode wizardJson)
            }
    Nothing ->
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
                  ],
              "panes"
                .= object
                  [ "type" .= ("object" :: Text),
                    "description" .= ("Named wizard panes. Use with 'start' for multi-pane wizard mode." :: Text),
                    "additionalProperties"
                      .= object
                        [ "type" .= ("object" :: Text),
                          "required" .= (["title", "elements"] :: [Text]),
                          "properties"
                            .= object
                              [ "title" .= object ["type" .= ("string" :: Text)],
                                "elements" .= object ["type" .= ("array" :: Text), "items" .= object ["type" .= ("object" :: Text)]],
                                "then" .= object ["description" .= ("Transition: string (goto) or object (branch by field value)" :: Text)]
                              ]
                        ]
                  ],
              "start"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Starting pane name (required when using panes)" :: Text)
                  ]
            ],
        "required" .= ([] :: [Text])
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
