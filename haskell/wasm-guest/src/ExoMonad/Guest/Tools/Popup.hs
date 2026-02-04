{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module ExoMonad.Guest.Tools.Popup where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text (Text)
import ExoMonad.Guest.Effects.UI (PopupDefinition, PopupResult, showPopup, runUI)
import ExoMonad.Guest.Tool.Class
import Polysemy (runM)

-- | Popup tool marker
data Popup

instance MCPTool Popup where
  type ToolArgs Popup = PopupDefinition
  
  toolName = "popup"
  
  toolDescription = "Display an interactive TUI popup form to the user. Use this to collect structured input, choices, or confirmation from the human."
  
  -- Simplified schema that accepts the structure loosely
  toolSchema = object
    [ "type" .= ("object" :: Text)
    , "properties" .= object
        [ "title" .= object 
            [ "type" .= ("string" :: Text)
            , "description" .= ("Title of the popup form" :: Text)
            ]
        , "components" .= object
            [ "type" .= ("array" :: Text)
            , "description" .= ("List of UI components (text, slider, checkbox, choice, etc.)" :: Text)
            , "items" .= object
                [ "type" .= ("object" :: Text)
                , "properties" .= object
                    [ "type" .= object ["type" .= ("string" :: Text), "enum" .= (["text", "slider", "checkbox", "textbox", "choice", "multiselect", "group"] :: [Text])]
                    , "id" .= object ["type" .= ("string" :: Text)]
                    , "label" .= object ["type" .= ("string" :: Text)]
                    -- Other fields are optional/variant specific, let's keep schema permissive
                    ]
                , "required" .= (["type", "id"] :: [Text])
                ]
            ]
        ]
    , "required" .= (["title", "components"] :: [Text])
    ]
  
handler :: (Member UI r) => PopupInput -> Sem r Value
handler (PopupInput title components) = do
  let definition = PopupDefinition title components
  res <- showPopup definition
  case res of
    Left err -> pure $ object ["error" .= err]
    Right result -> pure $ toJSON result
