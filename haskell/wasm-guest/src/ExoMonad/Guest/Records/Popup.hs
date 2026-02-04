-- | Popup tool record and handlers.
--
-- Provides the popup MCP tool for interactive user input.
module ExoMonad.Guest.Records.Popup
  ( PopupTools (..),
    popupToolsHandler,
    popupToolsSchema,
    popupTools,
  )
where

import ExoMonad.Guest.Tool.Mode (AsHandler, AsSchema, ToolMode ((:-)), mkHandler, mkSchema)
import ExoMonad.Guest.Tools.Popup (Popup)
import GHC.Generics (Generic)

-- | Popup tools record.
data PopupTools mode = PopupTools
  { popup :: mode :- Popup
  }
  deriving (Generic)

-- | Popup tools handler record.
popupToolsHandler :: PopupTools AsHandler
popupToolsHandler =
  PopupTools
    { popup = mkHandler @Popup
    }

-- | Popup tools schema record.
popupToolsSchema :: PopupTools AsSchema
popupToolsSchema =
  PopupTools
    { popup = mkSchema @Popup
    }

-- | Default handler instance for use in Role.hs
popupTools :: PopupTools AsHandler
popupTools = popupToolsHandler
