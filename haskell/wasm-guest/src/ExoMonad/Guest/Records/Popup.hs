{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module ExoMonad.Guest.Records.Popup
  ( PopupTools (..),
    popupToolsHandler,
    popupToolsSchema,
  )
where

import ExoMonad.Guest.Tool.Mode (AsHandler, AsSchema, ToolMode ((:-)), mkHandler, mkSchema)
import ExoMonad.Guest.Tools.Popup (Popup)
import GHC.Generics (Generic)

-- | Popup tools record.
data PopupTools mode = PopupTools
  { popupTool :: mode :- Popup
  }
  deriving (Generic)

-- | Popup tools handler record.
popupToolsHandler :: PopupTools AsHandler
popupToolsHandler =
  PopupTools
    { popupTool = mkHandler @Popup
    }

-- | Popup tools schema record.
popupToolsSchema :: PopupTools AsSchema
popupToolsSchema =
  PopupTools
    { popupTool = mkSchema @Popup
    }
