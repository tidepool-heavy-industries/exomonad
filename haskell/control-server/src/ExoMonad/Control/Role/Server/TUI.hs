{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoStarIsType #-}

-- | TUI Server for the Role DSL.
--
-- Interactive UI tools that display dialogs to the user.
module ExoMonad.Control.Role.Server.TUI
  ( TUIServer(..)
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import GHC.Generics (Generic)

import ExoMonad.Control.Role.Types (ToolField)
import ExoMonad.Control.Role.Tool.Popup (Popup)

-- | MCP Server for TUI-interactive tools.
data TUIServer mode (es :: [Type -> Type]) = TUIServer
  { tuiDescription :: Text
    -- ^ Server description for MCP listing
  , tuiPopup :: ToolField mode es Popup
    -- ^ Interactive popup dialogs
  }
  deriving Generic
