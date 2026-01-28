{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | Popup tool definition for the Role DSL.
module ExoMonad.Control.Role.Tool.Popup
  ( Popup(..)
  , PopupArgs(..)
  , PopupResult(..)
  ) where

import ExoMonad.Control.Role.Types (ToolSpec(..))
import ExoMonad.Control.TUITools.Types (PopupArgs(..), PopupResult(..))

-- | Marker type for the Popup tool.
data Popup = Popup

instance ToolSpec Popup where
  type Args Popup = PopupArgs
  type Result Popup = PopupResult
  toolName = "popup"
  toolDescription = "Display an interactive popup dialog with various UI elements (sliders, checkboxes, text inputs, choices). Returns user selections."
