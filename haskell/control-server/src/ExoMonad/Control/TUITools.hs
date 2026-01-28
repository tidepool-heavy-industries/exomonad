{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | General-purpose popup MCP tool.
module ExoMonad.Control.TUITools
  ( -- * Popup Tool
    popupLogic
  , PopupArgs(..)
  , PopupResult(..)

    -- * Element Types
  , PopupElement(..)
  , PopupResultElement(..)
  ) where

import Control.Monad.Freer (Eff, Member)
import Data.Aeson
  ( FromJSON(..), ToJSON(..), Value(..), (.:), (.:?), (.=)
  , object, withObject
  )
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import ExoMonad.Effect.TUI
  ( TUI, showUI, PopupDefinition(..), Component(..), ComponentSpec(..)
  )
import qualified ExoMonad.Effect.TUI as TUI (PopupResult(..))

import ExoMonad.Control.TUITools.Types

-- ════════════════════════════════════════════════════════════════════════════
-- POPUP LOGIC
-- ════════════════════════════════════════════════════════════════════════════

-- | Core logic for popup tool.
--
-- Converts the input elements to the internal PopupDefinition format,
-- shows the UI, and then zips values back into the result elements.
popupLogic
  :: (Member TUI es)
  => PopupArgs
  -> Eff es PopupResult
popupLogic args = do
  -- Convert to internal TUI format
  let internalDef = toPopupDefinition args

  -- Show UI and get result
  TUI.PopupResult button valuesMap <- showUI internalDef

  -- Zip values back into element structure
  let resultElements = zipWithValues (args.elements) valuesMap

  pure $ PopupResult
    { status = if button == "submit" then "completed" else "cancelled"
    , button = button
    , elements = resultElements
    }

-- | Convert PopupArgs to internal PopupDefinition.
toPopupDefinition :: PopupArgs -> PopupDefinition
toPopupDefinition args = PopupDefinition
  { pdTitle = maybe "Popup" id (args.title)
  , pdComponents = map toComponent (args.elements)
  }
  where
    toComponent :: PopupElement -> Component
    toComponent = \case
      PText eid content ->
        Component eid (Text content) Nothing
      PSlider eid label minV maxV defV ->
        Component eid (Slider label minV maxV (maybe ((minV + maxV) / 2) id defV)) Nothing
      PCheckbox eid label defV ->
        Component eid (Checkbox label (maybe False id defV)) Nothing
      PTextbox eid label placeholder ->
        Component eid (Textbox label placeholder Nothing) Nothing
      PChoice eid label opts defIdx ->
        Component eid (Choice label opts defIdx) Nothing
      PMultiselect eid label opts ->
        Component eid (Multiselect label opts) Nothing
      PGroup eid label ->
        Component eid (Group label) Nothing

-- | Zip values from the result map back into elements.
zipWithValues :: [PopupElement] -> Value -> [PopupResultElement]
zipWithValues elements valuesObj = map zipOne elements
  where
    valuesMap = case valuesObj of
      Object km -> km
      _ -> KM.empty

    getValue :: Text -> Maybe Value
    getValue eid = KM.lookup (Key.fromText eid) valuesMap

    getTextValue :: Text -> Text
    getTextValue eid = case getValue eid of
      Just (String t) -> t
      _ -> ""

    getDoubleValue :: Text -> Double -> Double
    getDoubleValue eid def = case getValue eid of
      Just (Number n) -> realToFrac n
      _ -> def

    getBoolValue :: Text -> Bool -> Bool
    getBoolValue eid def = case getValue eid of
      Just (Bool b) -> b
      _ -> def

    getTextArrayValue :: Text -> [Text]
    getTextArrayValue eid = case getValue eid of
      Just (Array arr) -> [t | String t <- toList arr]
      _ -> []
      where
        toList = foldr (:) []

    zipOne :: PopupElement -> PopupResultElement
    zipOne = \case
      PText eid content ->
        RText eid content
      PSlider eid label minV maxV defV ->
        RSlider eid label (getDoubleValue eid (maybe ((minV + maxV) / 2) id defV))
      PCheckbox eid label defV ->
        RCheckbox eid label (getBoolValue eid (maybe False id defV))
      PTextbox eid label _ ->
        RTextbox eid label (getTextValue eid)
      PChoice eid label opts defIdx ->
        let defaultOpt = case defIdx of
              Just idx | idx >= 0 && idx < length opts -> opts !! idx
              _ -> ""
            selectedText = case getTextValue eid of
              "" -> defaultOpt
              t -> t
        in RChoice eid label selectedText
      PMultiselect eid label _ ->
        RMultiselect eid label (getTextArrayValue eid)
      PGroup eid label ->
        RGroup eid label