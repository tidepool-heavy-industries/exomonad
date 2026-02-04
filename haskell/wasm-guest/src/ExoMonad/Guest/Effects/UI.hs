{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module ExoMonad.Guest.Effects.UI
  ( -- * Effect
    UI (..),
    -- * Actions
    showPopup,
    -- * Interpretations
    runUI,
    -- * Re-exports
    PopupDefinition (..),
    PopupResult (..),
  )
where

import qualified Data.Text as T
import ExoMonad.Guest.HostCall (callHost, host_ui_show_popup)
import ExoMonad.Guest.TUI (PopupDefinition (..), PopupResult (..))
import Polysemy (Embed, Member, Sem, embed, interpret, send)

-- | UI Effect
data UI m a where
  ShowPopup :: PopupDefinition -> UI m (Either T.Text PopupResult)

-- | Smart constructor (manual implementation to avoid TH issues in WASM)
showPopup :: Member UI r => PopupDefinition -> Sem r (Either T.Text PopupResult)
showPopup def = send (ShowPopup def)

-- | Interpret UI effect via host calls
runUI :: (Member (Embed IO) r) => Sem (UI ': r) a -> Sem r a
runUI = interpret $ \case
  ShowPopup def -> do
    embed $ do
      res <- callHost host_ui_show_popup def
      case res of
        Left err -> pure (Left err)
        Right val -> pure (Right val)
