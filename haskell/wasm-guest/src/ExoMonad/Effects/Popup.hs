{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Popup effects for showing interactive UI dialogs.
--
-- All effects are dispatched via the @popup@ namespace.
-- Request and response types are proto-generated from @proto/effects/popup.proto@.
module ExoMonad.Effects.Popup
  ( -- * Effect Types
    PopupShowPopup,

    -- * Smart Constructors
    showPopup,

    -- * Re-exported proto types
    module Effects.Popup,
  )
where

import Effects.EffectError (EffectError)
import Effects.Popup
import ExoMonad.Effect.Class (Effect (..), runEffect)

-- ============================================================================
-- Effect phantom types + instances
-- ============================================================================

data PopupShowPopup

instance Effect PopupShowPopup where
  type Input PopupShowPopup = ShowPopupRequest
  type Output PopupShowPopup = ShowPopupResponse
  effectId = "popup.show_popup"

-- ============================================================================
-- Smart constructors
-- ============================================================================

showPopup :: ShowPopupRequest -> IO (Either EffectError ShowPopupResponse)
showPopup = runEffect @PopupShowPopup
