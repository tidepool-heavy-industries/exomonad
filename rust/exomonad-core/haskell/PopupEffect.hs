{-# LANGUAGE GADTs, DataKinds, TypeOperators, FlexibleContexts #-}
module PopupEffect where

import Control.Monad.Freer

-- Effect GADT. Constructor names must match Rust #[core(name = "...")] exactly.
data Popup a where
  GetToolInput :: Popup ToolInput
  ShowPopup    :: String -> String -> Popup PopupResponse

-- Types mirror Rust FromCore/ToCore structs (names must match exactly).
data ToolInput = ToolInput
  { tiTitle      :: String
  , tiComponents :: String
  }

data PopupResponse = PopupResponse
  { prButton :: String
  , prValues :: String
  }

-- Smart constructors
getToolInput :: Member Popup effs => Eff effs ToolInput
getToolInput = send GetToolInput

showPopup :: Member Popup effs => String -> String -> Eff effs PopupResponse
showPopup title components = send (ShowPopup title components)

-- Entry point: zero-arg, all input via effects.
-- POC passthrough — later Haskell adds validation, wizard flows, etc.
popupTool :: Eff '[Popup] PopupResponse
popupTool = do
  input <- getToolInput
  showPopup (tiTitle input) (tiComponents input)
