{-# LANGUAGE GADTs, DataKinds, TypeOperators, FlexibleContexts #-}
module PopupEffect where

import Control.Monad.Freer

-- Per-tool input effect (tag 0 in HList)
data PopupInput' a where
  GetToolInput :: PopupInput' ToolInput

data ToolInput = ToolInput
  { tiTitle      :: String
  , tiComponents :: String
  }

-- Shared Identity effect (tag 1 in HList)
-- All constructors declared for DataConTable completeness.
data Identity a where
  GetAgentId   :: Identity String
  GetParentTab :: Identity String
  GetOwnTab    :: Identity String
  GetWorkingDir :: Identity String

-- Per-tool domain op (tag 2 in HList)
data PopupOp a where
  ShowPopup :: String -> String -> String -> PopupOp PopupResponse

data PopupResponse = PopupResponse
  { prButton :: String
  , prValues :: String
  }

-- Entry point: multi-effect composition.
-- Own tab resolution via Identity effect.
popupTool :: Eff '[PopupInput', Identity, PopupOp] PopupResponse
popupTool = do
  input <- send GetToolInput
  ownTab <- send GetOwnTab
  send (ShowPopup (tiTitle input) (tiComponents input) ownTab)
