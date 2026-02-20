{-# LANGUAGE GADTs, DataKinds, TypeOperators, FlexibleContexts #-}
module PopupMinimal where

import Control.Monad.Freer

data Popup a where
  GetToolInput :: Popup Int

-- Simplest possible: one effect yield, no chaining, no records
minimal :: Eff '[Popup] Int
minimal = send GetToolInput
