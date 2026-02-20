{-# LANGUAGE GADTs, DataKinds, TypeOperators, FlexibleContexts #-}
module PreludeTest where

import Control.Monad.Freer

data TestInput' a where
  GetToolInput :: TestInput' TestInput

data TestInput = TestInput
  { tiStatus  :: String
  , tiMessage :: String
  }

data TestOp a where
  Echo :: String -> TestOp String

-- Test: null check on string (WORKS on tidepool main)
testNull :: Eff '[TestInput', TestOp] String
testNull = do
  input <- send GetToolInput
  if null (tiMessage input)
    then send (Echo "empty")
    else send (Echo (tiMessage input))
