{-# LANGUAGE GADTs, DataKinds, TypeOperators, FlexibleContexts #-}
module SpawnEffect where

import Control.Monad.Freer

-- Per-tool input effect (tag 0 in HList)
data SpawnSubtreeInput' a where
  GetToolInput :: SpawnSubtreeInput' SpawnSubtreeInput

data SpawnSubtreeInput = SpawnSubtreeInput
  { ssiTask       :: String
  , ssiBranchName :: String
  }

-- Per-tool domain op (tag 1 in HList)
data SpawnSubtreeOp a where
  SpawnSubtree :: String -> String -> SpawnSubtreeOp SpawnSubtreeResult

data SpawnSubtreeResult = SpawnSubtreeResult
  { ssrTabName    :: String
  , ssrBranchName :: String
  }

-- Entry point: multi-effect composition.
spawnSubtreeTool :: Eff '[SpawnSubtreeInput', SpawnSubtreeOp] SpawnSubtreeResult
spawnSubtreeTool = do
  input <- send GetToolInput
  send (SpawnSubtree (ssiTask input) (ssiBranchName input))
