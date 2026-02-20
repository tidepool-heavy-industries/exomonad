{-# LANGUAGE GADTs, DataKinds, TypeOperators, FlexibleContexts #-}
module SpawnLeafEffect where

import Control.Monad.Freer

-- Per-tool input effect (tag 0 in HList)
data SpawnLeafInput' a where
  GetToolInput :: SpawnLeafInput' SpawnLeafInput

data SpawnLeafInput = SpawnLeafInput
  { sliTask       :: String
  , sliBranchName :: String
  }

-- Per-tool domain op (tag 1 in HList)
data SpawnLeafOp a where
  SpawnLeaf :: String -> String -> SpawnLeafOp SpawnLeafResult

data SpawnLeafResult = SpawnLeafResult
  { slrTabName    :: String
  , slrBranchName :: String
  }

-- Entry point: multi-effect composition.
spawnLeafTool :: Eff '[SpawnLeafInput', SpawnLeafOp] SpawnLeafResult
spawnLeafTool = do
  input <- send GetToolInput
  send (SpawnLeaf (sliTask input) (sliBranchName input))
