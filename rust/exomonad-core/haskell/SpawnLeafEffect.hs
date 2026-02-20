{-# LANGUAGE GADTs, DataKinds, TypeOperators, FlexibleContexts #-}
module SpawnLeafEffect where

import Control.Monad.Freer

-- Effect GADT. Constructor names must match Rust #[core(name = "...")] exactly.
data SpawnLeafOp a where
  GetToolInput :: SpawnLeafOp SpawnLeafInput
  SpawnLeaf    :: String -> String -> SpawnLeafOp SpawnLeafResult

-- Input from MCP args.
data SpawnLeafInput = SpawnLeafInput
  { sliTask       :: String
  , sliBranchName :: String
  }

-- Result returned to caller.
data SpawnLeafResult = SpawnLeafResult
  { slrTabName    :: String
  , slrBranchName :: String
  }

-- Entry point: zero-arg, all input via effects.
spawnLeafTool :: Eff '[SpawnLeafOp] SpawnLeafResult
spawnLeafTool = do
  input <- send GetToolInput
  send (SpawnLeaf (sliTask input) (sliBranchName input))
