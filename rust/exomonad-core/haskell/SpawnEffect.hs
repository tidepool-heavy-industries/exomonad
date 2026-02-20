{-# LANGUAGE GADTs, DataKinds, TypeOperators, FlexibleContexts #-}
module SpawnEffect where

import Control.Monad.Freer

-- Effect GADT. Constructor names must match Rust #[core(name = "...")] exactly.
data SpawnSubtreeOp a where
  GetToolInput  :: SpawnSubtreeOp SpawnSubtreeInput
  SpawnSubtree  :: String -> String -> String -> SpawnSubtreeOp SpawnSubtreeResult

-- Input from MCP args.
data SpawnSubtreeInput = SpawnSubtreeInput
  { ssiTask            :: String
  , ssiBranchName      :: String
  , ssiParentSessionId :: String
  }

-- Result returned to caller.
data SpawnSubtreeResult = SpawnSubtreeResult
  { ssrTabName    :: String
  , ssrBranchName :: String
  }

-- Entry point: zero-arg, all input via effects.
spawnSubtreeTool :: Eff '[SpawnSubtreeOp] SpawnSubtreeResult
spawnSubtreeTool = do
  input <- send GetToolInput
  send (SpawnSubtree (ssiTask input) (ssiBranchName input) (ssiParentSessionId input))
