{-# LANGUAGE GADTs, DataKinds, TypeOperators, FlexibleContexts #-}
module MergePREffect where

import Control.Monad.Freer

-- Effect GADT. Constructor names must match Rust #[core(name = "...")] exactly.
data MergePR a where
  GetToolInput    :: MergePR MergePRInput
  MergePullRequest :: String -> String -> String -> MergePR MergePRResult

-- Input from MCP args. pr_number as String; Rust parses to i64.
data MergePRInput = MergePRInput
  { mprPrNumber   :: String
  , mprStrategy   :: String
  , mprWorkingDir :: String
  }

-- Result returned to caller. Bool fields as String ("true"/"false").
data MergePRResult = MergePRResult
  { mprSuccess   :: String
  , mprMessage   :: String
  , mprJjFetched :: String
  }

-- Entry point: zero-arg, all input via effects.
mergePRTool :: Eff '[MergePR] MergePRResult
mergePRTool = do
  input <- send GetToolInput
  send (MergePullRequest (mprPrNumber input) (mprStrategy input) (mprWorkingDir input))
