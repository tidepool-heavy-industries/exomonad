{-# LANGUAGE GADTs, DataKinds, TypeOperators, FlexibleContexts #-}
module MergePREffect where

import Control.Monad.Freer

-- Per-tool input effect (tag 0 in HList)
data MergePRInput' a where
  GetToolInput :: MergePRInput' MergePRInput

data MergePRInput = MergePRInput
  { mprPrNumber :: String
  , mprStrategy :: String
  }

-- Shared Identity effect (tag 1 in HList)
-- All constructors declared for DataConTable completeness.
data Identity a where
  GetAgentId   :: Identity String
  GetParentTab :: Identity String
  GetOwnTab    :: Identity String
  GetWorkingDir :: Identity String

-- Per-tool domain op (tag 2 in HList)
data MergePROp a where
  MergePullRequest :: String -> String -> String -> MergePROp MergePRResult

-- Result returned to caller.
data MergePRResult = MergePRResult
  { mprSuccess   :: String
  , mprMessage   :: String
  , mprJjFetched :: String
  }

-- Entry point: multi-effect composition.
-- Working dir from Identity, no longer in MCP args.
mergePRTool :: Eff '[MergePRInput', Identity, MergePROp] MergePRResult
mergePRTool = do
  input <- send GetToolInput
  workingDir <- send GetWorkingDir
  send (MergePullRequest (mprPrNumber input) (mprStrategy input) workingDir)
