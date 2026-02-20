{-# LANGUAGE GADTs, DataKinds, TypeOperators, FlexibleContexts #-}
module FilePREffect where

import Control.Monad.Freer

-- Per-tool input effect (tag 0 in HList)
data FilePRInput' a where
  GetToolInput :: FilePRInput' FilePRInput

data FilePRInput = FilePRInput
  { fprTitle      :: String
  , fprBody       :: String
  , fprBaseBranch :: String
  }

-- Shared Identity effect (tag 1 in HList)
-- All constructors declared for DataConTable completeness.
data Identity a where
  GetAgentId   :: Identity String
  GetParentTab :: Identity String
  GetOwnTab    :: Identity String
  GetWorkingDir :: Identity String

-- Per-tool domain op (tag 2 in HList)
data FilePROp a where
  CreateOrUpdatePR :: String -> String -> String -> String -> FilePROp FilePRResult

-- Result returned to caller.
data FilePRResult = FilePRResult
  { fprPrUrl      :: String
  , fprPrNumber   :: String
  , fprHeadBranch :: String
  , fprResultBase :: String
  , fprCreated    :: String
  }

-- Entry point: multi-effect composition.
-- Working dir resolution in Haskell via Identity.
filePRTool :: Eff '[FilePRInput', Identity, FilePROp] FilePRResult
filePRTool = do
  input <- send GetToolInput
  workingDir <- send GetWorkingDir
  send (CreateOrUpdatePR (fprTitle input) (fprBody input) (fprBaseBranch input) workingDir)
