{-# LANGUAGE GADTs, DataKinds, TypeOperators, FlexibleContexts #-}
module FilePREffect where

import Control.Monad.Freer

-- Effect GADT. Constructor names must match Rust #[core(name = "...")] exactly.
data FilePR a where
  GetToolInput     :: FilePR FilePRInput
  CreateOrUpdatePR :: String -> String -> String -> FilePR FilePRResult

-- Input from MCP args. All fields String; Rust converts as needed.
data FilePRInput = FilePRInput
  { fprTitle      :: String
  , fprBody       :: String
  , fprBaseBranch :: String
  }

-- Result returned to caller. Bool fields as String ("true"/"false").
data FilePRResult = FilePRResult
  { fprPrUrl      :: String
  , fprPrNumber   :: String
  , fprHeadBranch :: String
  , fprResultBase :: String
  , fprCreated    :: String
  }

-- Entry point: zero-arg, all input via effects.
filePRTool :: Eff '[FilePR] FilePRResult
filePRTool = do
  input <- send GetToolInput
  send (CreateOrUpdatePR (fprTitle input) (fprBody input) (fprBaseBranch input))
