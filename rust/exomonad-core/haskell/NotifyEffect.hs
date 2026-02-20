{-# LANGUAGE GADTs, DataKinds, TypeOperators, FlexibleContexts #-}
module NotifyEffect where

import Control.Monad.Freer

-- Effect GADT. Constructor names must match Rust #[core(name = "...")] exactly.
data Notify a where
  GetToolInput  :: Notify NotifyInput
  NotifyParent  :: String -> String -> Notify NotifyResult

-- Input from MCP args. Rust pre-composes the rich message (pr_number + tasks).
data NotifyInput = NotifyInput
  { niStatus  :: String
  , niMessage :: String
  }

-- Result returned to caller.
data NotifyResult = NotifyResult
  { nrAck :: String
  }

-- Entry point: zero-arg, all input via effects.
notifyTool :: Eff '[Notify] NotifyResult
notifyTool = do
  input <- send GetToolInput
  send (NotifyParent (niStatus input) (niMessage input))
