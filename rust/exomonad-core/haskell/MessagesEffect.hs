{-# LANGUAGE GADTs, DataKinds, TypeOperators, FlexibleContexts #-}
module MessagesEffect where

import Control.Monad.Freer

-- Per-tool input effect (tag 0 in HList)
data MessagesInput' a where
  GetToolInput :: MessagesInput' MessagesInput

data MessagesInput = MessagesInput
  { miAgentId     :: String
  , miTimeoutSecs :: String
  }

-- Per-tool domain op (tag 1 in HList)
data MessagesOp a where
  FetchMessages :: String -> String -> MessagesOp MessagesResult

-- Result returned to caller.
data MessagesResult = MessagesResult
  { mrMessagesJson :: String
  , mrWarning      :: String
  }

-- Entry point: multi-effect composition.
-- Passes args to MessagesOp for Rust to handle timeout branching + inbox read.
messagesTool :: Eff '[MessagesInput', MessagesOp] MessagesResult
messagesTool = do
  input <- send GetToolInput
  send (FetchMessages (miAgentId input) (miTimeoutSecs input))
