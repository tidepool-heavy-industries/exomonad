{-# LANGUAGE GADTs, DataKinds, TypeOperators, FlexibleContexts #-}
module MessagesEffect where

import Control.Monad.Freer

-- Effect GADT. Constructor names must match Rust #[core(name = "...")] exactly.
data MessagesOp a where
  GetToolInput :: MessagesOp MessagesInput
  GetMessages  :: String -> String -> MessagesOp MessagesResult

-- Input from MCP args. timeout_secs as String; Rust parses to i32.
data MessagesInput = MessagesInput
  { miAgentId     :: String
  , miTimeoutSecs :: String
  }

-- Result returned to caller. messages_json is the serialized JSON string.
data MessagesResult = MessagesResult
  { mrMessagesJson :: String
  , mrWarning      :: String
  }

-- Entry point: zero-arg, all input via effects.
messagesTool :: Eff '[MessagesOp] MessagesResult
messagesTool = do
  input <- send GetToolInput
  send (GetMessages (miAgentId input) (miTimeoutSecs input))
