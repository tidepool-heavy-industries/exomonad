{-# LANGUAGE GADTs, DataKinds, TypeOperators, FlexibleContexts #-}
module NoteEffect where

import Control.Monad.Freer

-- Effect GADT. Constructor names must match Rust #[core(name = "...")] exactly.
data NoteOp a where
  GetToolInput :: NoteOp NoteInput
  SendNote     :: String -> NoteOp NoteResult

-- Input from MCP args.
data NoteInput = NoteInput
  { niContent :: String
  }

-- Result returned to caller.
data NoteResult = NoteResult
  { nrAck :: String
  }

-- Entry point: zero-arg, all input via effects.
noteTool :: Eff '[NoteOp] NoteResult
noteTool = do
  input <- send GetToolInput
  send (SendNote (niContent input))
