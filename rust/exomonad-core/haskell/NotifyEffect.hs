{-# LANGUAGE GADTs, DataKinds, TypeOperators, FlexibleContexts #-}
module NotifyEffect where

import Control.Monad.Freer

-- Per-tool input effect (tag 0 in HList)
data NotifyInput' a where
  GetToolInput :: NotifyInput' NotifyInput

data NotifyInput = NotifyInput
  { niStatus  :: String
  , niMessage :: String
  }

-- Shared Identity effect (tag 1 in HList)
-- All constructors declared for DataConTable completeness.
data Identity a where
  GetAgentId   :: Identity String
  GetParentTab :: Identity String
  GetOwnTab    :: Identity String
  GetWorkingDir :: Identity String

-- Shared FormatOp effect (tag 2 in HList)
-- Temporary bridge until Tidepool supports Prelude functions.
data FormatOp a where
  FormatNotification :: String -> String -> String -> FormatOp String
  FormatNoteSubject  :: String -> FormatOp String
  DefaultMessage     :: String -> String -> FormatOp String
  FormatNote         :: String -> String -> FormatOp String

-- Per-tool domain op (tag 3 in HList)
data NotifyOp a where
  DeliverNotification :: String -> String -> NotifyOp NotifyResult

-- Result returned to caller.
data NotifyResult = NotifyResult
  { nrAck :: String
  }

-- Entry point: multi-effect composition.
-- Flow: get identity → default empty message → format notification → deliver.
notifyTool :: Eff '[NotifyInput', Identity, FormatOp, NotifyOp] NotifyResult
notifyTool = do
  input <- send GetToolInput
  agentId <- send GetAgentId
  parentTab <- send GetParentTab
  msg <- send (DefaultMessage (niStatus input) (niMessage input))
  formatted <- send (FormatNotification (niStatus input) msg agentId)
  send (DeliverNotification parentTab formatted)
