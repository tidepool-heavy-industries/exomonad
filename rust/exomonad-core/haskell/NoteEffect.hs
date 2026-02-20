{-# LANGUAGE GADTs, DataKinds, TypeOperators, FlexibleContexts #-}
module NoteEffect where

import Control.Monad.Freer

-- Per-tool input effect (tag 0 in HList)
data NoteInput' a where
  GetToolInput :: NoteInput' NoteInput

data NoteInput = NoteInput
  { niContent :: String
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

-- Shared Inbox effect (tag 3 in HList)
data Inbox a where
  WriteMessage :: String -> String -> String -> String -> Inbox String
  ReadMessages :: String -> Inbox String
  PollMessages :: String -> String -> Inbox String

-- Per-tool domain op (tag 4 in HList)
data NoteOp a where
  InjectNote :: String -> String -> NoteOp NoteResult

-- Result returned to caller.
data NoteResult = NoteResult
  { nrAck :: String
  }

-- Entry point: multi-effect composition.
-- Flow: get input → get identity → truncate subject → write inbox → format → inject.
noteTool :: Eff '[NoteInput', Identity, FormatOp, Inbox, NoteOp] NoteResult
noteTool = do
  input <- send GetToolInput
  agentId <- send GetAgentId
  parentTab <- send GetParentTab
  subject <- send (FormatNoteSubject (niContent input))
  _ <- send (WriteMessage "team-lead" agentId (niContent input) subject)
  formatted <- send (FormatNote agentId (niContent input))
  send (InjectNote parentTab formatted)
