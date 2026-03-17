{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

-- | Shim module for legacy dev state types.
module ExoMonad.Guest.Lifecycle.DevState
  ( DevPhase (..),
    DevState (..),
    SomeDevState (..),
    PRNumber,
    URL,
    describeDevPhase,
  )
where

import Data.Text (Text)
import Data.Text qualified as T

type PRNumber = Int
type URL = Text

data DevPhase
  = Spawned
  | Working
  | PRFiled
  | UnderReview
  | ChangesRequested
  | Approved
  | Done
  | Failed

data DevState (p :: DevPhase) where
  SSpawned :: DevState 'Spawned
  SWorking :: DevState 'Working
  SPRFiled :: PRNumber -> URL -> DevState 'PRFiled
  SUnderReview :: PRNumber -> Int -> DevState 'UnderReview
  SChangesRequested :: PRNumber -> [Text] -> DevState 'ChangesRequested
  SApproved :: PRNumber -> DevState 'Approved
  SDone :: DevState 'Done
  SFailed :: Text -> DevState 'Failed

deriving instance Show (DevState p)

data SomeDevState where
  SomeDevState :: DevState p -> SomeDevState

deriving instance Show SomeDevState

describeDevPhase :: SomeDevState -> Text
describeDevPhase (SomeDevState SSpawned) = "spawned"
describeDevPhase (SomeDevState SWorking) = "working"
describeDevPhase (SomeDevState (SPRFiled n _)) = "pr_filed (#" <> T.pack (show n) <> ")"
describeDevPhase (SomeDevState (SUnderReview n r)) = "under_review (#" <> T.pack (show n) <> ", round " <> T.pack (show r) <> ")"
describeDevPhase (SomeDevState (SChangesRequested n _)) = "changes_requested (#" <> T.pack (show n) <> ")"
describeDevPhase (SomeDevState (SApproved n)) = "approved (#" <> T.pack (show n) <> ")"
describeDevPhase (SomeDevState SDone) = "done"
describeDevPhase (SomeDevState (SFailed msg)) = "failed: " <> msg
