{-# LANGUAGE StandaloneDeriving #-}

-- | GADT-indexed dev agent lifecycle phases.
--
-- Each phase carries exactly the data relevant to that state.
-- The existential 'SomeDevState' wraps for KV persistence.
module ExoMonad.Guest.Lifecycle.DevState
  ( DevPhase (..),
    DevState (..),
    SomeDevState (..),
    PRNumber,
    ReviewRound,
    URL,
    describeDevPhase,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.=))
import Data.Text (Text)
import Data.Text qualified as T

type PRNumber = Int

type ReviewRound = Int

type URL = Text

-- | Dev agent lifecycle phases (promoted to kinds via DataKinds).
data DevPhase
  = Spawned
  | Working
  | PRFiled
  | UnderReview
  | ChangesRequested
  | Approved
  | Done
  | Failed

-- | GADT-indexed dev state. Each constructor carries phase-specific data.
data DevState (p :: DevPhase) where
  SSpawned :: DevState 'Spawned
  SWorking :: DevState 'Working
  SPRFiled :: PRNumber -> URL -> DevState 'PRFiled
  SUnderReview :: PRNumber -> ReviewRound -> DevState 'UnderReview
  SChangesRequested :: PRNumber -> [Text] -> DevState 'ChangesRequested
  SApproved :: PRNumber -> DevState 'Approved
  SDone :: DevState 'Done
  SFailed :: Text -> DevState 'Failed

deriving instance Show (DevState p)

-- | Existential wrapper for runtime phase tracking.
data SomeDevState where
  SomeDevState :: DevState p -> SomeDevState

deriving instance Show SomeDevState

-- | Human-readable phase description.
describeDevPhase :: SomeDevState -> Text
describeDevPhase (SomeDevState SSpawned) = "spawned"
describeDevPhase (SomeDevState SWorking) = "working"
describeDevPhase (SomeDevState (SPRFiled n _)) = "pr_filed (#" <> T.pack (show n) <> ")"
describeDevPhase (SomeDevState (SUnderReview n r)) = "under_review (#" <> T.pack (show n) <> ", round " <> T.pack (show r) <> ")"
describeDevPhase (SomeDevState (SChangesRequested n _)) = "changes_requested (#" <> T.pack (show n) <> ")"
describeDevPhase (SomeDevState (SApproved n)) = "approved (#" <> T.pack (show n) <> ")"
describeDevPhase (SomeDevState SDone) = "done"
describeDevPhase (SomeDevState (SFailed msg)) = "failed: " <> msg

instance ToJSON SomeDevState where
  toJSON (SomeDevState SSpawned) = object ["phase" .= ("dev_spawned" :: Text)]
  toJSON (SomeDevState SWorking) = object ["phase" .= ("dev_working" :: Text)]
  toJSON (SomeDevState (SPRFiled n url)) = object ["phase" .= ("dev_pr_filed" :: Text), "pr_number" .= n, "url" .= url]
  toJSON (SomeDevState (SUnderReview n r)) = object ["phase" .= ("dev_under_review" :: Text), "pr_number" .= n, "review_round" .= r]
  toJSON (SomeDevState (SChangesRequested n cs)) = object ["phase" .= ("dev_changes_requested" :: Text), "pr_number" .= n, "comments" .= cs]
  toJSON (SomeDevState (SApproved n)) = object ["phase" .= ("dev_approved" :: Text), "pr_number" .= n]
  toJSON (SomeDevState SDone) = object ["phase" .= ("dev_done" :: Text)]
  toJSON (SomeDevState (SFailed msg)) = object ["phase" .= ("dev_failed" :: Text), "message" .= msg]

instance FromJSON SomeDevState where
  parseJSON = withObject "SomeDevState" $ \v -> do
    phase <- v .: "phase"
    case (phase :: Text) of
      "dev_spawned" -> pure (SomeDevState SSpawned)
      "dev_working" -> pure (SomeDevState SWorking)
      "dev_pr_filed" -> do
        n <- v .: "pr_number"
        url <- v .:? "url"
        pure (SomeDevState (SPRFiled n (maybe "" id url)))
      "dev_under_review" -> do
        n <- v .: "pr_number"
        r <- v .: "review_round"
        pure (SomeDevState (SUnderReview n r))
      "dev_changes_requested" -> do
        n <- v .: "pr_number"
        cs <- v .: "comments"
        pure (SomeDevState (SChangesRequested n cs))
      "dev_approved" -> do
        n <- v .: "pr_number"
        pure (SomeDevState (SApproved n))
      "dev_done" -> pure (SomeDevState SDone)
      "dev_failed" -> do
        msg <- v .: "message"
        pure (SomeDevState (SFailed msg))
      other -> fail $ "Unknown DevPhase: " <> T.unpack other
