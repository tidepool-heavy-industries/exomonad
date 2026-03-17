{-# LANGUAGE OverloadedStrings #-}

-- | Dev agent lifecycle phases as a simple sum type with StateMachine instance.
module DevPhase
  ( DevPhase (..),
    DevEvent (..),
    PRNumber,
    URL,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.=))
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Guest.StateMachine (StateMachine (..), StopCheckResult (..), TransitionResult (..))

type PRNumber = Int

type URL = Text

-- | Dev agent lifecycle phases.
data DevPhase
  = DevSpawned
  | DevWorking
  | DevPRFiled PRNumber URL
  | DevUnderReview PRNumber Int
  | DevChangesRequested PRNumber [Text]
  | DevApproved PRNumber
  | DevDone
  | DevFailed Text
  deriving (Show, Eq)

-- | Dev lifecycle events.
data DevEvent
  = PRCreated PRNumber Text Text
  | NotifyParentSuccess Text
  | NotifyParentFailure Text
  | ShutdownRequested
  | ReviewReceivedEv PRNumber Text
  | ReviewApprovedEv PRNumber
  | FixesPushedEv PRNumber Text
  | CommitsPushedEv PRNumber Text
  deriving (Show, Eq)

instance StateMachine DevPhase DevEvent where
  machineName = "dev"

  transition phase event = case event of
    PRCreated prNum url _branch ->
      Transitioned (DevPRFiled prNum url)
    NotifyParentSuccess _ ->
      Transitioned DevDone
    NotifyParentFailure msg ->
      Transitioned (DevFailed msg)
    ShutdownRequested ->
      Transitioned DevDone
    ReviewReceivedEv prNum comments ->
      Transitioned (DevChangesRequested prNum [comments])
    ReviewApprovedEv prNum ->
      Transitioned (DevApproved prNum)
    FixesPushedEv prNum _ci ->
      let round = case phase of
            DevUnderReview _ r -> r + 1
            _ -> 1
       in Transitioned (DevUnderReview prNum round)
    CommitsPushedEv prNum _ci ->
      let round = case phase of
            DevUnderReview _ r -> r + 1
            _ -> 1
       in Transitioned (DevUnderReview prNum round)

  canExit (DevChangesRequested pr _) =
    MustBlock $ "PR #" <> T.pack (show pr) <> " has changes requested. Address review comments before stopping."
  canExit (DevPRFiled pr _) =
    ShouldNudge $ "PR #" <> T.pack (show pr) <> " awaiting review. System will auto-notify parent."
  canExit (DevUnderReview pr _) =
    ShouldNudge $ "PR #" <> T.pack (show pr) <> " under review. System will auto-notify parent."
  canExit _ = Clean

instance ToJSON DevPhase where
  toJSON DevSpawned = object ["phase" .= ("dev_spawned" :: Text)]
  toJSON DevWorking = object ["phase" .= ("dev_working" :: Text)]
  toJSON (DevPRFiled n url) = object ["phase" .= ("dev_pr_filed" :: Text), "pr_number" .= n, "url" .= url]
  toJSON (DevUnderReview n r) = object ["phase" .= ("dev_under_review" :: Text), "pr_number" .= n, "review_round" .= r]
  toJSON (DevChangesRequested n cs) = object ["phase" .= ("dev_changes_requested" :: Text), "pr_number" .= n, "comments" .= cs]
  toJSON (DevApproved n) = object ["phase" .= ("dev_approved" :: Text), "pr_number" .= n]
  toJSON DevDone = object ["phase" .= ("dev_done" :: Text)]
  toJSON (DevFailed msg) = object ["phase" .= ("dev_failed" :: Text), "message" .= msg]

instance FromJSON DevPhase where
  parseJSON = withObject "DevPhase" $ \v -> do
    phase <- v .: "phase"
    case (phase :: Text) of
      "dev_spawned" -> pure DevSpawned
      "dev_working" -> pure DevWorking
      "dev_pr_filed" -> do
        n <- v .: "pr_number"
        url <- v .:? "url"
        pure (DevPRFiled n (maybe "" id url))
      "dev_under_review" -> do
        n <- v .: "pr_number"
        r <- v .: "review_round"
        pure (DevUnderReview n r)
      "dev_changes_requested" -> do
        n <- v .: "pr_number"
        cs <- v .: "comments"
        pure (DevChangesRequested n cs)
      "dev_approved" -> do
        n <- v .: "pr_number"
        pure (DevApproved n)
      "dev_done" -> pure DevDone
      "dev_failed" -> do
        msg <- v .: "message"
        pure (DevFailed msg)
      other -> fail $ "Unknown DevPhase: " <> T.unpack other
