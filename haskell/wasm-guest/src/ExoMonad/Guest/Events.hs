{-# LANGUAGE OverloadedStrings #-}

module ExoMonad.Guest.Events
  ( EventHandlerConfig (..),
    EventAction (..),
    PRReviewEvent (..),
    CIStatusEvent (..),
    TimeoutEvent (..),
    SiblingMergedEvent (..),
    EventInput (..),
    defaultEventHandlers,
    dispatchEvent,
  )
where

import Control.Monad.Freer (Eff)
import Data.Aeson (FromJSON, ToJSON, Value, object, withObject, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import ExoMonad.Guest.Types (Effects)

-- | PR review event types
data PRReviewEvent
  = ReviewReceived
      { prNumber :: Int,
        comments :: Text
      }
  | ReviewApproved
      { prNumber :: Int
      }
  | ReviewTimeout
      { prNumber :: Int,
        minutesElapsed :: Int
      }
  | FixesPushed
      { prNumber :: Int,
        fpCiStatus :: Text
      }
  | CommitsPushed
      { prNumber :: Int,
        cpCiStatus :: Text
      }
  deriving (Show, Generic)

instance FromJSON PRReviewEvent where
  parseJSON = withObject "PRReviewEvent" $ \v -> do
    kind <- v .: "kind"
    case kind of
      "review_received" -> ReviewReceived <$> v .: "pr_number" <*> v .: "comments"
      "approved" -> ReviewApproved <$> v .: "pr_number"
      "timeout" -> ReviewTimeout <$> v .: "pr_number" <*> v .: "minutes_elapsed"
      "fixes_pushed" -> FixesPushed <$> v .: "pr_number" <*> v .: "ci_status"
      "commits_pushed" -> CommitsPushed <$> v .: "pr_number" <*> v .: "ci_status"
      other -> fail $ "Unknown PRReviewEvent kind: " <> show other

instance ToJSON PRReviewEvent where
  toJSON (ReviewReceived n c) = object ["kind" .= ("review_received" :: Text), "pr_number" .= n, "comments" .= c]
  toJSON (ReviewApproved n) = object ["kind" .= ("approved" :: Text), "pr_number" .= n]
  toJSON (ReviewTimeout n m) = object ["kind" .= ("timeout" :: Text), "pr_number" .= n, "minutes_elapsed" .= m]
  toJSON (FixesPushed n ci) = object ["kind" .= ("fixes_pushed" :: Text), "pr_number" .= n, "ci_status" .= ci]
  toJSON (CommitsPushed n ci) = object ["kind" .= ("commits_pushed" :: Text), "pr_number" .= n, "ci_status" .= ci]

-- | CI status event
data CIStatusEvent = CIStatusEvent
  { ciPrNumber :: Int,
    ciStatus :: Text,
    ciBranch :: Text
  }
  deriving (Show, Generic)

instance FromJSON CIStatusEvent where
  parseJSON = withObject "CIStatusEvent" $ \v ->
    CIStatusEvent <$> v .: "pr_number" <*> v .: "status" <*> v .: "branch"

instance ToJSON CIStatusEvent where
  toJSON (CIStatusEvent n s b) = object ["pr_number" .= n, "status" .= s, "branch" .= b]

-- | Timeout event
data TimeoutEvent = TimeoutEvent
  { tePrNumber :: Int,
    teMinutesElapsed :: Int
  }
  deriving (Show, Generic)

instance FromJSON TimeoutEvent where
  parseJSON = withObject "TimeoutEvent" $ \v ->
    TimeoutEvent <$> v .: "pr_number" <*> v .: "minutes_elapsed"

instance ToJSON TimeoutEvent where
  toJSON (TimeoutEvent n m) = object ["pr_number" .= n, "minutes_elapsed" .= m]

-- | Sibling merged event
data SiblingMergedEvent = SiblingMergedEvent
  { mergedBranch :: Text,
    parentBranch :: Text,
    siblingPRNumber :: Int
  }
  deriving (Show, Generic)

instance FromJSON SiblingMergedEvent where
  parseJSON = withObject "SiblingMergedEvent" $ \v ->
    SiblingMergedEvent <$> v .: "merged_branch" <*> v .: "parent_branch" <*> v .: "sibling_pr_number"

instance ToJSON SiblingMergedEvent where
  toJSON (SiblingMergedEvent mb pb n) = object ["merged_branch" .= mb, "parent_branch" .= pb, "sibling_pr_number" .= n]

-- | Event handler return type
data EventAction
  = InjectMessage Text
  | NotifyParentAction { naMessage :: Text, naPrNumber :: Int }
  | NoAction
  deriving (Show, Generic)

instance ToJSON EventAction where
  toJSON (InjectMessage msg) = object ["action" .= ("inject_message" :: Text), "message" .= msg]
  toJSON (NotifyParentAction msg pr) = object ["action" .= ("notify_parent" :: Text), "message" .= msg, "pr_number" .= pr]
  toJSON NoAction = object ["action" .= ("no_action" :: Text)]

instance FromJSON EventAction where
  parseJSON = withObject "EventAction" $ \v -> do
    action <- v .: "action"
    case action of
      "inject_message" -> InjectMessage <$> v .: "message"
      "notify_parent" -> NotifyParentAction <$> v .: "message" <*> v .: "pr_number"
      "no_action" -> pure NoAction
      other -> fail $ "Unknown EventAction: " <> show other

-- | Configuration for event handlers per role.
data EventHandlerConfig = EventHandlerConfig
  { onPRReview :: PRReviewEvent -> Eff Effects EventAction,
    onCIStatus :: CIStatusEvent -> Eff Effects EventAction,
    onTimeout :: TimeoutEvent -> Eff Effects EventAction,
    onSiblingMerged :: SiblingMergedEvent -> Eff Effects EventAction
  }

-- | Default event handlers (all NoAction).
defaultEventHandlers :: EventHandlerConfig
defaultEventHandlers =
  EventHandlerConfig
    { onPRReview = \_ -> pure NoAction,
      onCIStatus = \_ -> pure NoAction,
      onTimeout = \_ -> pure NoAction,
      onSiblingMerged = \_ -> pure NoAction
    }

-- | Top-level event type wrapper for dispatching.
data EventInput
  = PRReviewInput PRReviewEvent
  | CIStatusInput CIStatusEvent
  | TimeoutInput TimeoutEvent
  | SiblingMergedInput SiblingMergedEvent
  deriving (Show, Generic)

instance FromJSON EventInput where
  parseJSON = withObject "EventInput" $ \v -> do
    eventType <- v .: "event_type"
    payload <- v .: "payload"
    case eventType of
      "pr_review" -> PRReviewInput <$> Aeson.parseJSON payload
      "ci_status" -> CIStatusInput <$> Aeson.parseJSON payload
      "timeout" -> TimeoutInput <$> Aeson.parseJSON payload
      "sibling_merged" -> SiblingMergedInput <$> Aeson.parseJSON payload
      other -> fail $ "Unknown event_type: " <> show other

-- | Dispatch an event to the appropriate handler.
dispatchEvent :: EventHandlerConfig -> EventInput -> Eff Effects EventAction
dispatchEvent cfg (PRReviewInput ev) = onPRReview cfg ev
dispatchEvent cfg (CIStatusInput ev) = onCIStatus cfg ev
dispatchEvent cfg (TimeoutInput ev) = onTimeout cfg ev
dispatchEvent cfg (SiblingMergedInput ev) = onSiblingMerged cfg ev
