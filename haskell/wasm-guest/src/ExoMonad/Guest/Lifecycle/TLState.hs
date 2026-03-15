{-# LANGUAGE StandaloneDeriving #-}

-- | GADT-indexed TL agent lifecycle phases.
module ExoMonad.Guest.Lifecycle.TLState
  ( TLPhase (..),
    TLState (..),
    SomeTLState (..),
    ChildHandle (..),
    describeTLPhase,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.=))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Guest.Lifecycle.DevState (PRNumber)

-- | Child agent handle for tracking spawned agents.
data ChildHandle = ChildHandle
  { chSlug :: Text,
    chBranch :: Text,
    chAgentType :: Text
  }
  deriving (Show, Eq)

instance ToJSON ChildHandle where
  toJSON (ChildHandle s b a) = object ["slug" .= s, "branch" .= b, "agent_type" .= a]

instance FromJSON ChildHandle where
  parseJSON = withObject "ChildHandle" $ \v ->
    ChildHandle <$> v .: "slug" <*> v .: "branch" <*> v .: "agent_type"

-- | TL agent lifecycle phases (promoted to kinds via DataKinds).
data TLPhase
  = Planning
  | Dispatching
  | Waiting
  | Merging
  | AllMerged
  | TLDone
  | TLFailed

-- | GADT-indexed TL state.
data TLState (p :: TLPhase) where
  STLPlanning :: TLState 'Planning
  STLDispatching :: TLState 'Dispatching
  STLWaiting :: Map Text ChildHandle -> TLState 'Waiting
  STLMerging :: PRNumber -> Map Text ChildHandle -> TLState 'Merging
  STLAllMerged :: TLState 'AllMerged
  STLDone :: TLState 'TLDone
  STLFailed :: Text -> TLState 'TLFailed

deriving instance Show (TLState p)

-- | Existential wrapper for runtime phase tracking.
data SomeTLState where
  SomeTLState :: TLState p -> SomeTLState

deriving instance Show SomeTLState

-- | Human-readable phase description.
describeTLPhase :: SomeTLState -> Text
describeTLPhase (SomeTLState STLPlanning) = "planning"
describeTLPhase (SomeTLState STLDispatching) = "dispatching"
describeTLPhase (SomeTLState (STLWaiting children)) =
  "waiting (" <> T.pack (show (Map.size children)) <> " children)"
describeTLPhase (SomeTLState (STLMerging pr children)) =
  "merging PR #" <> T.pack (show pr) <> " (" <> T.pack (show (Map.size children)) <> " remaining)"
describeTLPhase (SomeTLState STLAllMerged) = "all_merged"
describeTLPhase (SomeTLState STLDone) = "done"
describeTLPhase (SomeTLState (STLFailed msg)) = "failed: " <> msg

instance ToJSON SomeTLState where
  toJSON (SomeTLState STLPlanning) = object ["phase" .= ("tl_planning" :: Text)]
  toJSON (SomeTLState STLDispatching) = object ["phase" .= ("tl_dispatching" :: Text)]
  toJSON (SomeTLState (STLWaiting children)) = object ["phase" .= ("tl_waiting" :: Text), "children" .= children]
  toJSON (SomeTLState (STLMerging pr children)) = object ["phase" .= ("tl_merging" :: Text), "pr_number" .= pr, "children" .= children]
  toJSON (SomeTLState STLAllMerged) = object ["phase" .= ("tl_all_merged" :: Text)]
  toJSON (SomeTLState STLDone) = object ["phase" .= ("tl_done" :: Text)]
  toJSON (SomeTLState (STLFailed msg)) = object ["phase" .= ("tl_failed" :: Text), "message" .= msg]

instance FromJSON SomeTLState where
  parseJSON = withObject "SomeTLState" $ \v -> do
    phase <- v .: "phase"
    case (phase :: Text) of
      "tl_planning" -> pure (SomeTLState STLPlanning)
      "tl_dispatching" -> pure (SomeTLState STLDispatching)
      "tl_waiting" -> do
        children <- v .:? "children"
        pure (SomeTLState (STLWaiting (maybe Map.empty id children)))
      -- Backward compat: old "tl_idle" format with pending_children count
      "tl_idle" -> pure (SomeTLState (STLWaiting Map.empty))
      "tl_merging" -> do
        pr <- v .: "pr_number"
        children <- v .:? "children"
        pure (SomeTLState (STLMerging pr (maybe Map.empty id children)))
      "tl_all_merged" -> pure (SomeTLState STLAllMerged)
      "tl_done" -> pure (SomeTLState STLDone)
      "tl_failed" -> do
        msg <- v .: "message"
        pure (SomeTLState (STLFailed msg))
      other -> fail $ "Unknown TLPhase: " <> T.unpack other
