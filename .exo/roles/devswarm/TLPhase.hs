{-# LANGUAGE OverloadedStrings #-}

-- | TL agent lifecycle phases as a simple sum type with StateMachine instance.
module TLPhase
  ( TLPhase (..),
    TLEvent (..),
    ChildHandle (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.=))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Guest.StateMachine (StateMachine (..), StopCheckResult (..), TransitionResult (..))

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

-- | TL agent lifecycle phases.
data TLPhase
  = TLPlanning
  | TLDispatching
  | TLWaiting (Map Text ChildHandle)
  | TLMerging Int (Map Text ChildHandle)
  | TLAllMerged
  | TLPRFiled Int Text
  | TLDone
  | TLFailed Text
  deriving (Show, Eq)

-- | TL lifecycle events.
data TLEvent
  = ChildSpawned ChildHandle
  | ChildCompleted Text
  | ChildFailed Text Text
  | PRMerged Int Text
  | AllChildrenDone
  | OwnPRFiled Int Text Text
  deriving (Show, Eq)

instance StateMachine TLPhase TLEvent where
  machineName = "tl"

  transition phase event = case event of
    ChildSpawned handle ->
      let slug = chSlug handle
       in case phase of
            TLWaiting children ->
              Transitioned (TLWaiting (Map.insert slug handle children))
            TLDispatching ->
              Transitioned (TLWaiting (Map.singleton slug handle))
            _ ->
              Transitioned (TLWaiting (Map.singleton slug handle))
    ChildCompleted slug ->
      case phase of
        TLWaiting children ->
          let remaining = Map.delete slug children
           in if Map.null remaining
                then Transitioned TLAllMerged
                else Transitioned (TLWaiting remaining)
        _ -> Transitioned phase
    ChildFailed slug reason ->
      Transitioned (TLFailed (slug <> ": " <> reason))
    PRMerged _prNum slug ->
      case phase of
        TLMerging _ children ->
          let remaining = Map.delete slug children
           in if Map.null remaining
                then Transitioned TLAllMerged
                else Transitioned (TLWaiting remaining)
        TLWaiting children ->
          let remaining = Map.delete slug children
           in if Map.null remaining
                then Transitioned TLAllMerged
                else Transitioned (TLWaiting remaining)
        _ -> Transitioned phase
    AllChildrenDone ->
      Transitioned TLDone
    OwnPRFiled pr url _branch ->
      Transitioned (TLPRFiled pr url)

  canExit TLPlanning = Clean
  canExit TLDispatching = ShouldNudge "Still dispatching children."
  canExit (TLWaiting children) =
    ShouldNudge $ T.pack (show (Map.size children)) <> " children still pending."
  canExit (TLMerging pr children) =
    ShouldNudge $ "Merging PR #" <> T.pack (show pr) <> ", " <> T.pack (show (Map.size children)) <> " remaining."
  canExit TLAllMerged = Clean
  canExit (TLPRFiled pr _url) =
    MustBlock $ "PR #" <> T.pack (show pr) <> " filed, waiting for parent to merge. Do not exit."
  canExit TLDone = Clean
  canExit (TLFailed _) = Clean

instance ToJSON TLPhase where
  toJSON TLPlanning = object ["phase" .= ("tl_planning" :: Text)]
  toJSON TLDispatching = object ["phase" .= ("tl_dispatching" :: Text)]
  toJSON (TLWaiting children) = object ["phase" .= ("tl_waiting" :: Text), "children" .= children]
  toJSON (TLMerging pr children) = object ["phase" .= ("tl_merging" :: Text), "pr_number" .= pr, "children" .= children]
  toJSON TLAllMerged = object ["phase" .= ("tl_all_merged" :: Text)]
  toJSON (TLPRFiled pr url) = object ["phase" .= ("tl_pr_filed" :: Text), "pr_number" .= pr, "url" .= url]
  toJSON TLDone = object ["phase" .= ("tl_done" :: Text)]
  toJSON (TLFailed msg) = object ["phase" .= ("tl_failed" :: Text), "message" .= msg]

instance FromJSON TLPhase where
  parseJSON = withObject "TLPhase" $ \v -> do
    phase <- v .: "phase"
    case (phase :: Text) of
      "tl_planning" -> pure TLPlanning
      "tl_dispatching" -> pure TLDispatching
      "tl_waiting" -> do
        children <- v .:? "children"
        pure (TLWaiting (maybe Map.empty id children))
      "tl_idle" -> pure (TLWaiting Map.empty)
      "tl_merging" -> do
        pr <- v .: "pr_number"
        children <- v .:? "children"
        pure (TLMerging pr (maybe Map.empty id children))
      "tl_all_merged" -> pure TLAllMerged
      "tl_pr_filed" -> do
        pr <- v .: "pr_number"
        url <- v .: "url"
        pure (TLPRFiled pr url)
      "tl_done" -> pure TLDone
      "tl_failed" -> do
        msg <- v .: "message"
        pure (TLFailed msg)
      other -> fail $ "Unknown TLPhase: " <> T.unpack other
