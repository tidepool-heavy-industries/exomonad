{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Agent lifecycle phase types and KV-backed persistence.
module ExoMonad.Guest.Lifecycle
  ( DevPhase (..),
    TLPhase (..),
    WorkerPhase (..),
    PRNumber,
    ReviewRound,
    getPhase,
    setPhase,
    getDevPhase,
    setDevPhase,
    getTLPhase,
    setTLPhase,
    getWorkerPhase,
    setWorkerPhase,
  )
where

import Control.Monad.Freer (Eff, Member)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
import Effects.Kv qualified as KV
import ExoMonad.Effects.KV (KVGet, KVSet)
import ExoMonad.Guest.Tool.Suspend.Types (SuspendYield)
import ExoMonad.Guest.Tool.SuspendEffect (suspendEffect)
import GHC.Generics (Generic)

type PRNumber = Int
type ReviewRound = Int

-- ============================================================================
-- Dev Phase
-- ============================================================================

data DevPhase
  = DevSpawned
  | DevWorking
  | DevPRFiled PRNumber
  | DevUnderReview PRNumber ReviewRound
  | DevChangesRequested PRNumber [Text]
  | DevApproved PRNumber
  | DevDone
  | DevFailed Text
  deriving (Show, Eq, Generic)

instance ToJSON DevPhase where
  toJSON DevSpawned = object ["phase" .= ("dev_spawned" :: Text)]
  toJSON DevWorking = object ["phase" .= ("dev_working" :: Text)]
  toJSON (DevPRFiled n) = object ["phase" .= ("dev_pr_filed" :: Text), "pr_number" .= n]
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
      "dev_pr_filed" -> DevPRFiled <$> v .: "pr_number"
      "dev_under_review" -> DevUnderReview <$> v .: "pr_number" <*> v .: "review_round"
      "dev_changes_requested" -> DevChangesRequested <$> v .: "pr_number" <*> v .: "comments"
      "dev_approved" -> DevApproved <$> v .: "pr_number"
      "dev_done" -> pure DevDone
      "dev_failed" -> DevFailed <$> v .: "message"
      other -> fail $ "Unknown DevPhase: " <> T.unpack other

-- ============================================================================
-- TL Phase
-- ============================================================================

data TLPhase
  = TLPlanning
  | TLDispatching
  | TLIdle Int
  | TLMerging PRNumber Int
  | TLAllMerged
  | TLDone
  | TLFailed Text
  deriving (Show, Eq, Generic)

instance ToJSON TLPhase where
  toJSON TLPlanning = object ["phase" .= ("tl_planning" :: Text)]
  toJSON TLDispatching = object ["phase" .= ("tl_dispatching" :: Text)]
  toJSON (TLIdle n) = object ["phase" .= ("tl_idle" :: Text), "pending_children" .= n]
  toJSON (TLMerging pr n) = object ["phase" .= ("tl_merging" :: Text), "pr_number" .= pr, "remaining" .= n]
  toJSON TLAllMerged = object ["phase" .= ("tl_all_merged" :: Text)]
  toJSON TLDone = object ["phase" .= ("tl_done" :: Text)]
  toJSON (TLFailed msg) = object ["phase" .= ("tl_failed" :: Text), "message" .= msg]

instance FromJSON TLPhase where
  parseJSON = withObject "TLPhase" $ \v -> do
    phase <- v .: "phase"
    case (phase :: Text) of
      "tl_planning" -> pure TLPlanning
      "tl_dispatching" -> pure TLDispatching
      "tl_idle" -> TLIdle <$> v .: "pending_children"
      "tl_merging" -> TLMerging <$> v .: "pr_number" <*> v .: "remaining"
      "tl_all_merged" -> pure TLAllMerged
      "tl_done" -> pure TLDone
      "tl_failed" -> TLFailed <$> v .: "message"
      other -> fail $ "Unknown TLPhase: " <> T.unpack other

-- ============================================================================
-- Worker Phase
-- ============================================================================

data WorkerPhase
  = WorkerSpawned
  | WorkerWorking
  | WorkerDone
  | WorkerFailed Text
  deriving (Show, Eq, Generic)

instance ToJSON WorkerPhase where
  toJSON WorkerSpawned = object ["phase" .= ("worker_spawned" :: Text)]
  toJSON WorkerWorking = object ["phase" .= ("worker_working" :: Text)]
  toJSON WorkerDone = object ["phase" .= ("worker_done" :: Text)]
  toJSON (WorkerFailed msg) = object ["phase" .= ("worker_failed" :: Text), "message" .= msg]

instance FromJSON WorkerPhase where
  parseJSON = withObject "WorkerPhase" $ \v -> do
    phase <- v .: "phase"
    case (phase :: Text) of
      "worker_spawned" -> pure WorkerSpawned
      "worker_working" -> pure WorkerWorking
      "worker_done" -> pure WorkerDone
      "worker_failed" -> WorkerFailed <$> v .: "message"
      other -> fail $ "Unknown WorkerPhase: " <> T.unpack other

-- ============================================================================
-- KV Helpers
-- ============================================================================

phaseKey :: Text
phaseKey = "agent-phase"

getPhase :: (FromJSON a, Member SuspendYield effs) => Eff effs (Maybe a)
getPhase = do
  result <- suspendEffect @KVGet (KV.GetRequest {KV.getRequestKey = TL.fromStrict phaseKey})
  case result of
    Left _ -> pure Nothing
    Right resp
      | not (KV.getResponseFound resp) -> pure Nothing
      | otherwise ->
          case Aeson.eitherDecodeStrict (encodeUtf8 (TL.toStrict (KV.getResponseValue resp))) of
            Left _ -> pure Nothing
            Right phase -> pure (Just phase)

setPhase :: (ToJSON a, Member SuspendYield effs) => a -> Eff effs ()
setPhase phase = do
  let json = TL.toStrict (decodeUtf8 (Aeson.encode phase))
  _ <- suspendEffect @KVSet (KV.SetRequest {KV.setRequestKey = TL.fromStrict phaseKey, KV.setRequestValue = TL.fromStrict json})
  pure ()

getDevPhase :: (Member SuspendYield effs) => Eff effs (Maybe DevPhase)
getDevPhase = getPhase

setDevPhase :: (Member SuspendYield effs) => DevPhase -> Eff effs ()
setDevPhase = setPhase

getTLPhase :: (Member SuspendYield effs) => Eff effs (Maybe TLPhase)
getTLPhase = getPhase

setTLPhase :: (Member SuspendYield effs) => TLPhase -> Eff effs ()
setTLPhase = setPhase

getWorkerPhase :: (Member SuspendYield effs) => Eff effs (Maybe WorkerPhase)
getWorkerPhase = getPhase

setWorkerPhase :: (Member SuspendYield effs) => WorkerPhase -> Eff effs ()
setWorkerPhase = setPhase
