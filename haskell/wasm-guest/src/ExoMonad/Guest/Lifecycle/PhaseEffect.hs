-- | Phase persistence via KV store and stop check types.
--
-- Provides typed get/set functions for each role's existential state,
-- backed by the KV suspend effect.
module ExoMonad.Guest.Lifecycle.PhaseEffect
  ( -- * Stop check result
    StopCheckResult (..),
    describeStopResult,

    -- * Generic phase persistence
    getPhase,
    setPhase,

    -- * Dev phase persistence
    getDevPhase,
    setDevPhase,

    -- * TL phase persistence
    getTLPhase,
    setTLPhase,

    -- * Worker phase persistence
    getWorkerPhase,
    setWorkerPhase,
  )
where

import Control.Monad.Freer (Eff, Member)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
import Effects.Kv qualified as KV
import ExoMonad.Effects.KV (KVGet, KVSet)
import ExoMonad.Guest.Lifecycle.DevState (SomeDevState)
import ExoMonad.Guest.Lifecycle.TLState (SomeTLState)
import ExoMonad.Guest.Lifecycle.WorkerState (SomeWorkerState)
import ExoMonad.Guest.Tool.Suspend.Types (SuspendYield)
import ExoMonad.Guest.Tool.SuspendEffect (suspendEffect)

-- | Result of checking whether an agent can exit.
data StopCheckResult
  = MustBlock Text
  | ShouldNudge Text
  | Clean

-- | Describe a stop check result for logging.
describeStopResult :: StopCheckResult -> Text
describeStopResult (MustBlock msg) = "block: " <> msg
describeStopResult (ShouldNudge msg) = "nudge: " <> msg
describeStopResult Clean = "clean"

-- ============================================================================
-- Generic KV Helpers
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

-- ============================================================================
-- Per-Role Convenience Functions
-- ============================================================================

getDevPhase :: (Member SuspendYield effs) => Eff effs (Maybe SomeDevState)
getDevPhase = getPhase

setDevPhase :: (Member SuspendYield effs) => SomeDevState -> Eff effs ()
setDevPhase = setPhase

getTLPhase :: (Member SuspendYield effs) => Eff effs (Maybe SomeTLState)
getTLPhase = getPhase

setTLPhase :: (Member SuspendYield effs) => SomeTLState -> Eff effs ()
setTLPhase = setPhase

getWorkerPhase :: (Member SuspendYield effs) => Eff effs (Maybe SomeWorkerState)
getWorkerPhase = getPhase

setWorkerPhase :: (Member SuspendYield effs) => SomeWorkerState -> Eff effs ()
setWorkerPhase = setPhase
