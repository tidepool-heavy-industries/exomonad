{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}

module ExoMonad.Control.Hook.CircuitBreaker
  ( CircuitBreakerState(..)
  , SessionId
  , CircuitBreakerMap
  , CircuitBreakerConfig(..)
  , mkCircuitBreakerConfig
  , loadCircuitBreakerConfig
  , initCircuitBreaker
  , withCircuitBreaker
  , incrementStage
  , getCircuitBreakerState
  , getAllCircuitBreakerStates
  , resetSession
  , resetAll
  ) where

import Control.Concurrent.STM
import Control.Exception (try, SomeException)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock
import GHC.Generics (Generic)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

type SessionId = Text

data CircuitBreakerState = CircuitBreakerState
  { cbSessionId :: SessionId
  , cbGlobalStops :: Int
  , cbStageRetries :: Map Text Int
  , cbLastStopTime :: UTCTime
  , cbStopHookActive :: Bool
  } deriving (Show, Eq, Generic)

-- | Global state in control-server
type CircuitBreakerMap = TVar (Map SessionId CircuitBreakerState)

data CircuitBreakerConfig = CircuitBreakerConfig
  { cbcGlobalMax :: Int
  , cbcStageMax :: Int
  , cbcStaleTimeout :: NominalDiffTime
  } deriving (Show, Eq)

mkCircuitBreakerConfig :: Int -> Int -> Int -> Either Text CircuitBreakerConfig
mkCircuitBreakerConfig globalMax stageMax staleTimeout
  | globalMax < 1 = Left "globalMax must be >= 1"
  | stageMax < 1 = Left "stageMax must be >= 1"
  | staleTimeout < 60 = Left "staleTimeout must be >= 60 seconds"
  | otherwise = Right $ CircuitBreakerConfig globalMax stageMax (fromIntegral staleTimeout)

loadCircuitBreakerConfig :: IO (Either Text CircuitBreakerConfig)
loadCircuitBreakerConfig = do
  globalMax <- lookupEnv "CIRCUIT_BREAKER_GLOBAL_MAX"
  stageMax <- lookupEnv "CIRCUIT_BREAKER_STAGE_MAX"
  staleTimeout <- lookupEnv "CIRCUIT_BREAKER_STALE_TIMEOUT"

  let g = fromMaybe 15 (globalMax >>= readMaybe)
  let s = fromMaybe 5 (stageMax >>= readMaybe)
  let t = fromMaybe 300 (staleTimeout >>= readMaybe)

  pure $ mkCircuitBreakerConfig g s t

initCircuitBreaker :: IO CircuitBreakerMap
initCircuitBreaker = newTVarIO Map.empty

-- | Check if limits are exceeded
checkLimits :: CircuitBreakerConfig -> CircuitBreakerState -> Either Text ()
checkLimits config cbs
  | cbs.cbGlobalStops >= config.cbcGlobalMax = Left $ "Global stop limit reached (" <> T.pack (show config.cbcGlobalMax) <> "). Wait " <> T.pack (show $ config.cbcStaleTimeout) <> "s or use cb_reset."
  | any (>= config.cbcStageMax) (Map.elems cbs.cbStageRetries) = Left $ "Stage limit reached (" <> T.pack (show config.cbcStageMax) <> "). Fix the underlying issue or use cb_reset."
  | otherwise = Right ()

-- | Acquire lock, run action, release lock (with limits check)
--
-- The session ID is expected to be stable for a given agent session.
-- If the session ID is missing or ephemeral, the caller is responsible
-- for generating or persisting a stable ID before calling this function.
withCircuitBreaker
  :: CircuitBreakerMap
  -> CircuitBreakerConfig
  -> UTCTime
  -> SessionId
  -> IO a
  -> IO (Either Text a)
withCircuitBreaker stateVar config now sessionId action = do
  -- 1. Try to acquire lock and check limits inside atomically
  acquireResult <- atomically $ do
    states <- readTVar stateVar
    case Map.lookup sessionId states of
      Nothing -> do
        -- Initialize new session state and acquire lock
        let newState = CircuitBreakerState
              { cbSessionId = sessionId
              , cbGlobalStops = 0
              , cbStageRetries = Map.empty
              , cbLastStopTime = now
              , cbStopHookActive = True
              }
        modifyTVar' stateVar (Map.insert sessionId newState)
        pure $ Right ()

      Just cbs -> do
        -- Check staleness
        let elapsed = diffUTCTime now cbs.cbLastStopTime
        let isStale = elapsed > config.cbcStaleTimeout
        
        -- Check if active and not stale
        if cbs.cbStopHookActive && not isStale
          then pure $ Left "Stop hook already running"
          else do
            -- If we are here, either it's inactive OR it was stale (so we preempt)
            -- Check limits (unless we are just resetting a stale lock? No, limits apply always)
            case checkLimits config cbs of
              Left err -> pure $ Left err
              Right () -> do
                -- Acquire lock
                let updatedState = cbs { cbStopHookActive = True, cbLastStopTime = now }
                modifyTVar' stateVar (Map.insert sessionId updatedState)
                pure $ Right ()

  case acquireResult of
    Left err -> pure $ Left err
    Right () -> runActionAndRelease stateVar sessionId action

-- | Helper to run action and ensure lock is released
runActionAndRelease :: CircuitBreakerMap -> SessionId -> IO a -> IO (Either Text a)
runActionAndRelease stateVar sessionId action = do
  result <- try action
  now <- getCurrentTime
  atomically $ modifyTVar' stateVar $ Map.adjust 
    (\s -> s { cbStopHookActive = False, cbLastStopTime = now }) 
    sessionId
  case result of
    Left (e :: SomeException) -> pure $ Left $ T.pack (show e)
    Right val -> pure $ Right val

-- | Increment stage counter (used by graph handlers)
incrementStage :: CircuitBreakerMap -> SessionId -> Text -> UTCTime -> IO ()
incrementStage stateVar sessionId stage now = do
  atomically $ modifyTVar' stateVar $ Map.adjust (\s -> s
    { cbGlobalStops = s.cbGlobalStops + 1
    , cbStageRetries = Map.insertWith (+) stage 1 s.cbStageRetries
    , cbLastStopTime = now
    }) sessionId

-- | Get current state for a session
getCircuitBreakerState :: CircuitBreakerMap -> SessionId -> IO (Maybe CircuitBreakerState)
getCircuitBreakerState stateVar sessionId = Map.lookup sessionId <$> readTVarIO stateVar

-- | Get all circuit breaker states
getAllCircuitBreakerStates :: CircuitBreakerMap -> IO (Map SessionId CircuitBreakerState)
getAllCircuitBreakerStates stateVar = readTVarIO stateVar

-- | Reset a specific session
resetSession :: CircuitBreakerMap -> SessionId -> IO ()
resetSession stateVar sessionId = atomically $ modifyTVar' stateVar (Map.delete sessionId)

-- | Reset all sessions
resetAll :: CircuitBreakerMap -> IO ()
resetAll stateVar = atomically $ writeTVar stateVar Map.empty