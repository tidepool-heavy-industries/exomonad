{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}

module Tidepool.Control.Hook.CircuitBreaker
  ( CircuitBreakerState(..)
  , SessionId
  , CircuitBreakerMap
  , initCircuitBreaker
  , withCircuitBreaker
  , incrementStage
  , getCircuitBreakerState
  ) where

import Control.Concurrent.STM
import Control.Exception (try, SomeException)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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

initCircuitBreaker :: IO CircuitBreakerMap
initCircuitBreaker = newTVarIO Map.empty

-- | Get limits from environment variables
getLimits :: IO (Int, Int, NominalDiffTime)
getLimits = do
  globalMax <- lookupEnv "CIRCUIT_BREAKER_GLOBAL_MAX"
  stageMax <- lookupEnv "CIRCUIT_BREAKER_STAGE_MAX"
  staleTimeout <- lookupEnv "CIRCUIT_BREAKER_STALE_TIMEOUT"

  pure ( fromMaybe 15 (globalMax >>= readMaybe)
       , fromMaybe 5 (stageMax >>= readMaybe)
       , fromIntegral (fromMaybe 300 (staleTimeout >>= readMaybe :: Maybe Int))
       )
  where
    fromMaybe d Nothing = d
    fromMaybe _ (Just v) = v

-- | Check if limits are exceeded
checkLimits :: Int -> Int -> CircuitBreakerState -> Either Text ()
checkLimits globalMax stageMax cbs
  | cbs.cbGlobalStops >= globalMax = Left $ "Global stop limit reached (" <> T.pack (show globalMax) <> ")"
  | any (>= stageMax) (Map.elems cbs.cbStageRetries) = Left $ "Stage limit reached (" <> T.pack (show stageMax) <> ")"
  | otherwise = Right ()

-- | Check if state is stale
isStale :: NominalDiffTime -> CircuitBreakerState -> IO Bool
isStale timeout cbs = do
  now <- getCurrentTime
  let elapsed = diffUTCTime now cbs.cbLastStopTime
  pure $ elapsed > timeout

-- | Acquire lock, run action, release lock (with limits check)
--
-- The session ID is expected to be stable for a given agent session.
-- If the session ID is missing or ephemeral, the caller is responsible
-- for generating or persisting a stable ID before calling this function.
withCircuitBreaker
  :: CircuitBreakerMap
  -> SessionId
  -> IO a
  -> IO (Either Text a)
withCircuitBreaker stateVar sessionId action = do
  (globalMax, stageMax, staleTimeout) <- getLimits
  now <- getCurrentTime
  
  -- 1. Try to acquire lock and check limits
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
        -- Check if already active
        if cbs.cbStopHookActive
          then pure $ Left "Stop hook already running"
          else do
            -- Check limits
            case checkLimits globalMax stageMax cbs of
              Left err -> pure $ Left err
              Right () -> do
                -- Acquire lock
                let updatedState = cbs { cbStopHookActive = True }
                modifyTVar' stateVar (Map.insert sessionId updatedState)
                pure $ Right ()

  case acquireResult of
    Left "Stop hook already running" -> do
      -- Check staleness outside STM
      states <- readTVarIO stateVar
      case Map.lookup sessionId states of
        Just cbs -> do
          stale <- isStale staleTimeout cbs
          if stale
            then do
              -- Force reset if stale
              atomically $ modifyTVar' stateVar $ Map.adjust (\s -> s { cbStopHookActive = True }) sessionId
              runActionAndRelease stateVar sessionId action
            else pure $ Left "Stop hook already running"
        Nothing -> pure $ Left "Session lost during staleness check"
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