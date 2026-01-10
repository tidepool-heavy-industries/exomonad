{-# LANGUAGE AllowAmbiguousTypes #-}

-- | BarrierNode execution support for the actor runtime.
--
-- This module provides the handler wrapper for BarrierNode execution. A
-- BarrierNode collects results from parallel workers via 'Arrive' messages
-- and invokes a user handler when all workers have reported.
--
-- = Design
--
-- @
-- Worker "hTests" --arrive--> BarrierNode "hJoin" <--arrive-- Worker "hImpl"
--                                    |
--                                    v
--                           (when all arrived)
--                                    |
--                           userHandler :: HList '[TestsResult, ImplResult]
--                                        -> GotoChoice '[To Exit Combined]
--                                    |
--                                    v
--                               router "exit" combined
-- @
--
-- The barrier uses 'MergeAccumulator' to collect results. When all expected
-- sources have reported for a correlation key, the user handler is invoked
-- with the typed HList of results.
module Tidepool.Actor.Barrier
  ( -- * Barrier State
    BarrierState(..)
  , newBarrierState

    -- * Handler Wrapper
  , barrierHandler

    -- * Arrive Message
  , ArriveMessage(..)
  ) where

import Control.Monad.Freer (Eff)
import Data.Aeson (Value, ToJSON(..), FromJSON(..), (.:))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseEither, Parser)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Kind (Type, Constraint)
import Data.Text (Text)
import qualified Data.Text as T
import System.IO (hFlush, stdout)

import Tidepool.Graph.Goto (GotoChoice)
import Tidepool.Graph.Types (HList)

import Tidepool.Actor.Graph (NodeHandler(..), ExtractChoice(..))
import Tidepool.Actor.Merge
  ( MergeAccumulator
  , newMergeAccumulator
  , addResult
  , getCompletedResults
  , ExpectedSources
  , ExtractMergeResults
  , FromPayloads
  )


-- ════════════════════════════════════════════════════════════════════════════
-- ARRIVE MESSAGE
-- ════════════════════════════════════════════════════════════════════════════

-- | Message format for worker arrivals.
--
-- Workers send arrive messages via the router with target "arrive".
-- The router extracts the barrier name and dispatches to the correct barrier.
--
-- @
-- { "barrier": "hJoin", "payload": {...} }
-- @
data ArriveMessage = ArriveMessage
  { amBarrier :: Text    -- ^ Target barrier name
  , amSource  :: Text    -- ^ Source worker name
  , amPayload :: Value   -- ^ Worker result payload
  }
  deriving stock (Show, Eq)

instance FromJSON ArriveMessage where
  parseJSON = Aeson.withObject "ArriveMessage" $ \o -> do
    barrier <- o .: "barrier"
    payload <- o .: "payload"
    -- Note: Source name is added by the router when dispatching
    -- For now, extract from barrier context
    pure ArriveMessage
      { amBarrier = barrier
      , amSource = ""  -- Filled in by router
      , amPayload = payload
      }


-- ════════════════════════════════════════════════════════════════════════════
-- BARRIER STATE
-- ════════════════════════════════════════════════════════════════════════════

-- | Per-barrier state for tracking worker arrivals.
--
-- Uses unit @()@ as the correlation key since a single barrier instance
-- collects a single fan-out. For multiple concurrent fan-outs to the same
-- barrier, use a different key type.
data BarrierState sources = BarrierState
  { bsAccumulator :: MergeAccumulator ()
    -- ^ Merge accumulator (unit key for single fan-out)
  , bsSourceCount :: IORef Int
    -- ^ Number of sources that have arrived
  }

-- | Create new barrier state for the expected sources.
newBarrierState
  :: forall sources.
     ( ExpectedSources sources )
  => IO (BarrierState sources)
newBarrierState = do
  acc <- newMergeAccumulator @sources
  countRef <- newIORef 0
  pure BarrierState
    { bsAccumulator = acc
    , bsSourceCount = countRef
    }


-- ════════════════════════════════════════════════════════════════════════════
-- BARRIER HANDLER WRAPPER
-- ════════════════════════════════════════════════════════════════════════════

-- | Create a 'NodeHandler' for a BarrierNode.
--
-- The handler accumulates arrive messages from workers. When all expected
-- sources have reported, it invokes the user handler with the typed HList
-- of results and routes to the next node.
--
-- @
-- -- Define barrier handler
-- joinH :: HList '[TestsResult, ImplResult] -> Eff es (GotoChoice '[To Exit BlindResults])
-- joinH (testsRes ::: implRes ::: HNil) = do
--   let combined = BlindResults { tests = testsRes, impl = implRes }
--   pure $ gotoExit combined
--
-- -- Build node handler
-- handler <- barrierHandler
--   \@'[From "hTests" TestsResult, From "hImpl" ImplResult]
--   (runM . runEffects)  -- interpreter
--   joinH
-- @
--
-- Note: The handler is created via 'IO' to allocate per-barrier state.
barrierHandler
  :: forall sources targets es.
     ( ExpectedSources sources
     , ExtractMergeResults sources
     , ExtractChoice targets
     )
  => (forall a. Eff es a -> IO a)                              -- ^ Effect interpreter
  -> (HList (FromPayloads sources) -> Eff es (GotoChoice targets))  -- ^ User handler
  -> IO NodeHandler
barrierHandler interpret userHandler = do
  state <- newBarrierState @sources

  pure $ NodeHandler $ \router jsonPayload -> do
    -- Parse the arrive message
    -- Note: The router adds "source" field when dispatching
    case parseArriveWithSource jsonPayload of
      Left err -> error $ "BarrierNode: Failed to parse arrive message: " <> err
      Right (source, payload) -> do
        putStrLn $ "[BARRIER] " <> T.unpack source <> " arrived"
        hFlush stdout
        -- Add result to accumulator
        maybeComplete <- addResult (state.bsAccumulator) () source payload

        case maybeComplete of
          Nothing -> pure ()  -- Still waiting for more workers
          Just _completedSources -> do
            putStrLn "[BARRIER] All workers arrived, continuing"
            hFlush stdout
            -- All workers arrived! Extract typed results
            results <- getCompletedResults @sources (state.bsAccumulator) ()
            case results of
              Nothing ->
                error "BarrierNode: Results disappeared after completion"
              Just (Left err) ->
                -- TODO: Better error handling - let it crash for MVP
                error $ "BarrierNode: Failed to parse results: " <> show err
              Just (Right hlist) -> do
                -- Invoke user handler
                choice <- interpret (userHandler hlist)
                -- Extract target and route
                let (target, nextPayload) = extractChoice choice
                router target nextPayload


-- | Parse arrive message with source name.
--
-- The router adds "source" to the message when dispatching to the barrier.
parseArriveWithSource :: Value -> Either String (Text, Value)
parseArriveWithSource = parseEither $ Aeson.withObject "ArriveWithSource" $ \o -> do
  source <- o .: "source"
  payload <- o .: "payload"
  pure (source, payload)
