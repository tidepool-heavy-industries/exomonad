{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}

-- | Effect for logging execution events into a RunTree.
--
-- Captures LLM decisions, child spawns, child completions, and commits
-- during Work graph execution. Events are accumulated into a mutable
-- NodeBuilder, then frozen into an immutable Node tree at completion.
--
-- = Usage
--
-- @
-- workAfter (result, _sid) = do
--   logDecision result.ccrParsedOutput      -- Log the LLM decision
--   case result.ccrParsedOutput of
--     Spawn children -> do
--       forM_ children $ \\cs -> do
--         handle <- spawnSelf ...
--         logChildSpawned handle.chId cs    -- Log each spawn
--     Complete hash -> do
--       logNodeComplete (ChildSuccess hash) -- Log completion
-- @
module TypesFirstDev.Effect.RunTreeLog
  ( -- * Effect Type
    RunTreeLog(..)

    -- * Smart Constructors
  , logDecision
  , logChildSpawned
  , logChildComplete
  , logCommit
  , logNodeComplete
  , logSessionInfo
  , logMetrics
  , logSessionOp

    -- * Interpreter State
  , NodeBuilder(..)
  , newNodeBuilder
  , freezeNode

    -- * Interpreter
  , runRunTreeLog
  ) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO, writeTVar)
import Control.Monad.Freer (Eff, LastMember, Member, interpret, send, sendM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)

import Tidepool.Effect.Session (SessionInfo(..), SessionId(..))
import Tidepool.Effect.Subgraph (ChildId(..))
import TypesFirstDev.RunTree (Node(..), Event(..), Timed(..), Commit, SessionOpType)
import TypesFirstDev.Types.Work (WorkExit(..), ChildSpec(..))
import TypesFirstDev.WorkContext (ChildOutcome(..))


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT TYPE
-- ════════════════════════════════════════════════════════════════════════════

-- | Effect for logging execution events.
--
-- Each operation appends a timestamped event to the current node's event list.
data RunTreeLog r where
  -- | Log an LLM decision (Continue/Spawn/AwaitNext/Complete)
  LogDecision :: WorkExit -> RunTreeLog ()

  -- | Log a child spawn (before spawnSelf returns)
  LogChildSpawned :: ChildId -> ChildSpec -> RunTreeLog ()

  -- | Log a child completion (after awaitAny returns)
  -- Includes directive text for easy correlation in output
  LogChildComplete :: ChildId -> Text -> ChildOutcome -> RunTreeLog ()

  -- | Log a git commit made by this session
  LogCommit :: Commit -> RunTreeLog ()

  -- | Mark this node as complete with final outcome
  LogNodeComplete :: ChildOutcome -> RunTreeLog ()

  -- | Update session info (called when session ID becomes known)
  LogSessionInfo :: SessionInfo -> RunTreeLog ()

  -- | Log cost and turn metrics from a ClaudeCode call
  LogMetrics :: Double -> Int -> RunTreeLog ()

  -- | Log how this session was started (Fresh/Continue/Fork)
  LogSessionOp :: SessionOpType -> RunTreeLog ()


-- ════════════════════════════════════════════════════════════════════════════
-- SMART CONSTRUCTORS
-- ════════════════════════════════════════════════════════════════════════════

-- | Log an LLM decision.
logDecision :: Member RunTreeLog es => WorkExit -> Eff es ()
logDecision = send . LogDecision

-- | Log a child being spawned.
logChildSpawned :: Member RunTreeLog es => ChildId -> ChildSpec -> Eff es ()
logChildSpawned cid spec = send $ LogChildSpawned cid spec

-- | Log a child completing (with directive text for correlation).
logChildComplete :: Member RunTreeLog es => ChildId -> Text -> ChildOutcome -> Eff es ()
logChildComplete cid directive outcome = send $ LogChildComplete cid directive outcome

-- | Log a git commit.
logCommit :: Member RunTreeLog es => Commit -> Eff es ()
logCommit = send . LogCommit

-- | Mark node as complete.
logNodeComplete :: Member RunTreeLog es => ChildOutcome -> Eff es ()
logNodeComplete = send . LogNodeComplete

-- | Update session info (when it becomes known after first session call).
logSessionInfo :: Member RunTreeLog es => SessionInfo -> Eff es ()
logSessionInfo = send . LogSessionInfo

-- | Log cost and turn count from a ClaudeCode call.
logMetrics :: Member RunTreeLog es => Double -> Int -> Eff es ()
logMetrics cost turns = send $ LogMetrics cost turns

-- | Log how this session was started.
logSessionOp :: Member RunTreeLog es => SessionOpType -> Eff es ()
logSessionOp = send . LogSessionOp


-- ════════════════════════════════════════════════════════════════════════════
-- INTERPRETER STATE
-- ════════════════════════════════════════════════════════════════════════════

-- | Mutable state for building a node during execution.
--
-- Events are accumulated in reverse order (prepended for efficiency),
-- then reversed when freezing.
data NodeBuilder = NodeBuilder
  { nbSessionInfo :: TVar SessionInfo    -- ^ Updated when session ID becomes known
  , nbDirective   :: Text                -- ^ What this node was asked to do
  , nbDepth       :: Int                 -- ^ Recursion depth (0 = root)
  , nbEvents      :: TVar [Timed Event]  -- ^ Accumulated events (reverse order)
  , nbChildren    :: TVar (Map ChildId NodeBuilder)  -- ^ Child builders by ID
  , nbOutcome     :: TVar (Maybe ChildOutcome)       -- ^ Final outcome
  -- Metrics accumulators
  , nbTotalCost   :: TVar Double         -- ^ Accumulated cost in USD
  , nbTurnCount   :: TVar Int            -- ^ Accumulated LLM turns
  , nbSessionOp   :: TVar (Maybe SessionOpType) -- ^ How session was started
  }


-- | Create a new NodeBuilder.
newNodeBuilder
  :: Text            -- ^ Directive (what this node is asked to do)
  -> Int             -- ^ Depth (0 = root)
  -> IO NodeBuilder
newNodeBuilder directive depth = do
  sessionInfoVar <- newTVarIO (SessionInfo (Tidepool.Effect.Session.SessionId "") "" "")
  eventsVar <- newTVarIO []
  childrenVar <- newTVarIO Map.empty
  outcomeVar <- newTVarIO Nothing
  costVar <- newTVarIO 0.0
  turnsVar <- newTVarIO 0
  sessionOpVar <- newTVarIO Nothing
  pure NodeBuilder
    { nbSessionInfo = sessionInfoVar
    , nbDirective = directive
    , nbDepth = depth
    , nbEvents = eventsVar
    , nbChildren = childrenVar
    , nbOutcome = outcomeVar
    , nbTotalCost = costVar
    , nbTurnCount = turnsVar
    , nbSessionOp = sessionOpVar
    }


-- | Freeze a NodeBuilder into an immutable Node.
--
-- Recursively freezes all children. Events are reversed to chronological order.
freezeNode :: NodeBuilder -> IO Node
freezeNode builder = do
  sessionInfo <- readTVarIO builder.nbSessionInfo
  events <- reverse <$> readTVarIO builder.nbEvents
  childBuilders <- readTVarIO builder.nbChildren
  children <- traverse freezeNode (Map.elems childBuilders)
  outcome <- readTVarIO builder.nbOutcome
  totalCost <- readTVarIO builder.nbTotalCost
  turnCount <- readTVarIO builder.nbTurnCount
  sessionOp <- readTVarIO builder.nbSessionOp
  pure Node
    { nSessionInfo = sessionInfo
    , nDirective = builder.nbDirective
    , nDepth = builder.nbDepth
    , nEvents = events
    , nChildren = children
    , nOutcome = outcome
    , nTotalCost = if totalCost > 0 then Just totalCost else Nothing
    , nTurnCount = turnCount
    , nSessionOp = sessionOp
    }


-- ════════════════════════════════════════════════════════════════════════════
-- INTERPRETER
-- ════════════════════════════════════════════════════════════════════════════

-- | Run the RunTreeLog effect, accumulating events into the given NodeBuilder.
runRunTreeLog
  :: LastMember IO es
  => NodeBuilder
  -> Eff (RunTreeLog ': es) a
  -> Eff es a
runRunTreeLog builder = interpret $ \case
  LogDecision exit -> sendM $ do
    now <- getCurrentTime
    atomically $ modifyTVar' builder.nbEvents (Timed now (EvDecision exit) :)

  LogChildSpawned cid spec -> sendM $ do
    now <- getCurrentTime
    atomically $ modifyTVar' builder.nbEvents (Timed now (EvChildSpawned cid spec) :)

  LogChildComplete cid directive outcome -> sendM $ do
    now <- getCurrentTime
    atomically $ modifyTVar' builder.nbEvents (Timed now (EvChildComplete cid directive outcome) :)

  LogCommit commit -> sendM $ do
    now <- getCurrentTime
    atomically $ modifyTVar' builder.nbEvents (Timed now (EvCommit commit) :)

  LogNodeComplete outcome -> sendM $
    atomically $ writeTVar builder.nbOutcome (Just outcome)

  LogSessionInfo info -> sendM $
    atomically $ writeTVar builder.nbSessionInfo info

  LogMetrics cost turns -> sendM $ do
    now <- getCurrentTime
    atomically $ do
      modifyTVar' builder.nbTotalCost (+ cost)
      modifyTVar' builder.nbTurnCount (+ turns)
      modifyTVar' builder.nbEvents (Timed now (EvMetrics cost turns) :)

  LogSessionOp op -> sendM $
    atomically $ writeTVar builder.nbSessionOp (Just op)
