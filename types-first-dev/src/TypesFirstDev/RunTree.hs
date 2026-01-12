{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Execution tree for Work graph runs.
--
-- Captures every LLM decision and execution event into a typed tree structure
-- for post-run analysis. Uses domain types directly (WorkExit, ChildSpec, etc.)
-- rather than string representations.
--
-- = Architecture
--
-- @
-- RunTree
--   ├── rtSpec: Original task specification
--   ├── rtStartTime, rtEndTime: Run timing
--   └── rtRoot: Node (recursive tree)
--         ├── nSessionInfo: Session ID, worktree, branch
--         ├── nDirective: What this node was asked to do
--         ├── nDepth: Recursion depth (0 = root)
--         ├── nEvents: [Timed Event] -- chronological
--         ├── nChildren: [Node] -- recursive!
--         └── nOutcome: Success/Failure
-- @
--
-- = Usage
--
-- Built incrementally during execution via 'RunTreeLog' effect,
-- then frozen into immutable 'RunTree' at completion.
module TypesFirstDev.RunTree
  ( -- * Tree Structure
    RunTree(..)
  , Node(..)
  , SessionOpType(..)

    -- * Events
  , Event(..)
  , Timed(..)
  , Commit(..)

    -- * Re-exports for convenience
  , WorkExit(..)
  , ChildSpec(..)
  , ChildOutcome(..)
  , SessionInfo(..)
  , ChildId(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID ()
import GHC.Generics (Generic)

-- Domain types (reused, not recreated)
import Tidepool.Effect.Session (SessionInfo(..))
import Tidepool.Effect.Subgraph (ChildId(..))
import TypesFirstDev.Types.Core (Spec)
import TypesFirstDev.Types.Work (WorkExit(..), ChildSpec(..))
import TypesFirstDev.WorkContext (ChildOutcome(..))


-- ════════════════════════════════════════════════════════════════════════════
-- TREE STRUCTURE
-- ════════════════════════════════════════════════════════════════════════════

-- | Complete execution tree for a Work graph run.
--
-- Captures the full recursive structure of spawned children,
-- every LLM decision, and outcomes.
data RunTree = RunTree
  { rtSpec      :: Spec              -- ^ Original input specification
  , rtStartTime :: UTCTime           -- ^ When the run started
  , rtEndTime   :: Maybe UTCTime     -- ^ When the run ended (Nothing if still running)
  , rtRoot      :: Node              -- ^ Root of the execution tree
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)


-- | How a session was started.
data SessionOpType
  = OpFresh Text           -- ^ Fresh start with slug
  | OpContinue             -- ^ Continuation of existing session
  | OpFork Text            -- ^ Forked from parent with child slug
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)


-- | One node per session in the execution tree.
--
-- Recursive via 'nChildren' - children can spawn their own children
-- to arbitrary depth (bounded by WorkInput.wiMaxDepth).
data Node = Node
  { nSessionInfo :: SessionInfo        -- ^ CC session ID, worktree path, branch name
  , nDirective   :: Text               -- ^ What this node was asked to do (from ChildSpec or Spec)
  , nDepth       :: Int                -- ^ Recursion depth (0 = root)
  , nEvents      :: [Timed Event]      -- ^ Chronological events during execution
  , nChildren    :: [Node]             -- ^ Spawned children (recursive structure)
  , nOutcome     :: Maybe ChildOutcome -- ^ How this node ended (Nothing if still running)
  -- Metrics
  , nTotalCost   :: Maybe Double       -- ^ Total cost in USD for this node's LLM calls
  , nTurnCount   :: Int                -- ^ Number of LLM turns in this node
  , nSessionOp   :: Maybe SessionOpType -- ^ How this session was started
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)


-- ════════════════════════════════════════════════════════════════════════════
-- EVENTS
-- ════════════════════════════════════════════════════════════════════════════

-- | Timestamped wrapper for events.
data Timed a = Timed
  { tTime  :: UTCTime
  , tValue :: a
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)


-- | Events that occur during a session's execution.
--
-- Uses actual domain types:
-- - 'WorkExit' for LLM decisions (Continue/Spawn/AwaitNext/Complete)
-- - 'ChildSpec' for spawn specifications (directive + boundary)
-- - 'ChildOutcome' for child results (Success/Failure)
data Event
  = EvDecision WorkExit
    -- ^ LLM made a decision. The WorkExit captures:
    --   - Continue: self-loop for more work
    --   - Spawn [ChildSpec]: decompose into children
    --   - AwaitNext: wait for child completion
    --   - Complete Text: done with commit hash

  | EvChildSpawned ChildId ChildSpec
    -- ^ Child was spawned. ChildSpec has directive and optional boundary.

  | EvChildComplete ChildId Text ChildOutcome
    -- ^ Child finished. Includes directive text for easy correlation.
    -- ChildOutcome is Success (commit) or Failure (error).

  | EvCommit Commit
    -- ^ Git commit was made by this session.

  | EvMetrics Double Int
    -- ^ Cost (USD) and turn count from a ClaudeCode call.
    -- Accumulated into node totals.
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)


-- | Git commit information.
--
-- Captured when a session makes commits (scaffolding, implementation, merge).
data Commit = Commit
  { cHash    :: Text      -- ^ Short commit hash (e.g., "e8cf578")
  , cMessage :: Text      -- ^ Commit message
  , cFiles   :: [Text]    -- ^ Files changed (e.g., ["+src/Storage.hs", "M src/Main.hs"])
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
