{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Work Node Types
--
-- Single recursive node that handles everything:
-- - Planning/decomposition (Spawn)
-- - Waiting on children (AwaitNext)
-- - Continuing work (Continue)
-- - Completing (Complete)
--
-- Scaffolding is LLM judgement - create structure when it helps
-- children coordinate (e.g., "you do ModuleA, you do ModuleB").
module TypesFirstDev.Types.Work
  ( -- * Graph I/O
    WorkInput(..)
  , WorkResult(..)
    -- * Node Exit Type
  , WorkExit(..)
    -- * Child Specification
  , ChildSpec(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import Tidepool.Effect.Session (SessionInfo)
import Tidepool.StructuredOutput (StructuredOutput)
import Tidepool.StructuredOutput.ClaudeCodeSchema (ClaudeCodeSchema(..))
import Tidepool.StructuredOutput.DecisionTools (ToDecisionTools(..))

import TypesFirstDev.Types.Core (Spec)

--------------------------------------------------------------------------------
-- Graph Entry
--------------------------------------------------------------------------------

-- | Entry to the Work graph.
--
-- Root: gets full Spec + depth=0 + no parent info
-- Children: get navigation directive as spec description + depth=parent+1
--           + parent session info for fork-based inheritance
data WorkInput = WorkInput
  { wiSpec        :: Spec               -- ^ What to work on (root=full spec, child=focused directive)
  , wiDepth       :: Int                -- ^ Current depth (0=root)
  , wiMaxDepth    :: Int                -- ^ Maximum recursion depth
  , wiParentInfo  :: Maybe SessionInfo  -- ^ Parent session info for fork (Nothing for root)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

--------------------------------------------------------------------------------
-- Graph Exit
--------------------------------------------------------------------------------

-- | Graph exit type - just the commit hash.
--
-- Everything else (commit message, files changed) can be looked up via git.
-- The effect layer mechanically collects this when the node completes.
newtype WorkResult = WorkResult
  { wrCommitHash :: Text  -- ^ The commit hash this work completed with
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

--------------------------------------------------------------------------------
-- Node Exit (LLM Decision)
--------------------------------------------------------------------------------

-- | What the Work node decides to do next.
--
-- All except Complete are self-loops with different behaviors:
-- - Continue: immediate self-loop (more local work)
-- - Spawn: spawn children, then AwaitNext
-- - AwaitNext: block until a child completes, inject result into template
-- - Complete: exit graph with commit hash
data WorkExit
  = Continue
    -- ^ Self-loop immediately. More work to do locally.
    --   Use when: scaffolding, intermediate commits, local implementation.

  | Spawn
    { weChildren :: [ChildSpec]
    -- ^ Children to spawn. Each runs the same Work graph recursively.
    --   After spawning, immediately enters AwaitNext.
    --   Use when: decomposing into parallel subtasks.
    }

  | AwaitNext
    -- ^ Block until next child completes. Child's commit hash + message
    --   injected into template context on resume.
    --   Use when: waiting for spawned children to finish.

  | Complete
    { weCommitHash :: Text
    -- ^ The commit hash representing this unit of work.
    --   For leaves: the implementation commit.
    --   For parents: the merge commit integrating children.
    }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput, ToDecisionTools)

instance ClaudeCodeSchema WorkExit where
  ccDecisionTools = Just (toDecisionTools @WorkExit)
  ccParseToolCall = parseToolCall @WorkExit

--------------------------------------------------------------------------------
-- Child Specification
--------------------------------------------------------------------------------

-- | Specification for a child subtask.
--
-- Granularity: "todo item level" - what would get its own header.
-- Keep it focused: 3-6 word navigation directive style.
--
-- Examples:
--   "implement storage layer"
--   "add API endpoints"
--   "write property tests"
data ChildSpec = ChildSpec
  { csDirective :: Text
    -- ^ Navigation directive for child (3-6 words ideal).
    --   Child inherits full parent context via session fork.
    --   This is steering, not context duplication.

  , csBoundary  :: Maybe Text
    -- ^ Optional: file/module boundary for coordination.
    --   E.g., "src/Storage.hs" - tells child where to work.
    --   Helps prevent conflicts when multiple children run in parallel.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)
