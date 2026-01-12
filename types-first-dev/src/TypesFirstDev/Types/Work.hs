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
  , PlanRevisionDetails(..)
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

-- | Graph exit type - commit hash plus optional plan revision.
--
-- Normal completion: wrCommitHash contains hash, wrPlanRevision is Nothing
-- Plan revision: wrCommitHash is empty, wrPlanRevision contains details
data WorkResult = WorkResult
  { wrCommitHash :: Text  -- ^ The commit hash (empty if plan revision)
  , wrPlanRevision :: Maybe PlanRevisionDetails  -- ^ Plan revision details (if needed)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Details about why a plan revision is needed.
--
-- Carries structured information up the tree so parents can decide
-- whether to adapt (spawn new children) or escalate (emit own PlanRevisionNeeded).
data PlanRevisionDetails = PlanRevisionDetails
  { prdIssue :: Text
    -- ^ What blocker prevents meeting acceptance criteria

  , prdDiscovery :: Text
    -- ^ What was learned that invalidates the plan

  , prdProposedChange :: Text
    -- ^ Concrete change needed to criteria or approach
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
-- - PlanRevisionNeeded: acceptance criteria can't be met, plan needs update
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

  | PlanRevisionNeeded
    { prnIssue :: Text
    -- ^ What blocker prevents meeting the acceptance criteria as written.
    --   Examples:
    --   - "servant-server >= 0.21 doesn't exist on Hackage"
    --   - "Effect library doesn't support WASM compilation"
    --   - "Two acceptance criteria directly conflict (stateless API + session management)"
    --   - "Test framework requires GHC 9.6+ but spec requires 9.4 compatibility"

    , prnDiscovery :: Text
    -- ^ What was learned that invalidates assumptions in the plan.
    --   Examples:
    --   - "Latest servant-server is 0.20.3.0. WAI is alternative."
    --   - "Polysemy has no WASM backend. Effectful or freer-simple work."
    --   - "Stateless APIs can use signed cookies for limited session data"
    --   - "Spec targets GHC 9.4 but test deps require 9.6+ (no backports)"

    , prnProposedChange :: Text
    -- ^ Concrete change needed to acceptance criteria or technical approach.
    --   Examples:
    --   - "Allow servant-server 0.20.x OR switch requirement to raw WAI"
    --   - "Change effect library requirement to 'any effect system with WASM support'"
    --   - "Split into two endpoints: stateless main API + optional session endpoint"
    --   - "Bump minimum GHC to 9.6 OR find alternative test framework"
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
