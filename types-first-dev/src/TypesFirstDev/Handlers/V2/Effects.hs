{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Shared effect stack and context for V2 tree-based TDD workflow handlers.
--
-- The V2 workflow is simple:
-- * Scaffold (LLM) - writes tests + stubs + rubric
-- * Implement (Logic + Subgraph + LLM) - spawn children if needed, then implement
--
-- Key insight: The Subgraph effect enables recursive tree structure.
-- Children implement stubbed subsystems; parent implements glue.
module TypesFirstDev.Handlers.V2.Effects
  ( -- * Effect Stack
    V2Effects

    -- * Session Context
  , V2Context(..)
  , initialV2Context

    -- * Workflow Errors
  , V2Error(..)
  ) where

import Data.Text (Text)

import Control.Monad.Freer.Error (Error)
import Control.Monad.Freer.Reader (Reader)

import Tidepool.Effect.Session (Session)
import Tidepool.Effect.Subgraph (Subgraph)
import Tidepool.Effects.Worktree (Worktree)
import Tidepool.Graph.Memory (Memory)

import TypesFirstDev.Effect.Build (Build)
import TypesFirstDev.Types.V2
  ( Spec
  , ScaffoldingResult
  , V2Result
  )


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT STACK
-- ════════════════════════════════════════════════════════════════════════════

-- | Effect stack for V2 workflow handlers.
--
-- Effects are listed innermost-first:
-- - Error: Workflow failures (build, test, timeout)
-- - Memory: Session state (cost tracking, scaffolding stash)
-- - Subgraph: Recursive child spawning (the key to tree structure!)
-- - Reader: Immutable config (Spec)
-- - Build: Cabal build/test operations
-- - Session: LLM agent spawning via mantle
-- - Worktree: Git worktree management
-- - IO: Base effect
type V2Effects =
  '[ Error V2Error
   , Memory V2Context
   , Subgraph Spec V2Result  -- Recursive decomposition
   , Reader Spec
   , Build
   , Session
   , Worktree
   , IO
   ]


-- ════════════════════════════════════════════════════════════════════════════
-- SESSION CONTEXT
-- ════════════════════════════════════════════════════════════════════════════

-- | Session context for V2 workflow.
--
-- Minimal state needed for handlers to operate.
-- Scaffolding result is stashed for implement phase.
data V2Context = V2Context
  { v2SessionId         :: Text
  , v2TotalCost         :: Double
  , v2ScaffoldingStash  :: Maybe ScaffoldingResult  -- Scaffold → Implement handoff
  , v2Depth             :: Int                       -- Current recursion depth
  , v2Branch            :: Text                      -- Current git branch
  }
  deriving (Show, Eq)


-- | Initial V2 context.
initialV2Context :: Text -> Int -> Text -> V2Context
initialV2Context sessionId depth branch = V2Context
  { v2SessionId = sessionId
  , v2TotalCost = 0.0
  , v2ScaffoldingStash = Nothing
  , v2Depth = depth
  , v2Branch = branch
  }


-- ════════════════════════════════════════════════════════════════════════════
-- WORKFLOW ERRORS
-- ════════════════════════════════════════════════════════════════════════════

-- | Workflow errors for V2.
--
-- Simple error types for the two-phase workflow.
data V2Error
  = V2BuildFailed Text         -- ^ Cabal build failed
  | V2TestsFailed Text Int     -- ^ Tests failed after N iterations
  | V2MergeFailed Text         -- ^ Git merge failed (child MR)
  | V2MaxIterations Int        -- ^ Exceeded max CI iterations
  | V2ChildFailed Text Text    -- ^ Child graph failed: subsystem name, reason
  | V2ScaffoldingMissing       -- ^ Implement called without scaffolding
  deriving (Show, Eq)
