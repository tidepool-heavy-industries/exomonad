-- SKETCH: Proposed type changes for types-first-dev v2
-- This is a sketch file for discussion, not compilable code yet

{-# LANGUAGE FieldSelectors #-}

module TypesFirstDev.Types.V2 where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

-- ════════════════════════════════════════════════════════════════════════════
-- TYPES NODE OUTPUT (Structured Semantic Content)
-- ════════════════════════════════════════════════════════════════════════════

-- | A function signature with metadata for test generation.
data FunctionSig = FunctionSig
  { fsName :: Text
    -- ^ Function name (e.g., "push")
  , fsType :: Text
    -- ^ Type signature (e.g., "a -> Stack a -> Stack a")
  , fsDoc :: Text
    -- ^ Brief description used for test prompt generation
    -- e.g., "Add element to top of stack"
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | A high-level testing priority/property to verify.
data TestPriority = TestPriority
  { tpName :: Text
    -- ^ Property name for the test (e.g., "lifo_order")
  , tpDescription :: Text
    -- ^ What to test, becomes the error/spec message
    -- e.g., "Stack maintains LIFO order: push a,b,c then pop returns c,b,a"
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Output from types node - structured semantic content.
--
-- The harness uses this to generate skeleton files via templates.
-- Module header is DERIVED, not LLM-generated.
data TypeDefinitions = TypeDefinitions
  { tdDataTypeName :: Text
    -- ^ Just the type constructor name (e.g., "Stack")
  , tdDataType :: Text
    -- ^ Full data type definition
    -- e.g., "data Stack a = Empty | Push a (Stack a)\n  deriving (Show, Eq)"
  , tdSignatures :: [FunctionSig]
    -- ^ Structured function signatures with docs
  , tdTestPriorities :: [TestPriority]
    -- ^ High-level properties to test (beyond per-function tests)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- Example LLM output:
-- {
--   "tdDataTypeName": "Stack",
--   "tdDataType": "data Stack a = Empty | Push a (Stack a)\n  deriving (Show, Eq)",
--   "tdSignatures": [
--     {"fsName": "empty", "fsType": "Stack a", "fsDoc": "The empty stack"},
--     {"fsName": "push", "fsType": "a -> Stack a -> Stack a", "fsDoc": "Add element to top"},
--     {"fsName": "pop", "fsType": "Stack a -> Maybe (a, Stack a)", "fsDoc": "Remove and return top element"},
--     {"fsName": "peek", "fsType": "Stack a -> Maybe a", "fsDoc": "View top element without removing"},
--     {"fsName": "isEmpty", "fsType": "Stack a -> Bool", "fsDoc": "Check if stack is empty"}
--   ],
--   "tdTestPriorities": [
--     {"tpName": "lifo_order", "tpDescription": "Stack maintains LIFO order: push a,b,c then pop returns c,b,a"},
--     {"tpName": "push_pop_inverse", "tpDescription": "push then pop is identity: pop (push x s) == Just (x, s)"},
--     {"tpName": "empty_invariants", "tpDescription": "empty stack: isEmpty empty && pop empty == Nothing && peek empty == Nothing"}
--   ]
-- }


-- ════════════════════════════════════════════════════════════════════════════
-- INCREMENTAL PROGRESS (For Agent Loops)
-- ════════════════════════════════════════════════════════════════════════════

-- | Work status for incremental execution.
data WorkStatus
  = MoreWork   -- ^ More items remain to complete
  | Done       -- ^ All work finished
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Progress report from each agent invocation.
--
-- Same schema used for both test and impl agents.
-- Allows harness to track progress and decide on crosstalk.
data IncrementalProgress = IncrementalProgress
  { ipStatus :: WorkStatus
    -- ^ Whether more work remains
  , ipJustCompleted :: [Text]
    -- ^ Items completed THIS invocation
    -- e.g., ["prop_empty", "prop_push"] or ["empty", "push"]
  , ipTotalCompleted :: [Text]
    -- ^ All items completed so far (cumulative)
  , ipRemaining :: [Text]
    -- ^ Items still needing work
  , ipNotes :: Maybe Text
    -- ^ Optional notes for harness or other agents
    -- e.g., "pop implementation depends on pattern matching Empty case"
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- Example progress from test agent invocation 2:
-- {
--   "ipStatus": "MoreWork",
--   "ipJustCompleted": ["prop_push"],
--   "ipTotalCompleted": ["prop_empty", "prop_push"],
--   "ipRemaining": ["prop_pop", "prop_peek", "prop_isEmpty", "prop_lifo_order", "prop_push_pop_inverse", "prop_empty_invariants"],
--   "ipNotes": "prop_push tests that peek (push x s) == Just x"
-- }


-- ════════════════════════════════════════════════════════════════════════════
-- HARNESS CONTEXT INJECTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Context update injected between agent invocations.
--
-- Harness constructs this and injects as user message before next invocation.
data ContextUpdate = ContextUpdate
  { cuSource :: Text
    -- ^ Where this update came from (e.g., "test-agent", "harness")
  , cuEventType :: Text
    -- ^ Type of event (e.g., "commit", "checkpoint", "crosstalk")
  , cuMessage :: Text
    -- ^ Human-readable message for the agent
  , cuDetails :: Maybe Text
    -- ^ Optional structured details (e.g., diff, test output)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- Example crosstalk injection to impl agent:
-- {
--   "cuSource": "test-agent",
--   "cuEventType": "crosstalk",
--   "cuMessage": "Test agent committed prop_push which verifies: peek (push x s) == Just x. Consider this specification when implementing push.",
--   "cuDetails": "prop_push :: Int -> Stack Int -> Bool\nprop_push x s = peek (push x s) == Just x"
-- }

-- Example checkpoint failure injection:
-- {
--   "cuSource": "harness",
--   "cuEventType": "checkpoint",
--   "cuMessage": "Build failed after your last change. Error: Couldn't match type 'Maybe a' with 'a'",
--   "cuDetails": "src/Data/Stack.hs:15:8: error:\n    Couldn't match type..."
-- }


-- ════════════════════════════════════════════════════════════════════════════
-- MERGE AGENT TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Request from merge agent to a prior agent session.
data SessionMessage = SessionMessage
  { smTargetSession :: Text
    -- ^ Session ID to send to
  , smMessage :: Text
    -- ^ Message content
  , smRequestType :: MessageRequestType
    -- ^ What kind of response we want
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data MessageRequestType
  = Clarification    -- ^ "Can you explain why you did X?"
  | Adjustment       -- ^ "Please change X to Y"
  | Confirmation     -- ^ "Is it okay if I do X?"
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Final merge result.
data MergeResult = MergeResult
  { mrSuccess :: Bool
    -- ^ Whether merge succeeded
  , mrMergedCommit :: Maybe Text
    -- ^ Commit SHA of merged result
  , mrConflictsResolved :: [Text]
    -- ^ List of files that had conflicts
  , mrSummary :: Text
    -- ^ Human-readable summary
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)


-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH INTERNAL TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Input to fork handler (replaces ForkInput).
data SkeletonGenerated = SkeletonGenerated
  { sgTypeDefs :: TypeDefinitions
    -- ^ Type definitions from types node
  , sgImplPath :: FilePath
    -- ^ Path to generated impl skeleton
  , sgTestPath :: FilePath
    -- ^ Path to generated test skeleton
  , sgProjectPath :: FilePath
    -- ^ Project root
  , sgModuleName :: Text
    -- ^ Module name
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | State for an agent loop.
data AgentLoopState = AgentLoopState
  { alsSessionId :: Text
    -- ^ Claude session ID for continuity
  , alsWorktree :: FilePath
    -- ^ Worktree this agent operates in
  , alsProgress :: IncrementalProgress
    -- ^ Current progress
  , alsInvocationCount :: Int
    -- ^ How many times we've invoked this agent
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Results from parallel agent execution.
data ParallelResults = ParallelResults
  { prTestState :: AgentLoopState
    -- ^ Final state of test agent
  , prImplState :: AgentLoopState
    -- ^ Final state of impl agent
  , prCrosstalkEvents :: [ContextUpdate]
    -- ^ Log of all crosstalk that occurred
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
