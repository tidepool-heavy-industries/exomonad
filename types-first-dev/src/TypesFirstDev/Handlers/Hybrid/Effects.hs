{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Shared effect stack and context for hybrid TDD workflow handlers.
--
-- This module is separate to avoid cyclic imports between Genesis, Merge, and WS4 handlers.
module TypesFirstDev.Handlers.Hybrid.Effects
  ( -- * Effect Stack
    HybridEffects

    -- * Session Context
  , SessionContext(..)
  , initialSessionContext

    -- * Workflow Errors
  , WorkflowError(..)
  ) where

import Control.Monad.Freer (Eff, Member)
import Control.Monad.Freer.Error (Error)
import Control.Monad.Freer.Reader (Reader)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Tidepool.Effect.Session (Session, SessionId, SessionOperation(..))
import Tidepool.Effects.Worktree (Worktree)
import Tidepool.Graph.Memory (Memory, getMem, updateMem)

import TypesFirstDev.Effect.Build (Build)
import TypesFirstDev.Types.Hybrid


-- ════════════════════════════════════════════════════════════════════════════
-- SESSION MANAGEMENT
-- ════════════════════════════════════════════════════════════════════════════

-- SessionOperation is imported from Tidepool.Effect.Session


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT STACK
-- ════════════════════════════════════════════════════════════════════════════

-- | Effect stack for hybrid workflow handlers.
--
-- Effects are listed innermost-first:
-- - Error: Workflow failures (build, test, stuck patterns)
-- - Memory: Session state (stashes, cost tracking)
-- - Reader: Immutable config (StackSpec)
-- - Build: Cabal build/test operations
-- - Session: LLM agent spawning via mantle
-- - Worktree: Git worktree management
-- - IO: Base effect
type HybridEffects =
  '[ Error WorkflowError
   , Memory SessionContext
   , Reader StackSpec
   , Build
   , Session
   , Worktree
   , IO
   ]


-- | Session context for hybrid workflow.
-- Includes stash slots for join point patterns where handlers need
-- to pass data that isn't in the ClaudeCodeLLMHandler signature.
data SessionContext = SessionContext
  { scSessionId            :: Text
  , scTotalCost            :: Double

  -- Stash slots for handler joins
  , scSkeletonStash        :: Maybe SkeletonState    -- For hTypeAdversary → hGate join
  , scGatedStash           :: Maybe GatedState       -- For hFork → hVerifyTDD → hTestsReject
  , scConflictStash        :: Maybe ConflictState    -- For hConflictResolve LLM join
  , scValidatedStash       :: Maybe ValidatedState   -- For hPostValidate → hWitness join
  , scMutationResultStash  :: Maybe MutationAdversaryResult  -- For hMutationAdversary → hWitness join
  , scValidationFailureStash :: Maybe ValidationFailure  -- For hFix LLM join
  , scMutationCtxStash     :: Maybe MutationTemplateCtx  -- For hMutationAdversary LLM join
  }


-- | Initial session context.
initialSessionContext :: Text -> SessionContext
initialSessionContext sessionId = SessionContext
  { scSessionId = sessionId
  , scTotalCost = 0.0

  -- Stash slots
  , scSkeletonStash = Nothing
  , scGatedStash = Nothing
  , scConflictStash = Nothing
  , scValidatedStash = Nothing
  , scMutationResultStash = Nothing
  , scValidationFailureStash = Nothing
  , scMutationCtxStash = Nothing
  }


-- ════════════════════════════════════════════════════════════════════════════
-- WORKFLOW ERRORS
-- ════════════════════════════════════════════════════════════════════════════

-- | Workflow errors.
data WorkflowError
  = BuildFailed Text
  | TypeCheckFailed Text
  | MaxAttemptsExceeded Int
  | HoleFixFailed Text
  | StuckOnPattern Text        -- Same failure pattern 3+ times
  | NotConverging              -- Fixes not making progress
  deriving (Show, Eq)
