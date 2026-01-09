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

import Control.Monad.Freer.Error (Error)
import Control.Monad.Freer.Reader (Reader)
import Data.Text (Text)

import Tidepool.Effect.ClaudeCode (ClaudeCodeExec)
import Tidepool.Effects.Worktree (Worktree)
import Tidepool.Graph.Memory (Memory)

import TypesFirstDev.Effect.Build (Build)
import TypesFirstDev.Types.Hybrid


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
-- - ClaudeCodeExec: LLM agent spawning
-- - Worktree: Git worktree management
-- - IO: Base effect
type HybridEffects =
  '[ Error WorkflowError
   , Memory SessionContext
   , Reader StackSpec
   , Build
   , ClaudeCodeExec
   , Worktree
   , IO
   ]


-- | Session context for hybrid workflow.
-- Includes stash slots for join point patterns where handlers need
-- to pass data that isn't in the ClaudeCodeLLMHandler signature.
data SessionContext = SessionContext
  { scSessionId            :: Text
  , scTotalCost            :: Double
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
  , scSkeletonStash = Nothing
  , scGatedStash = Nothing
  , scConflictStash = Nothing
  , scValidatedStash = Nothing
  , scMutationResultStash = Nothing
  , scValidationFailureStash = Nothing
  , scMutationCtxStash = Nothing
  }


-- | Workflow errors.
data WorkflowError
  = BuildFailed Text
  | TypeCheckFailed Text
  | MaxAttemptsExceeded Int
  | HoleFixFailed Text
  | StuckOnPattern Text        -- Same failure pattern 3+ times
  | NotConverging              -- Fixes not making progress
  deriving (Show, Eq)
