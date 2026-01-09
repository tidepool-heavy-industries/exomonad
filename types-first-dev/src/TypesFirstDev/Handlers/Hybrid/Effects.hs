{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Shared effect stack and context for hybrid TDD workflow handlers.
--
-- This module is separate to avoid cyclic imports between Genesis and Merge.
module TypesFirstDev.Handlers.Hybrid.Effects
  ( HybridEffects
  , SessionContext(..)
  , WorkflowError(..)
  ) where

import Control.Monad.Freer.Error (Error)
import Control.Monad.Freer.Reader (Reader)
import Data.Text (Text)

import Tidepool.Effect.ClaudeCode (ClaudeCodeExec)
import Tidepool.Effects.Worktree (Worktree)
import Tidepool.Graph.Memory (Memory)

import TypesFirstDev.Types.Hybrid


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT STACK
-- ════════════════════════════════════════════════════════════════════════════

-- | Effect stack for hybrid workflow handlers.
type HybridEffects =
  '[ Error WorkflowError
   , Memory SessionContext
   , Reader StackSpec
   , ClaudeCodeExec
   , Worktree
   , IO
   ]

-- | Session context for hybrid workflow.
-- Includes stash slots for join point patterns where handlers need
-- to pass data that isn't in the ClaudeCodeLLMHandler signature.
data SessionContext = SessionContext
  { scSessionId      :: Text
  , scTotalCost      :: Double
  , scSkeletonStash  :: Maybe SkeletonState   -- For hTypeAdversary → hGate join
  , scGatedStash     :: Maybe GatedState      -- For hFork → hVerifyTDD → hTestsReject
  , scConflictStash  :: Maybe ConflictState   -- For hConflictResolve LLM join
  }

-- | Workflow errors.
data WorkflowError
  = BuildFailed Text
  | TypeCheckFailed Text
  | MaxAttemptsExceeded Int
  | HoleFixFailed Text
  deriving (Show, Eq)
