{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Stub handlers for WS2,3,4 nodes of the hybrid TDD graph.
--
-- These are type-correct placeholders that will be implemented in later workstreams:
-- * WS2: hFork, hTests, hImpl
-- * WS3: hJoin, hVerifyTDD, hTestsReject, hMerge, hConflictResolve
-- * WS4: hValidate, hFix, hPostValidate, hMutationAdversary, hWitness
module TypesFirstDev.Handlers.Hybrid.Stubs
  ( -- * WS2 Stubs (Parallel Execution)
    hForkStub
  , hTestsStub
  , hImplStub

    -- * WS3 Stubs (Verification)
  , hJoinStub
  , hVerifyTDDStub
  , hTestsRejectStub
  , hMergeStub
  , hConflictResolveStub

    -- * WS4 Stubs (Validation)
  , hValidateStub
  , hFixStub
  , hPostValidateStub
  , hMutationAdversaryStub
  , hWitnessStub
  ) where

import Control.Monad.Freer (Eff)
import Tidepool.Graph.Goto (GotoChoice, To)
import Tidepool.Graph.Types (Exit)

import TypesFirstDev.Types.Hybrid


-- ════════════════════════════════════════════════════════════════════════════
-- WS2: PARALLEL EXECUTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Fork handler - creates worktrees and spawns parallel agents.
hForkStub
  :: GatedState
  -> Eff es (GotoChoice '[To "hTests" TestsTemplateCtx, To "hImpl" ImplTemplateCtx])
hForkStub _ = error "WS2 TODO: hFork - create worktrees, spawn parallel agents"

-- | Tests handler - writes QuickCheck properties.
hTestsStub
  :: TestsTemplateCtx
  -> Eff es (GotoChoice '[To "hJoin" TestsResult])
hTestsStub _ = error "WS2 TODO: hTests - write QuickCheck properties, self-verify fail on skeleton"

-- | Impl handler - writes implementation.
hImplStub
  :: ImplTemplateCtx
  -> Eff es (GotoChoice '[To "hJoin" ImplResult])
hImplStub _ = error "WS2 TODO: hImpl - write implementation, verify build passes"


-- ════════════════════════════════════════════════════════════════════════════
-- WS3: VERIFICATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Join handler - synchronization barrier for parallel agents.
hJoinStub
  :: BlindResults
  -> Eff es (GotoChoice '[To "hVerifyTDD" BlindResults])
hJoinStub _ = error "WS3 TODO: hJoin - collect parallel results"

-- | Verify TDD handler - external TDD property verification.
hVerifyTDDStub
  :: BlindResults
  -> Eff es (GotoChoice '[To "hMerge" VerifiedResults, To "hTestsReject" TrivialTestsError])
hVerifyTDDStub _ = error "WS3 TODO: hVerifyTDD - run tests on skeleton, verify they fail"

-- | Tests reject handler - handle trivial/broken tests.
hTestsRejectStub
  :: TrivialTestsError
  -> Eff es (GotoChoice '[To "hFork" GatedState])
hTestsRejectStub _ = error "WS3 TODO: hTestsReject - route back to hFork with feedback"

-- | Merge handler - cherry-pick tests and impl into fresh worktree.
hMergeStub
  :: VerifiedResults
  -> Eff es (GotoChoice '[To "hValidate" MergedState, To "hConflictResolve" ConflictState])
hMergeStub _ = error "WS3 TODO: hMerge - create merge worktree, cherry-pick commits"

-- | Conflict resolve handler - resolve git conflicts via LLM.
hConflictResolveStub
  :: ConflictState
  -> Eff es (GotoChoice '[To "hValidate" MergedState])
hConflictResolveStub _ = error "WS3 TODO: hConflictResolve - resolve merge conflicts"


-- ════════════════════════════════════════════════════════════════════════════
-- WS4: VALIDATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Validate handler - run tests on merged code.
hValidateStub
  :: MergedState
  -> Eff es (GotoChoice '[To "hPostValidate" ValidatedState, To "hFix" ValidationFailure])
hValidateStub _ = error "WS4 TODO: hValidate - run cabal test, parse output"

-- | Fix handler - fix implementation based on test failures.
hFixStub
  :: ValidationFailure
  -> Eff es (GotoChoice '[To "hValidate" MergedState])
hFixStub _ = error "WS4 TODO: hFix - analyze failures, apply fixes, loop"

-- | Post-validate handler - route to mutation adversary.
hPostValidateStub
  :: ValidatedState
  -> Eff es (GotoChoice '[To "hMutationAdversary" MutationTemplateCtx])
hPostValidateStub _ = error "WS4 TODO: hPostValidate - route to mutation adversary"

-- | Mutation adversary handler - test suite red team.
hMutationAdversaryStub
  :: MutationTemplateCtx
  -> Eff es (GotoChoice '[To "hWitness" WitnessReport])
hMutationAdversaryStub _ = error "WS4 TODO: hMutationAdversary - introduce bugs, check test catches"

-- | Witness handler - observe and illuminate.
hWitnessStub
  :: WitnessReport
  -> Eff es (GotoChoice '[To Exit HybridResult])
hWitnessStub _ = error "WS4 TODO: hWitness - compile final result"
