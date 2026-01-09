{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Stub handlers for later workstream nodes of the hybrid TDD graph.
--
-- These are type-correct placeholders:
-- * Verification: hVerifyTDD, hTestsReject, hMerge, hConflictResolve
-- * Validation: hValidate, hFix, hPostValidate, hMutationAdversary, hWitness
--
-- Note: Parallel execution handlers (hFork, hTests, hImpl, hJoin) are
-- implemented in Handlers.Hybrid.Parallel using ForkNode/BarrierNode.
module TypesFirstDev.Handlers.Hybrid.Stubs
  ( -- * Verification Stubs
    hVerifyTDDStub
  , hTestsRejectStub
  , hMergeStub
  , hConflictResolveStub

    -- * Validation Stubs
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
-- VERIFICATION STUBS
-- ════════════════════════════════════════════════════════════════════════════

-- | Verify TDD handler - external TDD property verification.
hVerifyTDDStub
  :: BlindResults
  -> Eff es (GotoChoice '[To "hMerge" VerifiedResults, To "hTestsReject" TrivialTestsError])
hVerifyTDDStub _ = error "TODO: hVerifyTDD - run tests on skeleton, verify they fail"

-- | Tests reject handler - handle trivial/broken tests.
hTestsRejectStub
  :: TrivialTestsError
  -> Eff es (GotoChoice '[To "hFork" GatedState])
hTestsRejectStub _ = error "TODO: hTestsReject - route back to hFork with feedback"

-- | Merge handler - cherry-pick tests and impl into fresh worktree.
hMergeStub
  :: VerifiedResults
  -> Eff es (GotoChoice '[To "hValidate" MergedState, To "hConflictResolve" ConflictState])
hMergeStub _ = error "TODO: hMerge - create merge worktree, cherry-pick commits"

-- | Conflict resolve handler - resolve git conflicts via LLM.
hConflictResolveStub
  :: ConflictState
  -> Eff es (GotoChoice '[To "hValidate" MergedState])
hConflictResolveStub _ = error "TODO: hConflictResolve - resolve merge conflicts"


-- ════════════════════════════════════════════════════════════════════════════
-- VALIDATION STUBS
-- ════════════════════════════════════════════════════════════════════════════

-- | Validate handler - run tests on merged code.
hValidateStub
  :: MergedState
  -> Eff es (GotoChoice '[To "hPostValidate" ValidatedState, To "hFix" ValidationFailure])
hValidateStub _ = error "TODO: hValidate - run cabal test, parse output"

-- | Fix handler - fix implementation based on test failures.
hFixStub
  :: ValidationFailure
  -> Eff es (GotoChoice '[To "hValidate" MergedState])
hFixStub _ = error "TODO: hFix - analyze failures, apply fixes, loop"

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
hWitnessStub _ = error "TODO: hWitness - compile final result"
