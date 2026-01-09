{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Graph definition for the hybrid TDD workflow.
--
-- 20-node graph with parallel blind execution and adversarial verification.
--
-- @
-- Entry(StackSpec)
--     ↓
-- ┌─ hTypes (LLM) → TypesAgentOutput
-- │       ↓
-- ├─ hSkeleton (Logic) → SkeletonState
-- │       ↓                    ↓
-- │       └───────────┐        │
-- │                   ↓        ↓
-- │             hTypeAdversary (LLM) → TypeAdversaryResult
-- │                   ↓
-- ├─ hGate (Logic) [join point]
-- │       ↓ clean        ↓ holes
-- │       │              │
-- │       │        hTypesFix (LLM) ─┐
-- │       │              ↑         │
-- │       │              └─────────┘
-- │       ↓
-- ├─ hFork (ForkNode) → parallel spawn
-- │       ├── hTests (LLM+ClaudeCode) → Arrive TestsResult
-- │       └── hImpl (LLM+ClaudeCode) → Arrive ImplResult
-- │                 ↓
-- ├─ hJoin (BarrierNode) → BlindResults
-- │       ↓
-- ├─ hVerifyTDD (Logic) → VerifiedResults
-- │       ↓ fail: hTestsReject → back to hFork
-- │       ↓ pass
-- ├─ hMerge (Logic) → MergedState
-- │       ↓ conflict: hConflictResolve (LLM)
-- │       ↓ clean
-- ├─ hValidate (Logic)
-- │       ↓ fail: hFix (LLM) → back to hValidate
-- │       ↓ pass
-- ├─ hPostValidate (Logic) → spawn hMutationAdversary
-- │       ↓
-- ├─ hWitness (observer)
-- │       ↓
-- └─ hExit → HybridResult
-- @
module TypesFirstDev.Graph.Hybrid
  ( TypesFirstGraphHybrid(..)
  ) where

import GHC.Generics (Generic)

import Tidepool.Graph.Types
  ( type (:@)
  , Schema
  , Template
  , UsesEffects
  , Exit
  , ClaudeCode
  , ModelChoice(..)
    -- Fork/Barrier annotations
  , Spawn
  , Barrier
  , Awaits
  , Arrive
  )
import qualified Tidepool.Graph.Types as Types (Input)
import Tidepool.Graph.Generic (GraphMode(..))
import qualified Tidepool.Graph.Generic as G
import Tidepool.Graph.Goto (Goto, To)

import TypesFirstDev.Types.Hybrid

import TypesFirstDev.Templates.Hybrid
  ( HTypesTpl, HTypeAdversaryTpl, HTypesFixTpl
  , HTestsTpl, HImplTpl
  , HConflictResolveTpl
  , HFixTpl, HMutationAdversaryTpl
  )


-- | Hybrid TDD workflow graph.
--
-- 20 nodes organized into phases:
--
-- * Phase 1: Type Design (hTypes)
-- * Phase 2: Skeleton + Type Adversary (parallel gate)
-- * Phase 3: Parallel Blind Execution (hTests || hImpl)
-- * Phase 4: TDD Verification
-- * Phase 5: Merge
-- * Phase 6: Validation Loop
-- * Phase 7: Post-Validation + Mutation Adversary
--
-- Workstream ownership:
-- * WS1 (Genesis): hEntry, hTypes, hSkeleton, hTypeAdversary, hGate, hTypesFix
-- * WS2 (Parallel): hFork, hTests, hImpl
-- * WS3 (Verification): hJoin, hVerifyTDD, hTestsReject, hMerge, hConflictResolve
-- * WS4 (Validation): hValidate, hFix, hPostValidate, hMutationAdversary, hWitness, hExit
data TypesFirstGraphHybrid mode = TypesFirstGraphHybrid
  { --------------------------------------------------------------------------
    -- ENTRY
    --------------------------------------------------------------------------
    hEntry :: mode :- G.Entry StackSpec

    --------------------------------------------------------------------------
    -- PHASE 1: TYPE DESIGN [WS1]
    --------------------------------------------------------------------------

    -- | Design types with semantic descriptions and examples.
    -- Uses ClaudeCode to write types and signatures to disk.
    -- Returns TypesAgentOutput (semantic metadata).
  , hTypes :: mode :- G.LLMNode
      :@ Types.Input StackSpec
      :@ Template HTypesTpl
      :@ Schema TypesAgentOutput
      :@ UsesEffects '[Goto "hSkeleton" TypesResult]
      :@ ClaudeCode 'Haiku 'Nothing

    --------------------------------------------------------------------------
    -- PHASE 2: SKELETON + TYPE ADVERSARY [WS1]
    --------------------------------------------------------------------------

    -- | Generate skeleton with undefined stubs.
    -- Fast LogicNode - template rendering + build check.
  , hSkeleton :: mode :- G.LogicNode
      :@ Types.Input TypesResult
      :@ UsesEffects '[Goto "hTypeAdversary" SkeletonState]

    -- | TYPE ADVERSARY: Red team for type system.
    -- Tries to construct invalid states the type allows.
  , hTypeAdversary :: mode :- G.LLMNode
      :@ Types.Input SkeletonState
      :@ Template HTypeAdversaryTpl
      :@ Schema TypeAdversaryOutput
      :@ UsesEffects '[Goto "hGate" TypeAdversaryResult]
      :@ ClaudeCode 'Haiku 'Nothing

    -- | Gate: Collects skeleton + adversary results.
    -- Routes to fork (clean) or typesFix (holes found).
  , hGate :: mode :- G.LogicNode
      :@ Types.Input TypeAdversaryResult
      :@ UsesEffects '[ Goto "hFork" GatedState
                      , Goto "hTypesFix" TypeHolesFound
                      ]

    -- | Fix type holes if adversary found issues.
    -- Routes back to hSkeleton with fixed types.
  , hTypesFix :: mode :- G.LLMNode
      :@ Types.Input TypeHolesFound
      :@ Template HTypesFixTpl
      :@ Schema TypesAgentOutput
      :@ UsesEffects '[Goto "hSkeleton" TypesResult]
      :@ ClaudeCode 'Haiku 'Nothing

    --------------------------------------------------------------------------
    -- PHASE 3: PARALLEL BLIND EXECUTION [WS2]
    --------------------------------------------------------------------------

    -- | Fork into parallel blind agents.
    -- Creates two worktrees, spawns tests and impl concurrently.
    -- ForkNode returns HList of payloads for each spawn target.
  , hFork :: mode :- G.ForkNode
      :@ Types.Input GatedState
      :@ Spawn '[To "hTests" TestsTemplateCtx, To "hImpl" ImplTemplateCtx]
      :@ Barrier "hJoin"

    -- | TESTS AGENT: Writes QuickCheck properties.
    -- Blind to impl. Self-verifies tests fail on skeleton.
    -- Uses Arrive to deposit result at barrier (not Goto).
  , hTests :: mode :- G.LLMNode
      :@ Types.Input TestsTemplateCtx
      :@ Template HTestsTpl
      :@ Schema TestsAgentOutput
      :@ UsesEffects '[Arrive TestsResult]
      :@ ClaudeCode 'Sonnet 'Nothing

    -- | IMPL AGENT: Writes implementation.
    -- Blind to tests. Verifies build passes.
    -- Uses Arrive to deposit result at barrier (not Goto).
  , hImpl :: mode :- G.LLMNode
      :@ Types.Input ImplTemplateCtx
      :@ Template HImplTpl
      :@ Schema ImplAgentOutput
      :@ UsesEffects '[Arrive ImplResult]
      :@ ClaudeCode 'Sonnet 'Nothing

    --------------------------------------------------------------------------
    -- PHASE 4: TDD VERIFICATION [WS3]
    --------------------------------------------------------------------------

    -- | Join: Barrier collecting both blind agents.
    -- Receives HList '[TestsResult, ImplResult] from parallel paths.
  , hJoin :: mode :- G.BarrierNode
      :@ Awaits '[TestsResult, ImplResult]
      :@ UsesEffects '[Goto "hVerifyTDD" BlindResults]

    -- | External TDD verification.
    -- Re-runs tests on skeleton to confirm they fail.
  , hVerifyTDD :: mode :- G.LogicNode
      :@ Types.Input BlindResults
      :@ UsesEffects '[ Goto "hMerge" VerifiedResults
                      , Goto "hTestsReject" TrivialTestsError
                      ]

    -- | Handle trivial tests (passed on skeleton = broken).
  , hTestsReject :: mode :- G.LogicNode
      :@ Types.Input TrivialTestsError
      :@ UsesEffects '[Goto "hFork" GatedState]

    --------------------------------------------------------------------------
    -- PHASE 5: MERGE [WS3]
    --------------------------------------------------------------------------

    -- | Merge into fresh worktree.
    -- Cherry-picks tests and impl commits.
  , hMerge :: mode :- G.LogicNode
      :@ Types.Input VerifiedResults
      :@ UsesEffects '[ Goto "hValidate" MergedState
                      , Goto "hConflictResolve" ConflictState
                      ]

    -- | Resolve git conflicts via LLM agent.
    -- ClaudeCode agent resolves conflicts with context from both agents.
  , hConflictResolve :: mode :- G.LLMNode
      :@ Types.Input ConflictState
      :@ Template HConflictResolveTpl
      :@ Schema ConflictResolveOutput
      :@ UsesEffects '[Goto "hValidate" MergedState]
      :@ ClaudeCode 'Haiku 'Nothing

    --------------------------------------------------------------------------
    -- PHASE 6: VALIDATION LOOP [WS4]
    --------------------------------------------------------------------------

    -- | Run tests on merged code.
  , hValidate :: mode :- G.LogicNode
      :@ Types.Input MergedState
      :@ UsesEffects '[ Goto "hPostValidate" ValidatedState
                      , Goto "hFix" ValidationFailure
                      ]

    -- | Fix implementation based on test failures.
    -- Uses ClaudeCode to analyze failures and apply fixes.
    -- Exit conditions (stuck, not converging) checked before LLM call.
  , hFix :: mode :- G.LLMNode
      :@ Types.Input ValidationFailure
      :@ Template HFixTpl
      :@ Schema FixAgentOutput
      :@ UsesEffects '[Goto "hValidate" MergedState]
      :@ ClaudeCode 'Haiku 'Nothing

    --------------------------------------------------------------------------
    -- PHASE 7: POST-VALIDATION [WS4]
    --------------------------------------------------------------------------

    -- | Post-validation: route to mutation adversary.
    -- For synchronous MVP, always runs mutation adversary before witness.
  , hPostValidate :: mode :- G.LogicNode
      :@ Types.Input ValidatedState
      :@ UsesEffects '[Goto "hMutationAdversary" MutationTemplateCtx]

    -- | MUTATION ADVERSARY: Red team for test suite.
    -- Uses ClaudeCode to introduce mutations and verify tests catch them.
    -- ADVISORY: Findings included in output, don't block exit.
    -- Routes to hWitness with partial WitnessReport; stashes MutationAdversaryResult in Memory.
  , hMutationAdversary :: mode :- G.LLMNode
      :@ Types.Input MutationTemplateCtx
      :@ Template HMutationAdversaryTpl
      :@ Schema MutationAdversaryOutput
      :@ UsesEffects '[Goto "hWitness" WitnessReport]
      :@ ClaudeCode 'Haiku 'Nothing

    -- | Witness: Observes flow and maintains coherent understanding.
  , hWitness :: mode :- G.LogicNode
      :@ Types.Input WitnessReport
      :@ UsesEffects '[Goto Exit HybridResult]

    --------------------------------------------------------------------------
    -- EXIT
    --------------------------------------------------------------------------

  , hExit :: mode :- G.Exit HybridResult
  }
  deriving Generic
