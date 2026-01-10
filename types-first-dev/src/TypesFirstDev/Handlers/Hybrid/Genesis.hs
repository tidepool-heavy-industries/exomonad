{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Genesis handlers for the hybrid TDD graph.
--
-- Phase 1: Type Design
-- * hEntry (entry point)
-- * hTypes (LLM: design types)
-- * hSkeleton (Logic: generate skeleton files)
-- * hTypeAdversary (LLM: find type holes)
-- * hGate (Logic: route based on holes)
-- * hTypesFix (LLM: fix type holes)
module TypesFirstDev.Handlers.Hybrid.Genesis
  ( -- * Effect Stack (re-exported from Effects)
    HybridEffects
  , SessionContext(..)
  , WorkflowError(..)

    -- * Handlers
  , hTypesHandler
  , hSkeletonHandler
  , hTypeAdversaryHandler
  , hGateHandler
  , hTypesFixHandler

    -- * Handler Record
  , hybridGenesisHandlers
  ) where

import Control.Monad.Freer (Eff, sendM)
import Control.Monad.Freer.Error (throwError)
import Control.Monad.Freer.Reader (ask)
import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import System.Directory (getCurrentDirectory)

import TypesFirstDev.Effect.Build (buildAll, BuildResult(..))

import Tidepool.Graph.Generic (AsHandler)
import Tidepool.Graph.Goto
  ( GotoChoice
  , To
  , ClaudeCodeLLMHandler(..)
  , ClaudeCodeResult(..)
  , gotoChoice
  )
import Tidepool.Graph.Memory (getMem, updateMem)
import Tidepool.Graph.Types (ModelChoice(..))

import TypesFirstDev.Handlers.Hybrid.Effects
  ( HybridEffects
  , SessionContext(..)
  , WorkflowError(..)
  )
import TypesFirstDev.Types.Hybrid
import TypesFirstDev.Templates.Hybrid
  ( hTypesCompiled
  , hTypeAdversaryCompiled
  , hTypesFixCompiled
  )
import TypesFirstDev.Graph.Hybrid (TypesFirstGraphHybrid(..))
import TypesFirstDev.Handlers.Hybrid.Parallel
  ( hForkHandler
  , hTestsHandler
  , hImplHandler
  )
import TypesFirstDev.Handlers.Hybrid.Merge
  ( hJoinHandler
  , hVerifyTDDHandler
  , hTestsRejectHandler
  , hMergeHandler
  , hConflictResolveHandler
  )

-- WS4 handler imports
import TypesFirstDev.Handlers.Hybrid.Validation
  ( hValidateHandler
  , hFixHandler
  , hPostValidateHandler
  )
import TypesFirstDev.Handlers.Hybrid.Adversary
  ( hMutationAdversaryHandler
  )
import TypesFirstDev.Handlers.Hybrid.Witness
  ( hWitnessHandler
  )


-- ════════════════════════════════════════════════════════════════════════════
-- TYPES HANDLER (LLM Node)
-- ════════════════════════════════════════════════════════════════════════════

-- | Handler for hTypes node.
-- Builds TypesTemplateCtx from StackSpec, routes to skeleton.
hTypesHandler
  :: ClaudeCodeLLMHandler
       'Haiku                               -- model
       StackSpec                            -- needs
       TypesAgentOutput                     -- schema
       '[To "hSkeleton" TypesResult]        -- targets
       HybridEffects                        -- effs
       TypesTemplateCtx                     -- tpl
hTypesHandler = ClaudeCodeLLMHandler @'Haiku
  Nothing              -- no system template
  hTypesCompiled       -- user template
  buildTypesContext    -- before: builds context
  routeAfterTypes      -- after: routes to skeleton
  where
    buildTypesContext :: StackSpec -> Eff HybridEffects TypesTemplateCtx
    buildTypesContext spec = pure TypesTemplateCtx
      { moduleName = spec.specModuleName
      , description = spec.specDescription
      , acceptanceCriteria = spec.specAcceptanceCriteria
      , implPath = spec.specImplPath
      }

    routeAfterTypes :: ClaudeCodeResult TypesAgentOutput -> Eff HybridEffects (GotoChoice '[To "hSkeleton" TypesResult])
    routeAfterTypes ccResult = do
      let result = TypesResult
            { trOutput = ccResult.ccrParsedOutput
            , trSessionId = T.pack (show ccResult.ccrSessionId)
            , trCost = 0.0  -- TODO: Extract from ClaudeCode metrics
            }
      pure $ gotoChoice @"hSkeleton" result


-- ════════════════════════════════════════════════════════════════════════════
-- SKELETON HANDLER (Logic Node)
-- ════════════════════════════════════════════════════════════════════════════

-- | Handler for hSkeleton node.
-- Validates skeleton files written by hTypes via ClaudeCode.
--
-- In the hybrid workflow, hTypes uses ClaudeCode which writes files to disk.
-- This handler:
-- 1. Gets the current project path
-- 2. Validates the skeleton compiles (belt and suspenders)
-- 3. Routes to type adversary with skeleton state
hSkeletonHandler
  :: TypesResult
  -> Eff HybridEffects (GotoChoice '[To "hTypeAdversary" SkeletonState])
hSkeletonHandler typesResult = do
  spec <- ask @StackSpec

  -- Get project path (current working directory in the worktree)
  projectPath <- sendM getCurrentDirectory

  sendM $ putStrLn $ "[SKELETON] Validating skeleton in: " <> projectPath
  sendM $ putStrLn $ "  Impl path: " <> spec.specImplPath
  sendM $ putStrLn $ "  Test path: " <> spec.specTestPath

  -- Validate skeleton compiles (belt and suspenders - hTypes should have done this)
  buildResult <- buildAll projectPath

  if brSuccess buildResult
    then sendM $ putStrLn "[SKELETON] Build validation passed"
    else do
      sendM $ putStrLn $ "[SKELETON] Build failed: " <> T.unpack (brErrors buildResult)
      throwError $ BuildFailed (brErrors buildResult <> "\n" <> brOutput buildResult)

  let skeletonState = SkeletonState
        { ssTypesResult = typesResult
        , ssImplPath = spec.specImplPath
        , ssTestPath = spec.specTestPath
        , ssProjectPath = projectPath
        }

  pure $ gotoChoice @"hTypeAdversary" skeletonState


-- ════════════════════════════════════════════════════════════════════════════
-- TYPE ADVERSARY HANDLER (LLM Node)
-- ════════════════════════════════════════════════════════════════════════════

-- | Handler for hTypeAdversary node.
-- Builds TypeAdversaryTemplateCtx, routes to gate.
--
-- Uses Memory stash pattern: before stashes SkeletonState, after retrieves it.
-- This supports the join point where hGate needs both skeleton + adversary output.
hTypeAdversaryHandler
  :: ClaudeCodeLLMHandler
       'Haiku                                 -- model
       SkeletonState                          -- needs
       TypeAdversaryOutput                    -- schema
       '[To "hGate" TypeAdversaryResult]      -- targets
       HybridEffects                          -- effs
       TypeAdversaryTemplateCtx               -- tpl
hTypeAdversaryHandler = ClaudeCodeLLMHandler @'Haiku
  Nothing                  -- no system template
  hTypeAdversaryCompiled   -- user template
  buildAdversaryContext    -- before: builds context AND stashes skeleton
  routeAfterAdversary      -- after: retrieves skeleton from stash
  where
    buildAdversaryContext :: SkeletonState -> Eff HybridEffects TypeAdversaryTemplateCtx
    buildAdversaryContext skelState = do
      -- Stash skeleton state in Memory for after-handler to retrieve
      updateMem (\ctx -> ctx { scSkeletonStash = Just skelState })
      pure TypeAdversaryTemplateCtx
        { types = skelState.ssTypesResult.trOutput
        , scopeLevel = Leaf  -- Single module scope for now
        }

    routeAfterAdversary :: ClaudeCodeResult TypeAdversaryOutput -> Eff HybridEffects (GotoChoice '[To "hGate" TypeAdversaryResult])
    routeAfterAdversary ccResult = do
      -- Retrieve stashed skeleton state
      ctx <- getMem @SessionContext
      case ctx.scSkeletonStash of
        Nothing -> error "BUG: SkeletonState not stashed before LLM call"
        Just skelState -> do
          let output = ccResult.ccrParsedOutput
              verdict = deriveTypeSystemVerdict output.tadHoles
              result = TypeAdversaryResult
                { tarOutput = output
                , tarVerdict = verdict
                , tarSkeleton = skelState
                }
          pure $ gotoChoice @"hGate" result

-- | Derive verdict from hole list.
deriveTypeSystemVerdict :: [TypeHole] -> TypeSystemVerdict
deriveTypeSystemVerdict holes
  | any isCritical holes = TypeSystemHasHoles
  | any isMajor holes    = TypeSystemHasHoles
  | null holes           = TypeSystemSound
  | otherwise            = TypeSystemMinorHoles
  where
    isCritical h = h.severity == Critical
    isMajor h = h.severity == Major


-- ════════════════════════════════════════════════════════════════════════════
-- GATE HANDLER (Logic Node)
-- ════════════════════════════════════════════════════════════════════════════

-- | Handler for hGate node.
-- Routes to fork (clean) or typesFix (holes found).
-- TypeAdversaryResult now carries SkeletonState from the join point stash.
hGateHandler
  :: TypeAdversaryResult
  -> Eff HybridEffects (GotoChoice '[To "hFork" GatedState, To "hTypesFix" TypeHolesFound])
hGateHandler adversaryResult = do
  spec <- ask @StackSpec
  let skelState = adversaryResult.tarSkeleton
      typesOutput = skelState.ssTypesResult.trOutput

  case adversaryResult.tarVerdict of
    TypeSystemSound -> do
      -- No holes - proceed to fork with full state
      let gatedState = GatedState
            { gsSkeleton = skelState
            , gsTypeAdversary = adversaryResult
            }
      pure $ gotoChoice @"hFork" gatedState

    TypeSystemMinorHoles -> do
      -- Minor holes - log advisory but proceed to fork
      sendM $ putStrLn "[GATE] Minor type holes found (advisory only)"
      let gatedState = GatedState
            { gsSkeleton = skelState
            , gsTypeAdversary = adversaryResult
            }
      pure $ gotoChoice @"hFork" gatedState

    TypeSystemHasHoles -> do
      -- Critical/Major holes - route to fix
      let holesFound = TypeHolesFound
            { thfOriginalTypes = typesOutput
            , thfHoles = adversaryResult.tarOutput.tadHoles
            , thfAttempt = 1
            , thfMaxAttempts = spec.specStrictness.scMaxFixAttempts
            , thfPriorFixes = []
            }
      pure $ gotoChoice @"hTypesFix" holesFound


-- ════════════════════════════════════════════════════════════════════════════
-- TYPES FIX HANDLER (LLM Node)
-- ════════════════════════════════════════════════════════════════════════════

-- | Handler for hTypesFix node.
-- Fixes type holes, routes back to skeleton.
hTypesFixHandler
  :: ClaudeCodeLLMHandler
       'Haiku                               -- model
       TypeHolesFound                       -- needs
       TypesAgentOutput                     -- schema
       '[To "hSkeleton" TypesResult]        -- targets
       HybridEffects                        -- effs
       TypesFixTemplateCtx                  -- tpl
hTypesFixHandler = ClaudeCodeLLMHandler @'Haiku
  Nothing            -- no system template
  hTypesFixCompiled  -- user template
  buildFixContext    -- before: builds context
  routeAfterFix      -- after: routes to skeleton
  where
    buildFixContext :: TypeHolesFound -> Eff HybridEffects TypesFixTemplateCtx
    buildFixContext holesFound = pure TypesFixTemplateCtx
      { originalTypes = holesFound.thfOriginalTypes
      , holes = holesFound.thfHoles
      , attempt = holesFound.thfAttempt
      , priorFixes = holesFound.thfPriorFixes
      }

    routeAfterFix :: ClaudeCodeResult TypesAgentOutput -> Eff HybridEffects (GotoChoice '[To "hSkeleton" TypesResult])
    routeAfterFix ccResult = do
      let result = TypesResult
            { trOutput = ccResult.ccrParsedOutput
            , trSessionId = T.pack (show ccResult.ccrSessionId)
            , trCost = 0.0
            }
      pure $ gotoChoice @"hSkeleton" result


-- ════════════════════════════════════════════════════════════════════════════
-- HANDLER RECORD
-- ════════════════════════════════════════════════════════════════════════════

-- | Genesis handlers for the hybrid TDD workflow.
--
-- DEPRECATED: This record is deprecated. Handlers have type divergence issues
-- due to ClaudeCodeLLMHandler API changes. The actual execution uses the actor
-- runtime which builds handlers dynamically at runtime.
--
-- This record is kept only for API compatibility and backward compatibility.
-- All handlers are stubbed with 'undefined' since they're not used in the
-- current actor-based execution model.
hybridGenesisHandlers :: TypesFirstGraphHybrid (AsHandler HybridEffects)
hybridGenesisHandlers = TypesFirstGraphHybrid
  { hEntry          = Proxy @StackSpec
  , hTypes          = undefined
  , hSkeleton       = undefined
  , hTypeAdversary  = undefined
  , hGate           = undefined
  , hTypesFix       = undefined
  -- Phase 3: Parallel Blind Execution
  , hFork           = undefined
  , hTests          = undefined
  , hImpl           = undefined
  -- Phase 4-5: Verification + Merge
  , hJoin           = undefined
  , hVerifyTDD      = undefined
  , hTestsReject    = undefined
  , hMerge          = undefined
  , hConflictResolve = undefined
  -- WS4 handlers
  , hValidate       = undefined
  , hFix            = undefined
  , hPostValidate   = undefined
  , hMutationAdversary = undefined
  , hWitness        = undefined
  , hExit           = Proxy @HybridResult
  }
