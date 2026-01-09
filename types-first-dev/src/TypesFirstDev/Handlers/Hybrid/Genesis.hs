{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Genesis handlers for WS1 nodes of the hybrid TDD graph.
--
-- WS1 owns:
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
import Control.Monad.Freer.Reader (ask)
import Data.Proxy (Proxy(..))

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

import TypesFirstDev.Types.Hybrid
import TypesFirstDev.Templates.Hybrid
  ( hTypesCompiled
  , hTypeAdversaryCompiled
  , hTypesFixCompiled
  )
import TypesFirstDev.Graph.Hybrid (TypesFirstGraphHybrid(..))
import TypesFirstDev.Handlers.Hybrid.Effects
  ( HybridEffects
  , SessionContext(..)
  , WorkflowError(..)
  )
import TypesFirstDev.Handlers.Hybrid.Merge
  ( hJoinHandler
  , hVerifyTDDHandler
  , hTestsRejectHandler
  , hMergeHandler
  , hConflictResolveHandler
  )


-- ════════════════════════════════════════════════════════════════════════════
-- TYPES HANDLER (LLM Node)
-- ════════════════════════════════════════════════════════════════════════════

-- | Handler for hTypes node.
-- Builds TypesTemplateCtx from StackSpec, routes to skeleton.
hTypesHandler
  :: ClaudeCodeLLMHandler
       'Haiku                               -- model
       'Nothing                             -- cwd (Nothing = use default)
       StackSpec                            -- input
       TypesAgentOutput                     -- schema
       '[To "hSkeleton" TypesResult]        -- targets
       HybridEffects                        -- effects
       TypesTemplateCtx                     -- template context type
hTypesHandler = ClaudeCodeLLMHandler @'Haiku @'Nothing
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
            , trSessionId = maybe "unknown" id ccResult.ccrSessionId
            , trCost = 0.0  -- TODO: Extract from ClaudeCode metrics
            }
      pure $ gotoChoice @"hSkeleton" result


-- ════════════════════════════════════════════════════════════════════════════
-- SKELETON HANDLER (Logic Node)
-- ════════════════════════════════════════════════════════════════════════════

-- | Handler for hSkeleton node.
-- Generates skeleton files with undefined stubs.
hSkeletonHandler
  :: TypesResult
  -> Eff HybridEffects (GotoChoice '[To "hTypeAdversary" SkeletonState])
hSkeletonHandler typesResult = do
  spec <- ask @StackSpec

  -- TODO: Generate skeleton files
  -- 1. Render impl skeleton template
  -- 2. Render test skeleton template
  -- 3. Write files to disk
  -- 4. Run cabal build to verify

  let skeletonState = SkeletonState
        { ssTypesResult = typesResult
        , ssImplPath = spec.specImplPath
        , ssTestPath = spec.specTestPath
        , ssProjectPath = "."  -- TODO: Get from config
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
       'Nothing                               -- cwd
       SkeletonState                          -- input
       TypeAdversaryOutput                    -- schema
       '[To "hGate" TypeAdversaryResult]      -- targets
       HybridEffects                          -- effects
       TypeAdversaryTemplateCtx               -- template context type
hTypeAdversaryHandler = ClaudeCodeLLMHandler @'Haiku @'Nothing
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
       'Nothing                             -- cwd
       TypeHolesFound                       -- input
       TypesAgentOutput                     -- schema
       '[To "hSkeleton" TypesResult]        -- targets
       HybridEffects                        -- effects
       TypesFixTemplateCtx                  -- template context type
hTypesFixHandler = ClaudeCodeLLMHandler @'Haiku @'Nothing
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
            , trSessionId = maybe "unknown" id ccResult.ccrSessionId
            , trCost = 0.0
            }
      pure $ gotoChoice @"hSkeleton" result


-- ════════════════════════════════════════════════════════════════════════════
-- HANDLER RECORD (Partial - WS1 only)
-- ════════════════════════════════════════════════════════════════════════════

-- | Genesis handlers for WS1 nodes.
-- Other nodes use stubs from Handlers.Hybrid.Stubs.
hybridGenesisHandlers :: TypesFirstGraphHybrid (AsHandler HybridEffects)
hybridGenesisHandlers = TypesFirstGraphHybrid
  { hEntry          = Proxy @StackSpec
  , hTypes          = hTypesHandler
  , hSkeleton       = hSkeletonHandler
  , hTypeAdversary  = hTypeAdversaryHandler
  , hGate           = hGateHandler
  , hTypesFix       = hTypesFixHandler
  -- WS2 stubs
  , hFork           = error "WS2 TODO: hFork"
  , hTests          = error "WS2 TODO: hTests"
  , hImpl           = error "WS2 TODO: hImpl"
  -- WS3 handlers (Merge)
  , hJoin           = hJoinHandler
  , hVerifyTDD      = hVerifyTDDHandler
  , hTestsReject    = hTestsRejectHandler
  , hMerge          = hMergeHandler
  , hConflictResolve = hConflictResolveHandler
  -- WS4 stubs
  , hValidate       = error "WS4 TODO: hValidate"
  , hFix            = error "WS4 TODO: hFix"
  , hPostValidate   = error "WS4 TODO: hPostValidate"
  , hMutationAdversary = error "WS4 TODO: hMutationAdversary"
  , hWitness        = error "WS4 TODO: hWitness"
  , hExit           = Proxy @HybridResult
  }
