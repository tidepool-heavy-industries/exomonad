{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Parallel execution handlers for the hybrid TDD graph.
--
-- Phase 3: Parallel Blind Execution
-- * hFork (ForkNode): Create worktrees, spawn parallel agents
-- * hTests (LLMNode): Write QuickCheck properties (blind to impl)
-- * hImpl (LLMNode): Write implementations (blind to tests)
--
-- Note: hJoin is in Handlers.Hybrid.Merge
module TypesFirstDev.Handlers.Hybrid.Parallel
  ( -- * Handlers
    hForkHandler
  , hTestsHandler
  , hImplHandler
  ) where

import Control.Monad.Freer (Eff)
import Control.Monad.Freer.Reader (ask)

import Tidepool.Graph.Goto
  ( GotoChoice
  , To
  , ClaudeCodeLLMHandler(..)
  , ClaudeCodeResult(..)
  , gotoArrive
  )
import Tidepool.Graph.Types (HList(..), ModelChoice(..), Arrive)

import TypesFirstDev.Types.Hybrid
import TypesFirstDev.Templates.Hybrid (hTestsCompiled, hImplCompiled)
import TypesFirstDev.Handlers.Hybrid.Effects (HybridEffects)


-- ════════════════════════════════════════════════════════════════════════════
-- FORK HANDLER (ForkNode)
-- ════════════════════════════════════════════════════════════════════════════

-- | Handler for hFork node.
-- Builds template contexts for parallel agents.
-- Returns HList of payloads for spawn targets.
--
-- Note: Worktree creation happens in the orchestration layer,
-- not in the handler. The handler just prepares the contexts.
hForkHandler
  :: GatedState
  -> Eff HybridEffects (HList '[TestsTemplateCtx, ImplTemplateCtx])
hForkHandler gated = do
  spec <- ask @StackSpec

  let skeleton = gated.gsSkeleton
      typesOutput = skeleton.ssTypesResult.trOutput
      adversaryOutput = gated.gsTypeAdversary.tarOutput

      -- Build hardening hints from type adversary findings
      hardeningHints = mkHardeningHints adversaryOutput

      -- Build TestsTemplateCtx
      testsCtx = TestsTemplateCtx
        { typeName = typesOutput.typeName
        , functions = typesOutput.functions
        , testPath = spec.specTestPath
        , priorFeedback = Nothing  -- First attempt
        , echoes = Nothing         -- No crosstalk yet
        , hardeningHints = hardeningHints
        }

      -- Build ImplTemplateCtx
      implCtx = ImplTemplateCtx
        { typeName = typesOutput.typeName
        , functions = typesOutput.functions
        , implPath = spec.specImplPath
        , echoes = Nothing         -- No crosstalk yet
        , hardeningHints = hardeningHints
        }

  pure (testsCtx ::: implCtx ::: HNil)

-- | Convert type adversary findings to hardening hints.
mkHardeningHints :: TypeAdversaryOutput -> [HardeningHint]
mkHardeningHints adversaryOutput =
  [ HardeningHint
      { hhContext = hole.description
      , hhGuidance = hole.suggestedFix
      , hhSource = "typeAdversary"
      }
  | hole <- adversaryOutput.tadHoles
  , hole.severity `elem` [Critical, Major]  -- Only surface significant holes
  ]


-- ════════════════════════════════════════════════════════════════════════════
-- TESTS HANDLER (LLMNode with Arrive)
-- ════════════════════════════════════════════════════════════════════════════

-- | Handler for hTests node.
-- Spawns ClaudeCode agent to write QuickCheck properties.
-- Uses Arrive to deposit result at barrier.
hTestsHandler
  :: ClaudeCodeLLMHandler
       'Sonnet                               -- model
       'Nothing                              -- cwd (set by orchestration)
       TestsTemplateCtx                      -- input
       TestsAgentOutput                      -- schema
       '[To Arrive TestsResult]              -- targets (Arrive, not Goto)
       HybridEffects                         -- effects
       TestsTemplateCtx                      -- template context = input (passthrough)
hTestsHandler = ClaudeCodeLLMHandler @'Sonnet @'Nothing
  Nothing            -- no system template
  hTestsCompiled     -- user template
  buildTestsContext  -- before: passthrough (input IS the context)
  routeAfterTests    -- after: wrap result and arrive at barrier
  where
    -- Input is already the template context, so just pass through
    buildTestsContext :: TestsTemplateCtx -> Eff HybridEffects TestsTemplateCtx
    buildTestsContext = pure

    routeAfterTests :: ClaudeCodeResult TestsAgentOutput -> Eff HybridEffects (GotoChoice '[To Arrive TestsResult])
    routeAfterTests ccResult = do
      let result = TestsResult
            { testsOutput = ccResult.ccrParsedOutput
            , testsWorktree = ""  -- Set by orchestration layer
            , testsCommitHash = ""  -- Set by orchestration layer
            , testsSessionId = maybe "unknown" id ccResult.ccrSessionId
            , testsCost = 0.0  -- TODO: Extract from ClaudeCode metrics
            , testsFailureProof = []  -- Populated by external verification in hVerifyTDD
            }
      pure $ gotoArrive result


-- ════════════════════════════════════════════════════════════════════════════
-- IMPL HANDLER (LLMNode with Arrive)
-- ════════════════════════════════════════════════════════════════════════════

-- | Handler for hImpl node.
-- Spawns ClaudeCode agent to write implementations.
-- Uses Arrive to deposit result at barrier.
hImplHandler
  :: ClaudeCodeLLMHandler
       'Sonnet                               -- model
       'Nothing                              -- cwd (set by orchestration)
       ImplTemplateCtx                       -- input
       ImplAgentOutput                       -- schema
       '[To Arrive ImplResult]               -- targets (Arrive, not Goto)
       HybridEffects                         -- effects
       ImplTemplateCtx                       -- template context = input (passthrough)
hImplHandler = ClaudeCodeLLMHandler @'Sonnet @'Nothing
  Nothing           -- no system template
  hImplCompiled     -- user template
  buildImplContext  -- before: passthrough
  routeAfterImpl    -- after: wrap result and arrive at barrier
  where
    buildImplContext :: ImplTemplateCtx -> Eff HybridEffects ImplTemplateCtx
    buildImplContext = pure

    routeAfterImpl :: ClaudeCodeResult ImplAgentOutput -> Eff HybridEffects (GotoChoice '[To Arrive ImplResult])
    routeAfterImpl ccResult = do
      let result = ImplResult
            { implOutput = ccResult.ccrParsedOutput
            , implWorktree = ""  -- Set by orchestration layer
            , implCommitHash = ""  -- Set by orchestration layer
            , implSessionId = maybe "unknown" id ccResult.ccrSessionId
            , implCost = 0.0  -- TODO: Extract from ClaudeCode metrics
            }
      pure $ gotoArrive result
