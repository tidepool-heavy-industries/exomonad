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
import Data.Text (Text)
import qualified Data.Text as T

import Tidepool.Effect.Session (SessionId, SessionOperation(..))
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
       TestsTemplateCtx                      -- needs
       TestsAgentOutput                      -- schema
       '[To (Arrive "hJoin") TestsResult]    -- targets (Arrive, not Goto)
       HybridEffects                         -- effs
       TestsTemplateCtx                      -- tpl (passthrough)
hTestsHandler = ClaudeCodeLLMHandler @'Sonnet
  Nothing            -- no system template
  hTestsCompiled     -- user template
  buildTestsContext  -- before: passthrough (input IS the context)
  routeAfterTests    -- after: wrap result and arrive at barrier
  where
    -- Input is already the template context, so just pass through with session op
    buildTestsContext :: TestsTemplateCtx -> Eff HybridEffects (TestsTemplateCtx, SessionOperation)
    buildTestsContext ctx = pure (ctx, StartFresh "parallel/tests")

    routeAfterTests :: (ClaudeCodeResult TestsAgentOutput, SessionId) -> Eff HybridEffects (GotoChoice '[To (Arrive "hJoin") TestsResult])
    routeAfterTests (ccResult, sid) = do
      let result = TestsResult
            { testsOutput = ccResult.ccrParsedOutput
            , testsWorktree = ""  -- Set by orchestration layer
            , testsCommitHash = ""  -- Set by orchestration layer
            , testsSessionId = T.pack (show sid)
            , testsCost = 0.0  -- TODO: Extract from ClaudeCode metrics
            , testsFailureProof = []  -- Populated by external verification in hVerifyTDD
            }
      pure $ gotoArrive @"hJoin" result


-- ════════════════════════════════════════════════════════════════════════════
-- IMPL HANDLER (LLMNode with Arrive)
-- ════════════════════════════════════════════════════════════════════════════

-- | Handler for hImpl node.
-- Spawns ClaudeCode agent to write implementations.
-- Uses Arrive to deposit result at barrier.
hImplHandler
  :: ClaudeCodeLLMHandler
       'Sonnet                               -- model
       ImplTemplateCtx                       -- needs
       ImplAgentOutput                       -- schema
       '[To (Arrive "hJoin") ImplResult]     -- targets (Arrive, not Goto)
       HybridEffects                         -- effs
       ImplTemplateCtx                       -- tpl (passthrough)
hImplHandler = ClaudeCodeLLMHandler @'Sonnet
  Nothing           -- no system template
  hImplCompiled     -- user template
  buildImplContext  -- before: passthrough
  routeAfterImpl    -- after: wrap result and arrive at barrier
  where
    buildImplContext :: ImplTemplateCtx -> Eff HybridEffects (ImplTemplateCtx, SessionOperation)
    buildImplContext ctx = pure (ctx, StartFresh "parallel/impl")

    routeAfterImpl :: (ClaudeCodeResult ImplAgentOutput, SessionId) -> Eff HybridEffects (GotoChoice '[To (Arrive "hJoin") ImplResult])
    routeAfterImpl (ccResult, sid) = do
      let result = ImplResult
            { implOutput = ccResult.ccrParsedOutput
            , implWorktree = ""  -- Set by orchestration layer
            , implCommitHash = ""  -- Set by orchestration layer
            , implSessionId = T.pack (show sid)
            , implCost = 0.0  -- TODO: Extract from ClaudeCode metrics
            }
      pure $ gotoArrive @"hJoin" result
