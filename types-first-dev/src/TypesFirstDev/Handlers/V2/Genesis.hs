{-# LANGUAGE AllowAmbiguousTypes #-}

-- | V2 Handlers for tree-based TDD decomposition workflow.
--
-- Two handlers, one simple graph:
--
-- @
-- Entry(Spec) → hScaffold → hImplement → Exit(V2Result)
-- @
--
-- The power is in hImplement's use of Subgraph effect:
-- * Checks shouldDecompose policy
-- * If decomposing: spawns children, awaits MRs, merges, then implements glue
-- * If leaf: implements all stubs directly
--
-- Children implement stubbed subsystems; parent implements glue.
module TypesFirstDev.Handlers.V2.Genesis
  ( -- * Handlers
    hScaffoldingHandler
  , hImplementHandler

    -- * Re-exports
  , V2Effects
  , V2Error(..)
  ) where

import Control.Monad (forM)
import Control.Monad.Freer (Eff, Member, sendM)
import Control.Monad.Freer.Error (Error)
import Control.Monad.Freer.Reader (ask)
import Data.OpenUnion (LastMember)
import qualified Data.Text as T

import Tidepool.Effect.Session (SessionId, SessionOperation(..))
import Tidepool.Effect.Subgraph (Subgraph, spawnSelf, awaitAny, ChildId(..))
import Tidepool.Graph.Goto
  ( GotoChoice
  , To
  , ClaudeCodeLLMHandler(..)
  , ClaudeCodeResult(..)
  , gotoChoice
  , gotoExit
  )
import Tidepool.Graph.Memory (updateMem)
import Tidepool.Graph.Types (Exit, ModelChoice(..))

import TypesFirstDev.Handlers.V2.Effects
  ( V2Effects
  , V2Context(..)
  , V2Error(..)
  )
import TypesFirstDev.Types.V2
  ( Spec(..)
  , ScaffoldingCtx(..)
  , ScaffoldingOutput(..)
  , ScaffoldingResult(..)
  , ScaffoldingRubric(..)
  , ChildResult(..)
  , V2Result(..)
  , SubsystemSpec(..)
  , shouldDecompose
  , deriveSubsystemSpecs
  , specFromSubsystem
  )
import TypesFirstDev.Templates.V2 (v2ScaffoldingCompiled)


-- ════════════════════════════════════════════════════════════════════════════
-- SCAFFOLD HANDLER (LLM Node)
-- ════════════════════════════════════════════════════════════════════════════

-- | Handler for hScaffold node.
--
-- TDD scaffolding: writes tests + stubs + rubric, commits.
-- The heavy lifting of the workflow happens here.
hScaffoldingHandler
  :: ClaudeCodeLLMHandler
       'Sonnet                                    -- model
       Spec                                       -- needs
       ScaffoldingOutput                          -- schema
       '[To "hImplement" ScaffoldingResult]       -- targets
       V2Effects                                  -- effs
       ScaffoldingCtx                             -- tpl
hScaffoldingHandler = ClaudeCodeLLMHandler @'Sonnet
  Nothing                  -- no system template
  v2ScaffoldingCompiled    -- user template
  buildScaffoldingContext  -- before
  routeAfterScaffolding    -- after
  where
    buildScaffoldingContext :: Spec -> Eff V2Effects (ScaffoldingCtx, SessionOperation)
    buildScaffoldingContext spec = do
      -- Build context for template
      let parentContext = case spec.specParentBranch of
            Nothing -> Nothing
            Just _  -> Just $ "Child of depth " <> T.pack (show spec.specDepth)

          ctx = ScaffoldingCtx
            { scSpec = spec
            , scParentContext = parentContext
            }

      -- Session operation - fresh session for scaffolding
      let sessionOp = StartFresh $ "v2/scaffold-d" <> T.pack (show spec.specDepth)

      pure (ctx, sessionOp)

    routeAfterScaffolding
      :: (ClaudeCodeResult ScaffoldingOutput, SessionId)
      -> Eff V2Effects (GotoChoice '[To "hImplement" ScaffoldingResult])
    routeAfterScaffolding (ccResult, sid) = do
      spec <- ask @Spec

      let result = ScaffoldingResult
            { srOutput = ccResult.ccrParsedOutput
            , srSessionId = T.pack (show sid)
            , srBranch = maybe "main" id spec.specParentBranch
            , srSpec = spec
            }

      -- Stash for implement phase
      updateMem (\ctx -> ctx { v2ScaffoldingStash = Just result })

      pure $ gotoChoice @"hImplement" result


-- ════════════════════════════════════════════════════════════════════════════
-- IMPLEMENT HANDLER (Logic Node with Subgraph)
-- ════════════════════════════════════════════════════════════════════════════

-- | Handler for hImplement node.
--
-- This is where the magic happens:
-- 1. Check shouldDecompose on rubric (policy)
-- 2. If decomposing:
--    a. Spawn child graph for each subsystem
--    b. Await loop: merge MRs as children complete
--    c. Implement glue layer using the now-real subsystems
-- 3. If leaf: implement all stubs directly
-- 4. Run CI loop until tests pass
-- 5. Return V2Result (becomes MR to parent)
hImplementHandler
  :: forall effs. ( Member (Subgraph Spec V2Result) effs
                  , Member (Error V2Error) effs
                  , LastMember IO effs
                  )
  => ScaffoldingResult
  -> Eff effs (GotoChoice '[To Exit V2Result])
hImplementHandler scaffoldResult = do
  let spec = scaffoldResult.srSpec
      rubric = scaffoldResult.srOutput.soRubric
      depth = spec.specDepth

  sendM $ putStrLn $ "[IMPLEMENT] Depth " <> show depth
    <> ", facets: " <> show (length rubric.srIdentifiedFacets)

  -- Policy decision: decompose or implement directly?
  if shouldDecompose depth rubric
    then do
      -- DECOMPOSE PATH
      sendM $ putStrLn "[IMPLEMENT] Decomposing into children..."

      -- Derive child specs from facets
      let subsystems = deriveSubsystemSpecs spec rubric.srIdentifiedFacets
          childSpecs = map (specFromSubsystem spec) subsystems

      -- Spawn children
      handles <- forM (zip subsystems childSpecs) $ \(ss, childSpec) -> do
        sendM $ putStrLn $ "[IMPLEMENT] Spawning child: " <> T.unpack ss.ssName
        handle <- spawnSelf @Spec @V2Result childSpec
        pure (ss.ssName, handle)

      sendM $ putStrLn $ "[IMPLEMENT] Spawned " <> show (length handles) <> " children"

      -- Await loop: collect children, merge MRs
      childResults <- collectAndMergeChildren (length handles)

      sendM $ putStrLn $ "[IMPLEMENT] All children complete, implementing glue layer"

      -- Now implement glue layer
      -- TODO: Call LLM via Session effect for glue implementation
      let glueResult = V2Result
            { vrSuccess = all crSuccess childResults
            , vrDescription = "Glue layer for: " <> spec.specDescription
            , vrBranch = scaffoldResult.srBranch
            , vrCommitHash = "glue-" <> T.pack (show (length childResults))
            , vrSubsystemName = Nothing  -- Parent, not a subsystem
            }

      pure $ gotoExit glueResult

    else do
      -- LEAF PATH (no decomposition)
      sendM $ putStrLn "[IMPLEMENT] Implementing directly (leaf node)"

      -- TODO: Call LLM via Session effect for direct implementation
      let leafResult = V2Result
            { vrSuccess = True  -- Would be set by CI loop
            , vrDescription = "Direct impl for: " <> spec.specDescription
            , vrBranch = scaffoldResult.srBranch
            , vrCommitHash = "impl-" <> T.pack (show depth)
            , vrSubsystemName = Nothing
            }

      pure $ gotoExit leafResult


-- ════════════════════════════════════════════════════════════════════════════
-- CHILD COLLECTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Collect child results and merge MRs as they complete.
--
-- This is the unfold/fold pattern:
-- * Unfold: spawned children in hImplementHandler
-- * Fold: collect results here, merge each MR
--
-- When we merge a child's MR:
-- * Our branch HEAD advances
-- * Siblings (still running) will see this via git
-- * They rebase onto our new HEAD (cascade)
collectAndMergeChildren
  :: forall effs. ( Member (Subgraph Spec V2Result) effs
                  , LastMember IO effs
                  )
  => Int  -- ^ Number of children to collect
  -> Eff effs [ChildResult]
collectAndMergeChildren total = go total []
  where
    go :: Int -> [ChildResult] -> Eff effs [ChildResult]
    go 0 acc = pure acc
    go remaining acc = do
      -- Block until any child completes
      (cid, v2result) <- awaitAny @Spec @V2Result

      let ChildId uuid = cid
      sendM $ putStrLn $ "[COLLECT] Child " <> show uuid <> " completed"

      -- TODO: Actually merge the MR via git
      -- For now, just record the result
      let childResult = ChildResult
            { crSubsystemName = maybe "unknown" id v2result.vrSubsystemName
            , crBranch = v2result.vrBranch
            , crCommitHash = v2result.vrCommitHash
            , crSuccess = v2result.vrSuccess
            }

      -- TODO: git merge child's branch into ours
      -- This advances our HEAD, triggering rebase cascade in siblings

      go (remaining - 1) (childResult : acc)
