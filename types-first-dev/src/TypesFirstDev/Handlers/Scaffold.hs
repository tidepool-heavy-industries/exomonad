{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

-- | Scaffold node - analyzes spec, optionally spawns children, routes to TDDWriteTests.
--
-- Child spawning (Phase 7):
-- - If exit has childSpecs and depth < maxDepth, spawns children via spawnSelf
-- - Children run asynchronously; ImplBarrier collects results via awaitAny
-- - Each child gets: incremented depth, parent SessionId for ForkFrom
module TypesFirstDev.Handlers.Scaffold
  ( -- * Types
    ScaffoldInput(..)
  , ScaffoldExit(..)
    -- * Handlers
  , scaffoldBefore
  , scaffoldAfter
  ) where

import Control.Monad (forM_)
import Control.Monad.Freer (Eff, Member)
import qualified Data.Text as T
import Tidepool.Effect.Session (Session, SessionOperation(..), SessionId)
import Tidepool.Effect.Subgraph (Subgraph, spawnSelf)
import Tidepool.Effect.GraphContext (GraphContext, getEntry)
import Tidepool.Graph.Goto (To, GotoChoice, gotoChoice, gotoExit)
import Tidepool.Graph.Types (Exit)

import TypesFirstDev.Context (ScaffoldTemplateCtx(..))
import TypesFirstDev.Types.Core (Spec(..), ParentContext(..))
import TypesFirstDev.Types.Nodes (ScaffoldInput(..), ScaffoldExit(..), TDDWriteTestsInput(..))
import TypesFirstDev.Types.Payloads (InitWorkPayload(..), MergeComplete)
import TypesFirstDev.Types.Shared (ChildSpec(..))

-- ════════════════════════════════════════════════════════════════════════════
-- HANDLERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Before handler: build template context and session operation.
--
-- Returns tuple of (context, SessionOperation) for ClaudeCodeLLMHandler.
--
-- Session strategy (Phase 8):
-- - Root nodes (no parent): StartFresh "v3/scaffold"
-- - Child nodes (with parent session): ForkFrom parentSessionId childSlug
--   This inherits parent's Claude Code context for continuity.
scaffoldBefore
  :: (Member Session es, Member (Subgraph ScaffoldInput MergeComplete) es)
  => ScaffoldInput
  -> Eff es (ScaffoldTemplateCtx, SessionOperation)
scaffoldBefore input = do
  let ctx = ScaffoldTemplateCtx
        { spec = input.siSpec
        , parentContext = input.siParentContext
        , clarificationNeeded = input.siClarificationNeeded
        }
  -- Session operation based on parent context
  let sessionOp = case input.siParentSessionId of
        Just parentSid ->
          -- Child inherits parent's Claude Code context
          ForkFrom parentSid ("child-" <> input.siSpec.sId)
        Nothing ->
          -- Root starts fresh
          StartFresh "v3/scaffold"
  pure (ctx, sessionOp)

-- | After handler: spawn children if decomposed, route to TDDWriteTests.
--
-- Child spawning (Phase 7):
-- 1. Check if scaffold produced childSpecs
-- 2. If childSpecs exist and depth < maxDepth, spawn children via spawnSelf
-- 3. Children run asynchronously (ImplBarrier collects via awaitAny)
-- 4. Route to TDDWriteTests for this node's own TDD cycle
scaffoldAfter
  :: ( Member Session es
     , Member (Subgraph ScaffoldInput MergeComplete) es
     , Member (GraphContext ScaffoldInput) es
     )
  => (ScaffoldExit, SessionId)
  -> Eff es (GotoChoice '[To "v3TDDWriteTests" TDDWriteTestsInput, To Exit ScaffoldExit])
scaffoldAfter (exit, sid) = case exit of
  ScaffoldInitWork { seCommit, seInterface, seContract, seTestPlan, seChildSpecs, seInterfaces = _seInterfaces } -> do
    -- Get entry for depth checking and spec access
    entry <- getEntry @ScaffoldInput
    let currentSpec = entry.siSpec
    let currentDepth = entry.siCurrentDepth
    let maxDepth = entry.siMaxDepth

    -- Phase 7: Spawn children if decomposed and depth allows
    case seChildSpecs of
      Just childSpecs | currentDepth < maxDepth -> do
        -- Spawn each child as a new graph instance with full context
        forM_ childSpecs $ \childSpec -> do
          -- Build parent context from scaffold output
          let childParentContext = Just ParentContext
                { pcInterface = T.pack seInterface  -- Convert FilePath to Text
                , pcAssignedCriteria = childSpec.csDescription
                }
          let childInput = ScaffoldInput
                { siSpec = childSpecToSpec childSpec
                , siParentContext = childParentContext
                , siCurrentDepth = currentDepth + 1
                , siMaxDepth = maxDepth
                , siParentSessionId = Just sid  -- For ForkFrom in child scaffold
                , siClarificationNeeded = Nothing  -- Fresh child, no prior failure
                }
          _ <- spawnSelf @ScaffoldInput @MergeComplete childInput
          pure ()
      _ -> pure ()  -- No children or max depth reached

    -- Build InitWorkPayload for TDDWriteTests
    let scaffold = InitWorkPayload
          { iwpScaffoldCommit = seCommit
          , iwpInterfaceFile = seInterface
          , iwpContractSuite = seContract
          , iwpTestPlan = seTestPlan
          }

    -- Route to TDDWriteTests with spec from GraphContext
    let tddInput = TDDWriteTestsInput
          { twiSpec = currentSpec
          , twiScaffold = scaffold
          }
    pure $ gotoChoice @"v3TDDWriteTests" tddInput

  ScaffoldClarificationNeeded {} ->
    pure $ gotoExit exit


-- | Convert ChildSpec to Spec for child graph spawning.
--
-- ChildSpec has the decomposed task info; we need to build a Spec
-- that the child graph can execute.
childSpecToSpec :: ChildSpec -> Spec
childSpecToSpec cs = Spec
  { sId = cs.csId
  , sDescription = cs.csDescription
  , sAcceptanceCriteria = cs.csAcceptanceCriteria
  , sTargetPath = cs.csTargetPath
  , sTestPath = cs.csTestPath
  , sComplexityConstraints = Nothing  -- Inherit from parent or default
  }
