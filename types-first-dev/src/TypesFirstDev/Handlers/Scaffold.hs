{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Scaffold node - analyzes spec, optionally spawns children, routes to TDDWriteTests.
module TypesFirstDev.Handlers.Scaffold
  ( -- * Types
    ScaffoldInput(..)
  , ScaffoldExit(..)
    -- * Handlers
  , scaffoldBefore
  , scaffoldAfter
  ) where

import Control.Monad.Freer (Eff, Member)
import Tidepool.Effect.Session (Session, SessionOperation(..), SessionId)
import Tidepool.Effect.Subgraph (Subgraph, spawnSelf)
import Tidepool.Graph.Goto (To, GotoChoice, gotoChoice, gotoExit)
import Tidepool.Graph.Types (Exit)

import TypesFirstDev.Context (ScaffoldTemplateCtx(..))
import TypesFirstDev.Types.Core (Spec)
import TypesFirstDev.Types.Nodes (ScaffoldInput(..), ScaffoldExit(..), TDDWriteTestsInput(..))
import TypesFirstDev.Types.Payloads (InitWorkPayload(..), MergeComplete)

-- ════════════════════════════════════════════════════════════════════════════
-- HANDLERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Before handler: build template context and session operation.
--
-- Returns tuple of (context, SessionOperation) for ClaudeCodeLLMHandler.
scaffoldBefore
  :: (Member Session es, Member (Subgraph Spec MergeComplete) es)
  => ScaffoldInput
  -> Eff es (ScaffoldTemplateCtx, SessionOperation)
scaffoldBefore input = do
  let ctx = ScaffoldTemplateCtx
        { spec = input.siSpec
        , parentContext = input.siParentContext
        }
  -- Start fresh conversation for scaffold analysis
  pure (ctx, StartFresh "v3/scaffold")

-- | After handler: spawn children if decomposed, route to TDDWriteTests.
--
-- Linear flow: Scaffold → TDDWriteTests (no Fork node).
-- Child spawning happens here via Subgraph effect.
scaffoldAfter
  :: (Member Session es, Member (Subgraph Spec MergeComplete) es)
  => (ScaffoldExit, SessionId)
  -> Eff es (GotoChoice '[To "v3TDDWriteTests" TDDWriteTestsInput, To Exit ScaffoldExit])
scaffoldAfter (exit, _sid) = case exit of
  ScaffoldInitWork { seCommit, seInterface, seContract, seTestPlan } -> do
    -- TODO: Spawn child graphs if decomposed via spawnSelf
    -- Children run asynchronously; ImplBarrier collects results

    -- Build InitWorkPayload for TDDWriteTests
    let scaffold = InitWorkPayload
          { iwpScaffoldCommit = seCommit
          , iwpInterfaceFile = seInterface
          , iwpContractSuite = seContract
          , iwpTestPlan = seTestPlan
          }

    -- Route to TDDWriteTests with full input
    let input = TDDWriteTestsInput
          { twiSpec = error "TODO: Get spec from context"
          , twiScaffold = scaffold
          }
    pure $ gotoChoice @"v3TDDWriteTests" input

  ScaffoldClarificationNeeded {} ->
    pure $ gotoExit exit
