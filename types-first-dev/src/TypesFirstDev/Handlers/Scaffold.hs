{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Scaffold node - analyzes spec, optionally spawns children, routes to Fork.
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
import Tidepool.Effect.Session (Session, SessionOperation(..), SessionId)
import Tidepool.Effect.Subgraph (Subgraph, spawnSelf)
import Tidepool.Graph.Goto (To, GotoChoice, gotoChoice, gotoExit)
import Tidepool.Graph.Types (Exit)

import TypesFirstDev.Context (ScaffoldTemplateCtx(..))
import TypesFirstDev.Types.Core (Spec)
import TypesFirstDev.Types.Nodes (ScaffoldInput(..), ScaffoldExit(..))
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

-- | After handler: spawn children if decomposed, route to Fork.
--
-- Returns tuple of (context, SessionOperation) for ClaudeCodeLLMHandler.
scaffoldAfter
  :: (Member Session es, Member (Subgraph Spec MergeComplete) es)
  => (ScaffoldExit, SessionId)
  -> Eff es (GotoChoice '[To "v3Fork" InitWorkPayload, To Exit ScaffoldExit])
scaffoldAfter (exit, _sid) = case exit of
  ScaffoldInitWork { seCommit, seInterface, seContract, seTestPlan } -> do
    -- TODO: Spawn child graphs if decomposed
    -- For now, just route to Fork - child spawning will be implemented
    -- in Phase 8 when we have the full runner context

    -- Route to Fork with InitWorkPayload
    let payload = InitWorkPayload
          { iwpScaffoldCommit = seCommit
          , iwpInterfaceFile = seInterface
          , iwpContractSuite = seContract
          , iwpTestPlan = seTestPlan
          }
    pure $ gotoChoice @"v3Fork" payload

  ScaffoldClarificationNeeded {} ->
    pure $ gotoExit exit
