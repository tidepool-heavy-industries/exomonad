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
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import Control.Monad.Freer (Eff, Member)
import Tidepool.Effect.Session (Session, SessionOperation(..))
import Tidepool.Effect.Subgraph (Subgraph, spawnSelf)
import Tidepool.Graph.Goto (To, GotoChoice, gotoChoice, gotoExit)
import Tidepool.Graph.Types (Exit)
import Tidepool.StructuredOutput (StructuredOutput)
import Tidepool.StructuredOutput.ClaudeCodeSchema (ClaudeCodeSchema(..))
import Tidepool.StructuredOutput.DecisionTools (ToDecisionTools(..))

import TypesFirstDev.Context (ScaffoldTemplateCtx(..))
import TypesFirstDev.Types.Core (Spec, ParentContext(..))
import TypesFirstDev.Types.Shared (PlannedTest, ChildSpec, InterfaceFile)
import TypesFirstDev.Types.Payloads (InitWorkPayload(..), MergeComplete)

-- ════════════════════════════════════════════════════════════════════════════
-- TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Scaffold node input.
data ScaffoldInput = ScaffoldInput
  { siSpec :: Spec
    -- ^ Work specification to analyze
  , siParentContext :: Maybe ParentContext
    -- ^ Context from parent if this is a child decomposition
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Scaffold node output (oneOf).
data ScaffoldExit
  = ScaffoldInitWork
      { seCommit :: Text
        -- ^ Git commit hash of scaffold work (types + tests)
      , seInterface :: FilePath
        -- ^ Path to interface/types file
      , seContract :: FilePath
        -- ^ Path to contract suite (core tests)
      , seTestPlan :: [PlannedTest]
        -- ^ Tests planned for each criterion
      , seChildSpecs :: Maybe [ChildSpec]
        -- ^ Child decompositions if any
      , seInterfaces :: Maybe [InterfaceFile]
        -- ^ Interface files for children
      }
  | ScaffoldClarificationNeeded
      { seQuestion :: Text
        -- ^ Question for the user
      , seAmbiguityReference :: Text
        -- ^ Which part of spec is ambiguous
      , seSpecSentence :: Text
        -- ^ Exact sentence from spec that needs clarification
      }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput, ToDecisionTools)

instance ClaudeCodeSchema ScaffoldExit where
  ccDecisionTools = Just (toDecisionTools @ScaffoldExit)
  ccParseToolCall = parseToolCall @ScaffoldExit

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
scaffoldAfter
  :: (Member Session es, Member (Subgraph Spec MergeComplete) es)
  => ScaffoldExit
  -> Eff es (GotoChoice '[To "v3Fork" InitWorkPayload, To Exit ScaffoldExit])
scaffoldAfter exit = case exit of
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
