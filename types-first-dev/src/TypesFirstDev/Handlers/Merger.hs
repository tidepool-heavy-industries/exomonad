{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Merger node - files MR to parent after TDD approval.
module TypesFirstDev.Handlers.Merger
  ( MergerInput(..)
  , MergerExit(..)
  , MergeRejectedReason(..)
  , mergerBefore
  , mergerAfter
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import Tidepool.Effect.Freer (Eff, Member)
import Tidepool.Effect.Session (Session, SessionOperation(..))
import Tidepool.Graph.Goto (To, GotoChoice, gotoChoice, gotoExit)
import Tidepool.StructuredOutput (StructuredOutput)
import Tidepool.StructuredOutput.ClaudeCodeSchema (ClaudeCodeSchema(..))
import Tidepool.StructuredOutput.DecisionTools (ToDecisionTools(..))

import TypesFirstDev.Context (MergerTemplateCtx(..))
import TypesFirstDev.Types.Shared (NodeInfo, ImpactLevel, ChangeEntry)
import TypesFirstDev.Types.Payloads (TDDApproval, MergeComplete(..))

-- ════════════════════════════════════════════════════════════════════════════
-- TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Merger node input.
data MergerInput = MergerInput
  { miParentNode :: NodeInfo
    -- ^ Parent node information
  , miChildNode :: NodeInfo
    -- ^ Child node being merged
  , miTddApproval :: TDDApproval
    -- ^ TDD approval from child
  , miContractSuite :: FilePath
    -- ^ Path to contract suite for verification
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Merger node output (oneOf).
data MergerExit
  = MergerComplete
      { mexCommit :: Text
        -- ^ Commit hash of merged work
      , mexAuthor :: Text
        -- ^ Author of merge commit
      , mexImpactLevel :: ImpactLevel
        -- ^ Impact level (minor/moderate/major)
      , mexChanges :: [ChangeEntry]
        -- ^ Summary of changes made
      }
  | MergerRejected
      { mexReason :: MergeRejectedReason
        -- ^ Why merge was rejected
      , mexDetails :: Text
        -- ^ Details of rejection
      , mexFailingTests :: [Text]
        -- ^ Tests that fail
      }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput, ToDecisionTools)

instance ClaudeCodeSchema MergerExit where
  ccDecisionTools = Just (toDecisionTools @MergerExit)
  ccParseToolCall = parseToolCall @MergerExit

-- | Reason for merge rejection.
data MergeRejectedReason
  = ContractViolation
  | BuildFailure
  | IntegrationFailure
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- ════════════════════════════════════════════════════════════════════════════
-- HANDLERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Before handler: build context.
mergerBefore
  :: (Member Session es)
  => MergerInput
  -> Eff es (MergerTemplateCtx, SessionOperation)
mergerBefore input = do
  let ctx = MergerTemplateCtx
        { miParentNode = input.miParentNode
        , miChildNode = input.miChildNode
        , miTddApproval = input.miTddApproval
        , miContractSuite = input.miContractSuite
        }
  pure (ctx, StartFresh "v3/merger")

-- | After handler: file MR or reject.
mergerAfter
  :: (Member Session es)
  => MergerExit
  -> Eff es (GotoChoice '[Goto Exit MergeComplete, To "v3Impl" ImplInput])
mergerAfter exit = case exit of
  MergerComplete commit author impact changes -> do
    let mergeResult = MergeComplete
          { mcCommitHash = commit
          , mcAuthor = author
          , mcImpactLevel = impact
          , mcMergedModules = []
          , mcImpactedTests = []
          }
    pure $ gotoExit mergeResult

  MergerRejected _reason _details _failing -> do
    let retryInput = error "TODO: construct ImplInput for retry"
    pure $ gotoChoice @"v3Impl" retryInput
