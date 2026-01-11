{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Rebaser node - adapts to sibling changes after their merge.
module TypesFirstDev.Handlers.Rebaser
  ( RebaserInput(..)
  , RebaserExit(..)
  , rebaserBefore
  , rebaserAfter
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import Tidepool.Effect.Freer (Eff, Member)
import Tidepool.Effect.Session (Session, SessionOperation(..))
import Tidepool.Graph.Goto (To, GotoChoice, gotoChoice)
import Tidepool.StructuredOutput (StructuredOutput)
import Tidepool.StructuredOutput.ClaudeCodeSchema (ClaudeCodeSchema(..))
import Tidepool.StructuredOutput.DecisionTools (ToDecisionTools(..))

import TypesFirstDev.Context (RebaserTemplateCtx(..))
import TypesFirstDev.Types.Shared (NodeInfo)
import TypesFirstDev.Types.Payloads (MergeEvent, Adaptation)
import TypesFirstDev.Handlers.TDDWriteTests (TDDWriteTestsInput)
import TypesFirstDev.Handlers.Scaffold (ScaffoldInput)

-- ════════════════════════════════════════════════════════════════════════════
-- TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Rebaser node input.
data RebaserInput = RebaserInput
  { riNode :: NodeInfo
    -- ^ Node being rebased
  , riParentBranch :: Text
    -- ^ Parent branch name
  , riNewParentHead :: Text
    -- ^ New parent HEAD commit hash
  , riMergeEvent :: MergeEvent
    -- ^ Merge event from sibling that triggered rebase
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Rebaser node output (oneOf).
data RebaserExit
  = RebaserClean
      { reNewBase :: Text
        -- ^ New base commit hash after clean rebase
      }
  | RebaserAdapted
      { reAdaptedBase :: Text
        -- ^ Adapted base commit hash
      , reAdaptations :: [Adaptation]
        -- ^ Adaptations made to handle conflicts
      }
  | RebaserConflict
      { reConflictFile :: FilePath
        -- ^ File with unresolvable conflict
      , reOurChange :: Text
        -- ^ Our version of conflicting code
      , reTheirChange :: Text
        -- ^ Their version of conflicting code
      , reWhyUnresolvable :: Text
        -- ^ Explanation of why conflict cannot be auto-resolved
      }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput, ToDecisionTools)

instance ClaudeCodeSchema RebaserExit where
  ccDecisionTools = Just (toDecisionTools @RebaserExit)
  ccParseToolCall = parseToolCall @RebaserExit

-- ════════════════════════════════════════════════════════════════════════════
-- HANDLERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Before handler: build context.
rebaserBefore
  :: (Member Session es)
  => RebaserInput
  -> Eff es (RebaserTemplateCtx, SessionOperation)
rebaserBefore input = do
  let ctx = RebaserTemplateCtx
        { riNode = input.riNode
        , riParentBranch = input.riParentBranch
        , riNewParentHead = input.riNewParentHead
        , riMergeEvent = input.riMergeEvent
        }
  pure (ctx, StartFresh "v3/rebaser")

-- | After handler: route based on rebase result.
rebaserAfter
  :: (Member Session es)
  => RebaserExit
  -> Eff es (GotoChoice '[To "v3TDDWriteTests" TDDWriteTestsInput, To "v3Scaffold" ScaffoldInput])
rebaserAfter exit = case exit of
  RebaserClean _newBase -> do
    let testsInput = error "TODO: construct TDDWriteTestsInput with new parent state"
    pure $ gotoChoice @"v3TDDWriteTests" testsInput

  RebaserAdapted _newBase _adaptations -> do
    let testsInput = error "TODO: construct TDDWriteTestsInput with adaptations applied"
    pure $ gotoChoice @"v3TDDWriteTests" testsInput

  RebaserConflict _file _ours _theirs _why -> do
    let clarifyInput = error "TODO: construct ScaffoldInput for conflict escalation"
    pure $ gotoChoice @"v3Scaffold" clarifyInput
