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

import Control.Monad.Freer (Eff, Member)
import Tidepool.Effect.Session (Session, SessionOperation(..))
import Tidepool.Graph.Goto (To, GotoChoice, gotoChoice)
import Tidepool.StructuredOutput (StructuredOutput)
import Tidepool.StructuredOutput.ClaudeCodeSchema (ClaudeCodeSchema(..))
import Tidepool.StructuredOutput.DecisionTools (ToDecisionTools(..))

import TypesFirstDev.Context (RebaserTemplateCtx(..))
import TypesFirstDev.Types.Shared (NodeInfo)
import TypesFirstDev.Types.Payloads (MergeEvent, Adaptation)

-- ════════════════════════════════════════════════════════════════════════════
-- FORWARD REFERENCES (to avoid circular imports)
-- ════════════════════════════════════════════════════════════════════════════

-- | Placeholder for TDDWriteTestsInput (from TDDWriteTests handler)
data TDDWriteTestsInput_FwdRef

-- | Placeholder for ScaffoldInput (from Scaffold handler)
data ScaffoldInput_FwdRef

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
--
-- Returns tuple of (context, SessionOperation) for ClaudeCodeLLMHandler.
rebaserBefore
  :: (Member Session es)
  => RebaserInput
  -> Eff es (RebaserTemplateCtx, SessionOperation)
rebaserBefore input = do
  let ctx = RebaserTemplateCtx
        { node = input.riNode
        , parentBranch = input.riParentBranch
        , newParentHead = input.riNewParentHead
        , mergeEvent = input.riMergeEvent
        }
  pure (ctx, StartFresh "v3/rebaser")

-- | After handler: route based on rebase result.
rebaserAfter
  :: (Member Session es)
  => RebaserExit
  -> Eff es (GotoChoice '[To "v3TDDWriteTests" TDDWriteTestsInput_FwdRef, To "v3Scaffold" ScaffoldInput_FwdRef])
rebaserAfter exit = case exit of
  RebaserClean _newBase ->
    pure $ gotoChoice @"v3TDDWriteTests" (error "TODO: construct TDDWriteTestsInput with new parent state" :: TDDWriteTestsInput_FwdRef)

  RebaserAdapted _newBase _adaptations ->
    pure $ gotoChoice @"v3TDDWriteTests" (error "TODO: construct TDDWriteTestsInput with adaptations applied" :: TDDWriteTestsInput_FwdRef)

  RebaserConflict _file _ours _theirs _why ->
    pure $ gotoChoice @"v3Scaffold" (error "TODO: construct ScaffoldInput for conflict escalation" :: ScaffoldInput_FwdRef)
