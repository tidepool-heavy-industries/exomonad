{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Shared types and label constants for PM tools.
--
-- These tools use labels as pseudo-states to track workflow progress
-- without requiring BD schema changes.
module Tidepool.Control.PMTools
  ( -- * Workflow State
    WorkflowState(..)
  , labelNeedsTLReview
  , labelNeedsPMApproval
  , labelReady
  , allWorkflowLabels

    -- * Helper Functions
  , getWorkflowState
  , setWorkflowState

    -- * Prioritize Tool
  , PmPrioritizeArgs(..)
  , PrioritizeItem(..)
  , PmPrioritizeResult(..)
  , PrioritizeResultItem(..)
  , pmPrioritizeLogic
  , pmPrioritizeHandlers
  , PmPrioritizeGraph
  ) where

import Control.Monad (forM, forM_)
import Control.Monad.Freer (Eff, Member)
import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (mapMaybe, listToMaybe, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Tidepool.Effects.BD (BD, getLabels, addLabel, removeLabel, getBead, updateBead, BeadInfo(..), UpdateBeadInput(..), emptyUpdateInput)
import Tidepool.Graph.Generic (AsHandler, type (:-))
import Tidepool.Graph.Generic.Core (EntryNode, ExitNode, LogicNode)
import Tidepool.Graph.Goto (Goto, GotoChoice, To, gotoExit)
import Tidepool.Graph.Types (type (:@), Input, UsesEffects, Exit, MCPExport, MCPToolDef)
import Tidepool.Schema (HasJSONSchema(..), objectSchema, arraySchema, emptySchema, SchemaType(..), describeField)

-- ════════════════════════════════════════════════════════════════════════════
-- WORKFLOW STATE
-- ════════════════════════════════════════════════════════════════════════════

-- | PM Workflow state stored in BD labels.
data WorkflowState
  = NeedsTLReview
  | NeedsPMApproval
  | Ready
  deriving stock (Show, Eq, Generic, Enum, Bounded)
  deriving anyclass (FromJSON, ToJSON)

-- | Label for 'NeedsTLReview'.
labelNeedsTLReview :: Text
labelNeedsTLReview = "needs-tl-review"

-- | Label for 'NeedsPMApproval'.
labelNeedsPMApproval :: Text
labelNeedsPMApproval = "needs-pm-approval"

-- | Label for 'Ready'.
labelReady :: Text
labelReady = "ready"

-- | All possible workflow labels.
allWorkflowLabels :: [Text]
allWorkflowLabels =
  [ labelNeedsTLReview
  , labelNeedsPMApproval
  , labelReady
  ]

-- | Map a label to a 'WorkflowState'.
labelToState :: Text -> Maybe WorkflowState
labelToState t
  | t == labelNeedsTLReview = Just NeedsTLReview
  | t == labelNeedsPMApproval = Just NeedsPMApproval
  | t == labelReady = Just Ready
  | otherwise = Nothing

-- | Map a 'WorkflowState' to its label.
stateToLabel :: WorkflowState -> Text
stateToLabel NeedsTLReview = labelNeedsTLReview
stateToLabel NeedsPMApproval = labelNeedsPMApproval
stateToLabel Ready = labelReady

-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Get the current workflow state of a bead by inspecting its labels.
--
-- If multiple workflow labels are present, returns the first one found.
getWorkflowState :: Member BD effs => Text -> Eff effs (Maybe WorkflowState)
getWorkflowState beadId = do
  labels <- getLabels beadId
  pure $ listToMaybe $ mapMaybe labelToState labels

-- | Set the workflow state of a bead by updating its labels.
--
-- This removes any existing workflow labels before adding the new one.
setWorkflowState :: Member BD effs => Text -> WorkflowState -> Eff effs ()
setWorkflowState beadId newState = do
  -- Remove existing workflow labels
  labels <- getLabels beadId
  let toRemove = filter (`elem` allWorkflowLabels) labels
  forM_ toRemove $ removeLabel beadId

  -- Add new label
  addLabel beadId (stateToLabel newState)

-- ════════════════════════════════════════════════════════════════════════════
-- PRIORITIZE TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Argument for a single bead prioritization.
data PrioritizeItem = PrioritizeItem
  { piBeadId      :: Text
  , piNewPriority :: Int
  , piRationale   :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance HasJSONSchema PrioritizeItem where
  jsonSchema = objectSchema
    [ ("bead_id", describeField "bead_id" "ID of the bead to prioritize (e.g. tidepool-abc)." (emptySchema TString))
    , ("new_priority", describeField "new_priority" "New priority value (0-4)." (emptySchema TInteger))
    , ("rationale", describeField "rationale" "Reason for the priority change. Will be added to Priority History." (emptySchema TString))
    ]
    ["bead_id", "new_priority", "rationale"]

-- | Arguments for pm_prioritize tool.
data PmPrioritizeArgs = PmPrioritizeArgs
  { ppaUpdates :: [PrioritizeItem]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance HasJSONSchema PmPrioritizeArgs where
  jsonSchema = objectSchema
    [ ("updates", describeField "updates" "List of beads to prioritize." (arraySchema (jsonSchema @PrioritizeItem)))
    ]
    ["updates"]

-- | Result of a single bead prioritization.
data PrioritizeResultItem = PrioritizeResultItem
  { priBeadId :: Text
  , priSuccess :: Bool
  , priError   :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Result of pm_prioritize tool.
data PmPrioritizeResult = PmPrioritizeResult
  { pprResults :: [PrioritizeResultItem]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Graph definition for pm_prioritize tool.
data PmPrioritizeGraph mode = PmPrioritizeGraph
  { ppEntry :: mode :- EntryNode PmPrioritizeArgs
      :@ MCPExport
      :@ MCPToolDef '("pm_prioritize", "Batch update bead priorities with rationale audit trail.")

  , ppRun :: mode :- LogicNode
      :@ Input PmPrioritizeArgs
      :@ UsesEffects '[BD, Goto Exit PmPrioritizeResult]

  , ppExit :: mode :- ExitNode PmPrioritizeResult
  }
  deriving Generic

-- | Handlers for pm_prioritize graph.
pmPrioritizeHandlers :: Member BD effs => PmPrioritizeGraph (AsHandler effs)
pmPrioritizeHandlers = PmPrioritizeGraph
  { ppEntry = ()
  , ppRun = pmPrioritizeLogic
  , ppExit = ()
  }

-- | Logic for pm_prioritize tool.
--
-- Batch updates bead priorities and appends rationale to their descriptions.
pmPrioritizeLogic
  :: Member BD effs
  => PmPrioritizeArgs
  -> Eff effs (GotoChoice '[To Exit PmPrioritizeResult])
pmPrioritizeLogic args = do
  results <- forM args.ppaUpdates $ \item -> do
    mBead <- getBead item.piBeadId
    case mBead of
      Nothing -> pure $ PrioritizeResultItem item.piBeadId False (Just "Bead not found")
      Just bead -> do
        let oldDesc = bead.biDescription
            newDesc = appendRationale oldDesc item.piNewPriority item.piRationale

        updateBead item.piBeadId emptyUpdateInput
          { ubiPriority = Just item.piNewPriority
          , ubiDescription = Just newDesc
          }
        pure $ PrioritizeResultItem item.piBeadId True Nothing

  pure $ gotoExit $ PmPrioritizeResult results

-- | Helper to append rationale to the Priority History section of a description.
appendRationale :: Maybe Text -> Int -> Text -> Text
appendRationale mDesc newPriority rationale =
  let desc = fromMaybe "" mDesc
      historyLine = "- Priority " <> T.pack (show newPriority) <> ": " <> rationale
      header = "## Priority History"
  in if header `T.isInfixOf` desc
     then
       if T.isSuffixOf "\n" desc
       then desc <> historyLine
       else desc <> "\n" <> historyLine
     else
       let prefix = if T.null (T.strip desc) then "" else T.stripEnd desc <> "\n\n"
       in prefix <> header <> "\n" <> historyLine
