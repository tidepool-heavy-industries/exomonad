{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedRecordDot #-}

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

    -- * PM Approve Expansion Tool
  , PmApproveExpansionGraph(..)
  , pmApproveExpansionLogic
  , PmApproveExpansionArgs(..)
  , PmApproveExpansionResult(..)
  ) where

import Control.Monad (forM_)
import Control.Monad.Freer (Eff, Member)
import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), (.:), (.:?), withObject)
import Data.Maybe (mapMaybe, listToMaybe, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Tidepool.Effects.BD (BD, getLabels, addLabel, removeLabel, getBead, updateBead, BeadInfo(..), UpdateBeadInput(..), emptyUpdateInput)
import Tidepool.Graph.Generic (type (:-))
import Tidepool.Graph.Generic.Core (EntryNode, ExitNode, LogicNode)
import Tidepool.Graph.Goto (Goto, GotoChoice, To, gotoExit)
import Tidepool.Graph.Types (type (:@), Input, UsesEffects, Exit, MCPExport, MCPToolDef)
import Tidepool.Schema (HasJSONSchema(..), objectSchema, describeField, emptySchema, SchemaType(..))

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
-- PM-APPROVE-EXPANSION GRAPH
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for pm_approve_expansion tool.
data PmApproveExpansionArgs = PmApproveExpansionArgs
  { paeaBeadId :: Text
  , paeaDecision :: Text  -- "approve" or "reject"
  , paeaFeedback :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance HasJSONSchema PmApproveExpansionArgs where
  jsonSchema = objectSchema
    [ ("bead_id", describeField "bead_id" "bead ID" (emptySchema TString))
    , ("decision", describeField "decision" "Decision: 'approve' or 'reject'" (emptySchema TString))
    , ("feedback", describeField "feedback" "Feedback (required for rejection, optional for approval)" (emptySchema TString))
    ]
    ["bead_id", "decision"]

instance FromJSON PmApproveExpansionArgs where
  parseJSON = withObject "PmApproveExpansionArgs" $ \v ->
    PmApproveExpansionArgs
      <$> v .: "bead_id"
      <*> v .: "decision"
      <*> v .:? "feedback"

instance ToJSON PmApproveExpansionArgs where
  toJSON args = object
    [ "bead_id" .= paeaBeadId args
    , "decision" .= paeaDecision args
    , "feedback" .= paeaFeedback args
    ]

-- | Result of pm_approve_expansion tool.
data PmApproveExpansionResult = PmApproveExpansionResult
  { paerNewStatus :: Text
  , paerMessage   :: Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON PmApproveExpansionResult where
  toJSON res = object
    [ "new_status" .= paerNewStatus res
    , "message"    .= paerMessage res
    ]

-- | Graph definition for pm_approve_expansion tool.
data PmApproveExpansionGraph mode = PmApproveExpansionGraph
  { paeEntry :: mode :- EntryNode PmApproveExpansionArgs
      :@ MCPExport
      :@ MCPToolDef ('("pm_approve_expansion", "Approve or reject a bead's expansion plan. Handles label transitions and feedback."))

  , paeRun :: mode :- LogicNode
      :@ Input PmApproveExpansionArgs
      :@ UsesEffects '[BD, Goto Exit PmApproveExpansionResult]

  , paeExit :: mode :- ExitNode PmApproveExpansionResult
  }
  deriving Generic

-- | Core logic for pm_approve_expansion.
pmApproveExpansionLogic
  :: Member BD es
  => PmApproveExpansionArgs
  -> Eff es (GotoChoice '[To Exit PmApproveExpansionResult])
pmApproveExpansionLogic args = do
  mBead <- getBead args.paeaBeadId
  case mBead of
    Nothing -> pure $ gotoExit PmApproveExpansionResult
      { paerNewStatus = "error"
      , paerMessage = "Bead not found: " <> args.paeaBeadId
      }
    Just bead -> do
      case args.paeaDecision of
        "approve" -> do
          setWorkflowState args.paeaBeadId Ready
          pure $ gotoExit PmApproveExpansionResult
            { paerNewStatus = stateToLabel Ready
            , paerMessage = "Bead approved. Moved to 'ready' state."
            }
        
        "reject" -> do
          setWorkflowState args.paeaBeadId NeedsTLReview
          
          -- Append feedback to description
          case args.paeaFeedback of
            Just feedback -> do
              let oldDesc = fromMaybe "" bead.biDescription
                  newDesc = if T.null oldDesc 
                            then feedback 
                            else oldDesc <> "\n\n**PM Feedback:**\n" <> feedback
              updateBead args.paeaBeadId $ emptyUpdateInput { ubiDescription = Just newDesc }
              pure $ gotoExit PmApproveExpansionResult
                { paerNewStatus = stateToLabel NeedsTLReview
                , paerMessage = "Bead rejected. Moved to 'needs-tl-review' and feedback appended."
                }
            Nothing -> 
              pure $ gotoExit PmApproveExpansionResult
                { paerNewStatus = stateToLabel NeedsTLReview
                , paerMessage = "Bead rejected. Moved to 'needs-tl-review' (no feedback provided)."
                }

        _ -> pure $ gotoExit PmApproveExpansionResult
          { paerNewStatus = "error"
          , paerMessage = "Invalid decision: " <> args.paeaDecision <> ". Must be 'approve' or 'reject'."
          }
