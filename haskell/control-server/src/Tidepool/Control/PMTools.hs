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
-- without requiring GitHub metadata changes.
module Tidepool.Control.PMTools
  ( -- * Workflow State
    WorkflowState(..)
  , workflowStateToLabel
  , labelToWorkflowState
  , allWorkflowLabels

    -- * Constants
  , labelNeedsTLReview
  , labelNeedsPMApproval
  , labelReady

    -- * Helper Functions
  , getWorkflowState
  , setWorkflowState

    -- * PM Approve Expansion Tool
  , PmApproveExpansionGraph(..)
  , pmApproveExpansionLogic
  , PmApproveExpansionArgs(..)
  , PmApproveExpansionResult(..)

    -- * Prioritize Tool
  , PmPrioritizeArgs(..)
  , PrioritizeItem(..)
  , PmPrioritizeResult(..)
  , PrioritizeResultItem(..)
  , pmPrioritizeLogic
  , pmPrioritizeHandlers
  , PmPrioritizeGraph
  , appendRationale
  ) where

import Control.Monad (forM, forM_, when)
import Control.Monad.Freer (Eff, Member)
import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), (.:), (.:?), withObject)
import Data.Maybe (mapMaybe, listToMaybe, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Tidepool.Effects.GitHub
  ( GitHub, Issue(..), IssueState(..), Repo(..), getIssue, listIssues, defaultIssueFilter
  , updateIssue, addIssueLabel, removeIssueLabel, UpdateIssueInput(..), emptyUpdateIssueInput
  )
import Tidepool.Role (Role(..))
import Tidepool.Graph.Generic (AsHandler, type (:-))
import Tidepool.Graph.Generic.Core (EntryNode, ExitNode, LogicNode)
import Tidepool.Graph.Goto (Goto, GotoChoice, To, gotoExit)
import Tidepool.Graph.Types (type (:@), Input, UsesEffects, Exit, MCPExport, MCPToolDef, MCPRoleHint)
import Tidepool.Schema (HasJSONSchema(..), objectSchema, arraySchema, describeField, emptySchema, SchemaType(..))

-- ════════════════════════════════════════════════════════════════════════════
-- WORKFLOW STATE
-- ════════════════════════════════════════════════════════════════════════════

-- | PM Workflow state stored in GH labels.
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
labelToWorkflowState :: Text -> Maybe WorkflowState
labelToWorkflowState t
  | t == labelNeedsTLReview = Just NeedsTLReview
  | t == labelNeedsPMApproval = Just NeedsPMApproval
  | t == labelReady = Just Ready
  | otherwise = Nothing

-- | Map a 'WorkflowState' to its label.
workflowStateToLabel :: WorkflowState -> Text
workflowStateToLabel NeedsTLReview = labelNeedsTLReview
workflowStateToLabel NeedsPMApproval = labelNeedsPMApproval
workflowStateToLabel Ready = labelReady

-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Get the current workflow state of an issue by inspecting its labels.
--
-- If multiple workflow labels are present, returns the first one found.
getWorkflowState :: Member GitHub effs => Repo -> Int -> Eff effs (Maybe WorkflowState)
getWorkflowState repo issueNum = do
  mIssue <- getIssue repo issueNum False
  case mIssue of
    Nothing -> pure Nothing
    Just issue -> pure $ listToMaybe $ mapMaybe labelToWorkflowState issue.issueLabels

-- | Set the workflow state of an issue by updating its labels.
--
-- This removes any existing workflow labels before adding the new one.
setWorkflowState :: Member GitHub effs => Repo -> Int -> WorkflowState -> Eff effs ()
setWorkflowState repo issueNum newState = do
  mIssue <- getIssue repo issueNum False
  case mIssue of
    Nothing -> pure ()
    Just issue -> do
      -- Remove existing workflow labels
      forM_ issue.issueLabels $ \l ->
        when (l `elem` allWorkflowLabels) $
          removeIssueLabel repo issueNum l
      -- Add new workflow label
      addIssueLabel repo issueNum (workflowStateToLabel newState)

-- ════════════════════════════════════════════════════════════════════════════
-- PM-APPROVE-EXPANSION GRAPH
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for pm_approve_expansion tool.
data PmApproveExpansionArgs = PmApproveExpansionArgs
  { paeaIssueNum :: Int
  , paeaDecision :: Text  -- "approve" or "reject"
  , paeaFeedback :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance HasJSONSchema PmApproveExpansionArgs where
  jsonSchema = objectSchema
    [ ("issue_num", describeField "issue_num" "Issue number" (emptySchema TNumber))
    , ("decision", describeField "decision" "Decision: 'approve' or 'reject'" (emptySchema TString))
    , ("feedback", describeField "feedback" "Feedback (required for rejection, optional for approval)" (emptySchema TString))
    ]
    ["issue_num", "decision"]

instance FromJSON PmApproveExpansionArgs where
  parseJSON = withObject "PmApproveExpansionArgs" $ \v ->
    PmApproveExpansionArgs
      <$> v .: "issue_num"
      <*> v .: "decision"
      <*> v .:? "feedback"

instance ToJSON PmApproveExpansionArgs where
  toJSON args = object
    [ "issue_num" .= paeaIssueNum args
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
      :@ MCPToolDef ('("pm_approve_expansion", "Approve or reject an issue's expansion plan. Handles label transitions and feedback."))
      :@ MCPRoleHint 'PM

  , paeRun :: mode :- LogicNode
      :@ Input PmApproveExpansionArgs
      :@ UsesEffects '[GitHub, Goto Exit PmApproveExpansionResult]

  , paeExit :: mode :- ExitNode PmApproveExpansionResult
  }
  deriving Generic

-- | Core logic for pm_approve_expansion.
pmApproveExpansionLogic
  :: Member GitHub es
  => PmApproveExpansionArgs
  -> Eff es (GotoChoice '[To Exit PmApproveExpansionResult])
pmApproveExpansionLogic args = do
  -- TODO: Configurable repo
  let repo = Repo "tidepool/tidepool"
  mIssue <- getIssue repo args.paeaIssueNum False
  case mIssue of
    Nothing -> pure $ gotoExit PmApproveExpansionResult
      { paerNewStatus = "error"
      , paerMessage = "Issue #" <> T.pack (show args.paeaIssueNum) <> " not found"
      }
    Just issue -> do
      case args.paeaDecision of
        "approve" -> do
          setWorkflowState repo args.paeaIssueNum Ready
          pure $ gotoExit PmApproveExpansionResult
            { paerNewStatus = workflowStateToLabel Ready
            , paerMessage = "Issue approved. Moved to 'ready' state."
            }
        
        "reject" -> do
          setWorkflowState repo args.paeaIssueNum NeedsTLReview
          
          -- Append feedback to description
          case args.paeaFeedback of
            Just feedback -> do
              let oldDesc = issue.issueBody
                  newDesc = if T.null oldDesc 
                            then feedback 
                            else oldDesc <> "\n\n**PM Feedback:**\n" <> feedback
              updateIssue repo args.paeaIssueNum $ emptyUpdateIssueInput { uiiBody = Just newDesc }
              pure $ gotoExit PmApproveExpansionResult
                { paerNewStatus = workflowStateToLabel NeedsTLReview
                , paerMessage = "Issue rejected. Moved to 'needs-tl-review' and feedback appended."
                }
            Nothing -> 
              pure $ gotoExit PmApproveExpansionResult
                { paerNewStatus = workflowStateToLabel NeedsTLReview
                , paerMessage = "Issue rejected. Moved to 'needs-tl-review' (no feedback provided)."
                }

        _ -> pure $ gotoExit PmApproveExpansionResult
          { paerNewStatus = "error"
          , paerMessage = "Invalid decision: " <> args.paeaDecision <> ". Must be 'approve' or 'reject'."
          }

-- ════════════════════════════════════════════════════════════════════════════
-- PRIORITIZE TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Argument for a single issue prioritization.
data PrioritizeItem = PrioritizeItem
  { piIssueNum    :: Int
  , piNewPriority :: Int
  , piRationale   :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance HasJSONSchema PrioritizeItem where
  jsonSchema = objectSchema
    [ ("issue_num", describeField "issue_num" "Issue number" (emptySchema TNumber))
    , ("new_priority", describeField "new_priority" "New priority value (0-4)." (emptySchema TInteger))
    , ("rationale", describeField "rationale" "Reason for the priority change. Will be added to Priority History." (emptySchema TString))
    ]
    ["issue_num", "new_priority", "rationale"]

-- | Arguments for pm_prioritize tool.
data PmPrioritizeArgs = PmPrioritizeArgs
  { ppaUpdates :: [PrioritizeItem]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance HasJSONSchema PmPrioritizeArgs where
  jsonSchema = objectSchema
    [ ("updates", describeField "updates" "List of issues to prioritize." (arraySchema (jsonSchema @PrioritizeItem)))
    ]
    ["updates"]

-- | Result of a single issue prioritization.
data PrioritizeResultItem = PrioritizeResultItem
  { priIssueNum :: Int
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
      :@ MCPToolDef '("pm_prioritize", "Batch update issue priorities with rationale audit trail.")
      :@ MCPRoleHint 'PM

  , ppRun :: mode :- LogicNode
      :@ Input PmPrioritizeArgs
      :@ UsesEffects '[GitHub, Goto Exit PmPrioritizeResult]

  , ppExit :: mode :- ExitNode PmPrioritizeResult
  }
  deriving Generic

-- | Handlers for pm_prioritize graph.
pmPrioritizeHandlers :: Member GitHub effs => PmPrioritizeGraph (AsHandler effs)
pmPrioritizeHandlers = PmPrioritizeGraph
  { ppEntry = ()
  , ppRun = pmPrioritizeLogic
  , ppExit = ()
  }

-- | Logic for pm_prioritize tool.
--
-- Batch-updates issue priorities (via P0-P4 labels) and appends a rationale audit trail.
pmPrioritizeLogic
  :: Member GitHub effs
  => PmPrioritizeArgs
  -> Eff effs (GotoChoice '[To Exit PmPrioritizeResult])
pmPrioritizeLogic args = do
  -- TODO: Configurable repo
  let repo = Repo "tidepool/tidepool"
  
  results <- forM args.ppaUpdates $ \item -> do
    if item.piNewPriority < 0 || item.piNewPriority > 4
    then pure $ PrioritizeResultItem item.piIssueNum False (Just "Invalid priority: must be between 0 and 4")
    else do
      mIssue <- getIssue repo item.piIssueNum False
      case mIssue of
        Nothing -> pure $ PrioritizeResultItem item.piIssueNum False (Just "Issue not found")
        Just issue -> do
          let oldDesc = Just issue.issueBody
              newDesc = appendRationale oldDesc item.piNewPriority item.piRationale
              priorityLabel = "P" <> T.pack (show item.piNewPriority)
              
          -- Remove old priority labels
          forM_ ["P0", "P1", "P2", "P3", "P4"] $ \l ->
            when (l `elem` issue.issueLabels && l /= priorityLabel) $
              removeIssueLabel repo item.piIssueNum l
              
          -- Add new priority label
          addIssueLabel repo item.piIssueNum priorityLabel

          -- Update description
          updateIssue repo item.piIssueNum emptyUpdateIssueInput
            { uiiBody = Just newDesc
            }
          pure $ PrioritizeResultItem item.piIssueNum True Nothing

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
