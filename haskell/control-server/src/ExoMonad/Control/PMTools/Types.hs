{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module ExoMonad.Control.PMTools.Types
  ( WorkflowState(..)
  , PmApproveExpansionArgs(..)
  , PmApproveExpansionResult(..)
  , PrioritizeItem(..)
  , PmPrioritizeArgs(..)
  , PrioritizeResultItem(..)
  , PmPrioritizeResult(..)
  , workflowStateToLabel
  , labelToWorkflowState
  , allWorkflowLabels
  , labelNeedsTLReview
  , labelNeedsPMApproval
  , labelReady
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), (.:), (.:?), withObject)
import Data.Text (Text)
import GHC.Generics (Generic)
import ExoMonad.Schema (deriveMCPTypeWith, defaultMCPOptions, (??), (~>), (?), MCPOptions(..), HasJSONSchema(..), arraySchema, emptySchema, SchemaType(..), describeField, jsonSchema)

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
-- PM-APPROVE-EXPANSION
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for pm_approve_expansion tool.
data PmApproveExpansionArgs = PmApproveExpansionArgs
  { paeaIssueNum :: Int
  , paeaDecision :: Text  -- "approve" or "reject"
  , paeaFeedback :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

$(deriveMCPTypeWith defaultMCPOptions { fieldPrefix = "paea" } ''PmApproveExpansionArgs
  [ 'paeaIssueNum ?? "Issue number"
  , 'paeaDecision ?? "Decision: 'approve' or 'reject'"
  , 'paeaFeedback ?? "Feedback (required for rejection, optional for approval)"
  ])

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

-- ════════════════════════════════════════════════════════════════════════════
-- PRIORITIZE
-- ════════════════════════════════════════════════════════════════════════════

-- | Argument for a single issue prioritization.
data PrioritizeItem = PrioritizeItem
  { piIssueNum    :: Int
  , piNewPriority :: Int
  , piRationale   :: Text
  }
  deriving stock (Show, Eq, Generic)

$(deriveMCPTypeWith defaultMCPOptions { fieldPrefix = "pi" } ''PrioritizeItem
  [ 'piIssueNum    ~> "issue_num"    ? "Issue number"
  , 'piNewPriority ~> "new_priority" ? "New priority value (0-4)."
  , 'piRationale   ?? "Reason for the priority change. Will be added to Priority History."
  ])

-- | Arguments for pm_prioritize tool.
data PmPrioritizeArgs = PmPrioritizeArgs
  { ppaUpdates :: [PrioritizeItem]
  }
  deriving stock (Show, Eq, Generic)

$(deriveMCPTypeWith defaultMCPOptions { fieldPrefix = "ppa" } ''PmPrioritizeArgs
  [ 'ppaUpdates ?? "List of issues to prioritize."
  ])

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
