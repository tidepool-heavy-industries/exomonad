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
  ) where

import Control.Monad (forM_)
import Control.Monad.Freer (Eff, Member)
import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)

import Tidepool.Effects.BD (BD, getLabels, addLabel, removeLabel)

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
