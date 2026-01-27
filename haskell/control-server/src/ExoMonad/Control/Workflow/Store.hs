{-# LANGUAGE OverloadedStrings #-}

module ExoMonad.Control.Workflow.Store
  ( WorkflowStore
  , initWorkflowStore
  , getWorkflowState
  , updateWorkflowState
  , defaultWorkflowState
  ) where

import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, atomically, modifyTVar')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import ExoMonad.Control.StopHook.Types (WorkflowState(..), WorkflowStage(..))

-- | Global store for workflow states, keyed by Session ID.
type WorkflowStore = TVar (Map Text WorkflowState)

-- | Initialize a new empty workflow store.
initWorkflowStore :: IO WorkflowStore
initWorkflowStore = newTVarIO Map.empty

-- | Get the workflow state for a session, or a default one if new.
getWorkflowState :: WorkflowStore -> Text -> IO WorkflowState
getWorkflowState store sessionId = do
  states <- readTVarIO store
  case Map.lookup sessionId states of
    Just s  -> pure s
    Nothing -> pure defaultWorkflowState

-- | Update the workflow state for a session.
updateWorkflowState :: WorkflowStore -> Text -> WorkflowState -> IO ()
updateWorkflowState store sessionId newState = do
  atomically $ modifyTVar' store (Map.insert sessionId newState)

-- | Default initial state for a new workflow.
defaultWorkflowState :: WorkflowState
defaultWorkflowState = WorkflowState
  { wsGlobalStops = 0
  , wsStageRetries = Map.empty
  , wsCurrentStage = StageBuild
  , wsLastBuildResult = Nothing
  , wsLastPRStatus = Nothing
  , wsLastTestResult = Nothing
  }
