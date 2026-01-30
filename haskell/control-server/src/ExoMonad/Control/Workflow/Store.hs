{-# LANGUAGE OverloadedStrings #-}

module ExoMonad.Control.Workflow.Store
  ( WorkflowStore,
    initWorkflowStore,
    getWorkflowState,
    updateWorkflowState,
    defaultWorkflowState,
  )
where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO)
import Control.Lens (at, non, (.~), (^.))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import ExoMonad.Control.StopHook.Types (WorkflowStage (..), WorkflowState (..))

-- | Global store for workflow states, keyed by Session ID.
type WorkflowStore = TVar (Map Text WorkflowState)

-- | Initialize a new empty workflow store.
initWorkflowStore :: IO WorkflowStore
initWorkflowStore = newTVarIO Map.empty

-- | Get the workflow state for a session, or a default one if new.
getWorkflowState :: WorkflowStore -> Text -> IO WorkflowState
getWorkflowState store sessionId = do
  states <- readTVarIO store
  pure $ states ^. at sessionId . non defaultWorkflowState

-- | Update the workflow state for a session.
updateWorkflowState :: WorkflowStore -> Text -> WorkflowState -> IO ()
updateWorkflowState store sessionId newState = do
  atomically $ modifyTVar' store (at sessionId .~ Just newState)

-- | Default initial state for a new workflow.
defaultWorkflowState :: WorkflowState
defaultWorkflowState =
  WorkflowState
    { globalStops = 0,
      stageRetries = Map.empty,
      currentStage = StageBuild,
      lastBuildResult = Nothing,
      lastPRStatus = Nothing,
      lastTestResult = Nothing
    }
