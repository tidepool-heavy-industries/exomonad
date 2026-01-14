{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- | Before/After handlers for MyTask graph
module HumanDrivenDev.MyTaskHandlers
  ( myTaskBefore
  , myTaskAfter
  ) where

import Control.Monad.Freer (Eff, Member)

import Tidepool.Graph.Memory (Memory, getMem, updateMem)
import Tidepool.Graph.Goto (To, GotoChoice, gotoExit, gotoSelf)
import Tidepool.Graph.Types (Exit, Self)

import HumanDrivenDev.Types.MyTask
  ( MyTaskInput(..)
  , MyTaskExit(..)
  , MyTaskMem(..)
  )
import HumanDrivenDev.Types.Core (FinalResult(..))
import HumanDrivenDev.MyTaskContext (MyTaskContext, buildMyTaskContext)

-- | Before handler: build template context from input + memory
--
-- For standard LLM nodes, before handler just returns the template context.
myTaskBefore
  :: Member (Memory MyTaskMem) effs
  => MyTaskInput
  -> Eff effs MyTaskContext
myTaskBefore input = do
  mem <- getMem @MyTaskMem
  pure $ buildMyTaskContext input mem

-- | After handler: route based on exit type
--
-- Receives the parsed LLM output (MyTaskExit) and decides next transition.
--
-- IMPORTANT: Data flow dead-ends are bugs!
-- If exit type captures data, thread it forward to next node.
myTaskAfter
  :: Member (Memory MyTaskMem) effs
  => MyTaskInput
  -> MyTaskExit  -- ^ Parsed LLM output
  -> Eff effs (GotoChoice '[To Self MyTaskInput, To Exit FinalResult])
myTaskAfter input exit = case exit of
  -- COMPLETE: Exit the graph with final result
  TaskComplete result details -> do
    let finalResult = FinalResult
          { frSummary = result
          , frArtifacts = []  -- TODO: extract from details
          , frMetrics = Nothing
          }
    pure $ gotoExit finalResult

  -- RETRY: Self-loop with updated input
  TaskRetry reason adjustments -> do
    -- Update memory with attempt history
    updateMem @MyTaskMem $ \m -> m
      { mtmAttemptHistory = m.mtmAttemptHistory ++ [reason]
      }

    -- Build retry input
    let retryInput = input
          { mtiAttemptCount = input.mtiAttemptCount + 1
          , mtiContext = input.mtiContext ++ adjustments
          }

    pure $ gotoSelf retryInput

  -- SPAWN: Would spawn subtasks, then self-loop
  -- (Subgraph effect not shown in this template)
  TaskSpawn subtasks strategy -> do
    -- TODO: Use Subgraph effect to spawn children
    -- For now, just self-loop
    pure $ gotoSelf input

  -- FAILED: Exit with error in result
  TaskFailed err diagnostics -> do
    let errorResult = FinalResult
          { frSummary = "Failed: " <> err
          , frArtifacts = []
          , frMetrics = Nothing
          }
    pure $ gotoExit errorResult
