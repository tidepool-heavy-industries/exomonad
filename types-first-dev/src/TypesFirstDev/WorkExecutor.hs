{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Executor for single-node Work graph.
--
-- The Work node handles everything via self-loops:
-- - Continue: immediate self-loop
-- - Spawn: spawn children, self-loop
-- - AwaitNext: block on child, self-loop
-- - Complete: exit graph
--
-- All exits except Complete loop back to the same node.
module TypesFirstDev.WorkExecutor
  ( runWork
  ) where

import Control.Monad.Freer (Eff)
import Tidepool.Graph.Goto.Internal (GotoChoice(..), OneOf(..))
import Tidepool.Graph.Goto (To)
import Tidepool.Graph.Types (Exit)

import TypesFirstDev.Types.Work (WorkInput, WorkResult)

-- ════════════════════════════════════════════════════════════════════════════
-- MAIN ENTRY POINT
-- ════════════════════════════════════════════════════════════════════════════

-- | Run the Work graph from WorkInput to WorkResult (Complete).
--
-- This is simpler than the old Plan/Impl executor because everything
-- is a self-loop. The handler does the routing logic; the executor
-- just keeps calling the handler until Complete is reached.
runWork
  :: forall es.
     (WorkInput -> Eff es (GotoChoice '[To "wgWork" WorkInput, To Exit WorkResult]))
  -> WorkInput
  -> Eff es WorkResult
runWork workHandler = loop
  where
    loop :: WorkInput -> Eff es WorkResult
    loop input = do
      choice <- workHandler input
      case choice of
        -- Self-loop: Continue/Spawn/AwaitNext all come here
        GotoChoice (Here nextInput) ->
          loop nextInput

        -- Exit: Complete
        GotoChoice (There (Here result)) ->
          pure result
