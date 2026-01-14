{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- | Effect interpreter for MyTask graph
--
-- This is a placeholder showing the pattern.
-- Actual execution will be wired up in a runner (native-server or CF worker).
module HumanDrivenDev.MyTaskInterpreter
  ( runMyTaskGraph
  ) where

import HumanDrivenDev.MyTaskGraph (MyTaskGraph(..))
import HumanDrivenDev.Types.Core (TopLevelInput, FinalResult)
import HumanDrivenDev.Types.MyTask (MyTaskInput, emptyMyTaskMem)
import HumanDrivenDev.MyTaskHandlers (myTaskBefore, myTaskAfter)

-- | Run the MyTask graph with all effects interpreted
--
-- This is a placeholder. The actual runner would:
-- 1. Initialize Memory effect with emptyMyTaskMem
-- 2. Wire up LLM effect to Anthropic API
-- 3. Execute handlers via dispatchGraph
-- 4. Return final result
--
-- For native execution: see tidepool-native-server
-- For WASM execution: see deploy/ (Cloudflare Workers)
runMyTaskGraph :: MyTaskGraph mode -> TopLevelInput -> IO FinalResult
runMyTaskGraph graph input = do
  -- TODO: Implement actual graph execution
  -- This requires:
  -- - Converting TopLevelInput -> MyTaskInput
  -- - Running Memory, LLM, and other effects
  -- - Dispatching via tidepool-core's runGraph
  error "runMyTaskGraph: not implemented - see tidepool-native-server for execution pattern"
