{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

-- | Assembled handler record for Work graph.
--
-- Wires the Work handler into a WorkGraph record.
module TypesFirstDev.WorkHandlersAssembled
  ( workHandlers
  ) where

import Data.Proxy (Proxy(..))
import Tidepool.Graph.Generic (AsHandler)
import Tidepool.Graph.Goto (ClaudeCodeLLMHandler(..))
import Tidepool.Graph.Types (ModelChoice(..))

import TypesFirstDev.WorkGraph (WorkGraph(..))
import TypesFirstDev.WorkInterpreters (WorkEffects)
import TypesFirstDev.WorkTemplates (workCompiled)
import TypesFirstDev.WorkHandlers (workBefore, workAfter)

-- | Wired handler graph for single-node Work protocol.
--
-- One node that handles everything:
-- - Continue: self-loop immediately
-- - Spawn: spawn children, await them
-- - AwaitNext: block on child completion
-- - Complete: exit graph
workHandlers :: WorkGraph (AsHandler WorkEffects)
workHandlers = WorkGraph
  { -- Entry point marker
    wgEntry = Proxy

    -- ════════════════════════════════════════════════════════════════
    -- WORK
    -- ════════════════════════════════════════════════════════════════

  , wgWork = ClaudeCodeLLMHandler @'Sonnet
      Nothing  -- No worktree path
      workCompiled
      workBefore
      workAfter

    -- ════════════════════════════════════════════════════════════════
    -- EXIT
    -- ════════════════════════════════════════════════════════════════

  , wgExit = Proxy
  }
