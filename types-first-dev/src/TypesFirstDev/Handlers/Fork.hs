{-# LANGUAGE DataKinds #-}

-- | Fork node handler for V3 TDD protocol.
--
-- The Fork node spawns TDDWriteTests and ImplBarrier in parallel.
-- Each receives appropriate payloads derived from the InitWorkPayload.
module TypesFirstDev.Handlers.Fork
  ( forkHandler
  ) where

import Control.Monad.Freer (Eff)
import Tidepool.Graph.Types (HList(..))

import TypesFirstDev.Types.Nodes (TDDWriteTestsInput(..))
import TypesFirstDev.Types.Payloads (InitWorkPayload(..))

-- ════════════════════════════════════════════════════════════════════════════
-- HANDLER
-- ════════════════════════════════════════════════════════════════════════════

-- | Fork handler: distribute work to TDDWriteTests and ImplBarrier.
--
-- This handler:
-- 1. Takes InitWorkPayload from Scaffold
-- 2. Creates TDDWriteTestsInput for the TDD path
-- 3. Passes through InitWorkPayload for the barrier (which waits for children)
-- 4. Returns HList of spawn payloads for parallel dispatch
--
-- Handler type (per ForkNode definition):
--   InitWorkPayload -> Eff es (HList '[TDDWriteTestsInput, InitWorkPayload])
forkHandler
  :: InitWorkPayload
  -> Eff es (HList '[TDDWriteTestsInput, InitWorkPayload])
forkHandler initWork = pure $
  -- TDDWriteTests payload: needs spec and scaffold info
  TDDWriteTestsInput
    { twiSpec = error "TODO: Get spec from context"
    , twiScaffold = initWork
    }
  -- ImplBarrier payload: just pass through InitWorkPayload
  -- (barrier waits for TDD result and any child MergeComplete)
  ::: initWork
  ::: HNil
