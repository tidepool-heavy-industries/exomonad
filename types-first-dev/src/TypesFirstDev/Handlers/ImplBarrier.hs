{-# LANGUAGE FlexibleContexts #-}

-- | ImplBarrier node - async merge loop waiting for tests + children.
module TypesFirstDev.Handlers.ImplBarrier
  ( implBarrierHandler
  ) where

import Control.Monad.Freer (Eff)
import Tidepool.Graph.Goto (To, GotoChoice, gotoChoice)

import TypesFirstDev.Types.Payloads (TestsReadyPayload)

-- ════════════════════════════════════════════════════════════════════════════
-- FORWARD REFERENCE (to avoid circular imports)
-- ════════════════════════════════════════════════════════════════════════════

-- | Placeholder for ImplInput (from Impl handler)
data ImplInput_FwdRef

-- ════════════════════════════════════════════════════════════════════════════
-- HANDLER
-- ════════════════════════════════════════════════════════════════════════════

-- | ImplBarrier handler: async merge loop.
-- Awaits tests + children, merges children, routes to Impl when all complete.
--
-- NOTE: Phase 7 stub. Full async implementation deferred to Phase 8.
implBarrierHandler
  :: TestsReadyPayload
  -> Eff es (GotoChoice '[To "v3Impl" ImplInput_FwdRef])
implBarrierHandler _testsPayload = do
  -- TODO: Implement async merge loop in Phase 8
  -- For now, return error to indicate this is a stub
  pure $ gotoChoice @"v3Impl" (error "TODO: ImplBarrier async merge loop (Phase 8)" :: ImplInput_FwdRef)
