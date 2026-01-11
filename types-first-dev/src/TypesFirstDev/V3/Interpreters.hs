{-# LANGUAGE FlexibleContexts #-}

-- | V3 Effect Interpreter - chains effects for TDD execution
--
-- Composes interpreters for:
-- - Session: Claude Code session management via mantle
-- - Memory TDDMem: Shared conversation state
-- - Memory ImplMem: Impl node private state
-- - Subgraph Spec: Child graph spawning
--
-- Note: This is a placeholder for Phase 7. Full implementation will:
-- - Use TVars for mutable state
-- - Wire Session, Memory, and Subgraph effects
-- - Enable async child graph execution
module TypesFirstDev.V3.Interpreters
  ( V3Effects
  , defaultSessionConfig
  ) where

import Tidepool.Session.Executor (defaultSessionConfig)

-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT STACK
-- ════════════════════════════════════════════════════════════════════════════

-- | Concrete effect stack for V3 TDD protocol.
-- Handlers declare constraints on this stack via Member.
-- Full interpreter composition happens in Phase 8 (Runner).
type V3Effects =
  '[ {- Session
   , Memory TDDMem
   , Memory ImplMem
   , Subgraph Spec MergeComplete
   , IO
   -}
   ]

-- Note: Effect list above is symbolic. Actual composition in
-- types-first-dev/app/RunV3.hs using withRecursiveGraph and
-- Tidepool.Session.Executor.runSessionIO for handler execution.
