{-# LANGUAGE TemplateHaskell #-}

-- | JSON Schema instances for types-first-dev types.
--
-- Separate module due to TH staging requirements.
module TypesFirstDev.Schema
  ( -- * Re-exports from Types
    module TypesFirstDev.Types
  ) where

import Tidepool.Schema (deriveHasJSONSchema)

import TypesFirstDev.Types


-- Derive HasJSONSchema instances for record types used in graph execution.
-- Note: Sum types like TestResult can't use deriveHasJSONSchema.
$(deriveHasJSONSchema ''FunctionSig)
$(deriveHasJSONSchema ''TestPriority)
$(deriveHasJSONSchema ''StackSpec)
$(deriveHasJSONSchema ''TypeDefinitions)
$(deriveHasJSONSchema ''TestsResult)
$(deriveHasJSONSchema ''ImplResult)
$(deriveHasJSONSchema ''ForkInput)
-- ParallelResults not derived - it's internal and contains WorktreePath
-- which has no HasJSONSchema instance
