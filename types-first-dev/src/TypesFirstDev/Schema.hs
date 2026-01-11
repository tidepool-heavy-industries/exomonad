{-# LANGUAGE TemplateHaskell #-}

-- | JSON Schema instances for types.
--
-- Separate module due to TH staging requirements.
module TypesFirstDev.Schema
  ( -- * Re-exports from Types
    module TypesFirstDev.Types
  ) where

import Tidepool.Schema (deriveHasJSONSchema, HasJSONSchema(..), enumSchema)

import TypesFirstDev.Types

-- Core types
$(deriveHasJSONSchema ''Criterion)
$(deriveHasJSONSchema ''Constraints)
$(deriveHasJSONSchema ''ParentContext)
$(deriveHasJSONSchema ''Spec)

-- Enum types need manual instances (must come before types that use them)
instance HasJSONSchema ImpactLevel where
  jsonSchema = enumSchema ["Trivial", "Additive", "Breaking"]

instance HasJSONSchema ChangeType where
  jsonSchema = enumSchema ["SignatureChange", "NewExport", "RemovedExport", "BehaviorChange"]

-- Shared types
$(deriveHasJSONSchema ''PlannedTest)
$(deriveHasJSONSchema ''Critique)
$(deriveHasJSONSchema ''ChangeEntry)
$(deriveHasJSONSchema ''NodeInfo)
$(deriveHasJSONSchema ''CoverageReport)
$(deriveHasJSONSchema ''ChildSpec)
$(deriveHasJSONSchema ''InterfaceFile)

-- Payload types
$(deriveHasJSONSchema ''InitWorkPayload)
$(deriveHasJSONSchema ''TestsReadyPayload)
$(deriveHasJSONSchema ''ImplResult)
$(deriveHasJSONSchema ''TDDApproval)
$(deriveHasJSONSchema ''MergeComplete)
$(deriveHasJSONSchema ''MergeEvent)
$(deriveHasJSONSchema ''Adaptation)

-- Memory types
$(deriveHasJSONSchema ''AttemptRecord)
$(deriveHasJSONSchema ''TDDMem)
-- ImplMem contains SessionId which has no HasJSONSchema instance
-- It's node-private memory, so not part of public schema

-- Node input types
$(deriveHasJSONSchema ''ScaffoldInput)
$(deriveHasJSONSchema ''TDDWriteTestsInput)
$(deriveHasJSONSchema ''TDDReviewImplInput)
$(deriveHasJSONSchema ''ImplInput)
$(deriveHasJSONSchema ''MergerInput)
$(deriveHasJSONSchema ''RebaserInput)

-- Node exit types (sum types need manual handling or simpler approach)
-- For now, we derive the exit types that are records
