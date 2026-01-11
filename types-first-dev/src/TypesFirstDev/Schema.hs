{-# LANGUAGE TemplateHaskell #-}

-- | JSON Schema instances for V3 types.
--
-- Separate module due to TH staging requirements.
module TypesFirstDev.Schema
  ( -- * Re-exports from V3 Types
    module TypesFirstDev.V3.Types
  ) where

import Tidepool.Schema (deriveHasJSONSchema, HasJSONSchema(..), enumSchema)

import TypesFirstDev.V3.Types

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
$(deriveHasJSONSchema ''ImplMem)

-- Node input types
$(deriveHasJSONSchema ''ScaffoldInput)
$(deriveHasJSONSchema ''TDDWriteTestsInput)
$(deriveHasJSONSchema ''TDDReviewImplInput)
$(deriveHasJSONSchema ''ImplInput)
$(deriveHasJSONSchema ''MergerInput)
$(deriveHasJSONSchema ''RebaserInput)

-- Node exit types (sum types need manual handling or simpler approach)
-- For now, we derive the exit types that are records
