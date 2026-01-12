{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | JSON Schema instances for types.
--
-- Separate module due to TH staging requirements.
module TypesFirstDev.Schema
  ( -- * Re-exports from Types
    module TypesFirstDev.Types
  ) where

import Tidepool.Schema (deriveHasJSONSchema, HasJSONSchema(..), enumSchema, objectSchema, oneOfSchema, emptySchema, SchemaType(..))

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
$(deriveHasJSONSchema ''MergeRejectionFeedback)
$(deriveHasJSONSchema ''ChildFailureFeedback)
$(deriveHasJSONSchema ''CodeReviewFeedback)
$(deriveHasJSONSchema ''ChangeEntry)
$(deriveHasJSONSchema ''NodeInfo)
$(deriveHasJSONSchema ''CoverageReport)
$(deriveHasJSONSchema ''ChildSpec)
$(deriveHasJSONSchema ''InterfaceFile)

-- ClarificationType is an enum
instance HasJSONSchema ClarificationType where
  jsonSchema = enumSchema ["SpecAmbiguity", "BlockedDependency", "MergeConflict", "InvalidScaffold"]

$(deriveHasJSONSchema ''ClarificationRequest)

-- Payload types
$(deriveHasJSONSchema ''InitWorkPayload)
$(deriveHasJSONSchema ''TestsReadyPayload)
$(deriveHasJSONSchema ''ImplResult)
$(deriveHasJSONSchema ''TDDApproval)
$(deriveHasJSONSchema ''ChildFailure)

-- MergeComplete is a sum type: MergeSuccess | MergeFailed
-- Manual instance (must be before types that reference it)
instance HasJSONSchema MergeComplete where
  jsonSchema = oneOfSchema
    [ objectSchema
        [ ("tag", emptySchema TString)
        , ("commit", emptySchema TString)
        , ("author", emptySchema TString)
        , ("impactLevel", emptySchema TString)
        , ("changes", emptySchema TArray)
        ]
        ["tag", "commit", "author", "impactLevel", "changes"]
    , objectSchema
        [ ("tag", emptySchema TString)
        , ("failure", jsonSchema @ChildFailure)
        ]
        ["tag", "failure"]
    ]

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
$(deriveHasJSONSchema ''RetryFeedback)  -- Must come before ImplInput
$(deriveHasJSONSchema ''ImplInput)
$(deriveHasJSONSchema ''MergerInput)
$(deriveHasJSONSchema ''RebaserInput)

-- Node exit types (sum types need manual handling or simpler approach)
-- For now, we derive the exit types that are records
