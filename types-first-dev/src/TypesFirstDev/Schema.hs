{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | JSON Schema instances for types-first-dev types.
--
-- Separate module due to TH staging requirements.
module TypesFirstDev.Schema
  ( -- * Re-exports from Types
    module TypesFirstDev.Types
  ) where

import Tidepool.Schema (deriveHasJSONSchema, HasJSONSchema(..), enumSchema)

import TypesFirstDev.Types


-- | Manual HasJSONSchema for ProjectType (string enum).
-- Uses enumSchema helper for string enums.
instance HasJSONSchema ProjectType where
  jsonSchema = enumSchema ["PureLibrary", "ServantServer", "CLIApp"]

-- | Manual HasJSONSchema for ResumeStrategy (string enum).
-- ResumeStrategy comes from tidepool-claude-code-executor.
instance HasJSONSchema ResumeStrategy where
  jsonSchema = enumSchema ["NoResume", "AlwaysResume", "SmartResume"]


-- Derive HasJSONSchema instances for record types used in graph execution.
-- Note: Sum types like TestResult can't use deriveHasJSONSchema.
-- Order matters due to TH staging: types must be derived before types that reference them.

-- Basic types
$(deriveHasJSONSchema ''FunctionSig)
$(deriveHasJSONSchema ''TestPriority)

-- Per-function rubric types (LLM sensor output - semantic only)
-- BoundaryNote must come before FunctionRubric which uses it
$(deriveHasJSONSchema ''BoundaryNote)
$(deriveHasJSONSchema ''FunctionRubric)
$(deriveHasJSONSchema ''TestFunctionRubric)
$(deriveHasJSONSchema ''Blocker)

-- v3 semantic description types
$(deriveHasJSONSchema ''FunctionExample)
$(deriveHasJSONSchema ''FunctionSemantics)
$(deriveHasJSONSchema ''StubsOutput)

-- Agent result types (use rubric types)
$(deriveHasJSONSchema ''TestsResult)
$(deriveHasJSONSchema ''ImplResult)
$(deriveHasJSONSchema ''FixResult)

-- Entry and internal types
$(deriveHasJSONSchema ''StackSpec)
$(deriveHasJSONSchema ''TypeDefinitions)
$(deriveHasJSONSchema ''ForkInput)
-- ParallelResults not derived - it's internal and contains WorktreePath
-- which has no HasJSONSchema instance
