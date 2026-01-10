{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | HasJSONSchema instances for Hybrid types with compile-time doc validation.
--
-- == Why a Separate Module?
--
-- TH staging: 'deriveHasJSONSchema' uses 'getDoc' to read Haddock comments,
-- which requires the type to be in an already-compiled module. Types defined
-- in the same module as the TH splice haven't been compiled yet when the
-- splice runs.
--
-- == Compile-Time Doc Validation
--
-- 'deriveHasJSONSchema' FAILS at compile time if any record field is missing
-- a Haddock comment (@-- ^@ format). This ensures all LLM-facing schemas
-- have descriptions.
--
-- == Usage
--
-- Import this module instead of Types.Hybrid when you need schemas with
-- descriptions (e.g., for Claude Code's --json-schema flag):
--
-- @
-- import TypesFirstDev.Schema.Hybrid (TypesAgentOutput)
-- import Tidepool.Schema (jsonSchema, schemaToValue)
--
-- schemaJson = schemaToValue (jsonSchema @TypesAgentOutput)
-- @
module TypesFirstDev.Schema.Hybrid
  ( -- * Re-export types with HasJSONSchema instances
    module TypesFirstDev.Types.Hybrid
  ) where

import Tidepool.Schema (deriveHasJSONSchema, HasJSONSchema(..), enumSchema)

import TypesFirstDev.Types.Hybrid


-- ════════════════════════════════════════════════════════════════════════════
-- MANUAL INSTANCES (types TH can't handle)
-- ════════════════════════════════════════════════════════════════════════════

-- FixType has OtherFix Text (not nullary), so manual instance needed
instance HasJSONSchema FixType where
  jsonSchema = enumSchema
    [ "EdgeCaseFix"
    , "LogicFix"
    , "TypeFix"
    , "BoundaryFix"
    , "InitializationFix"
    , "OtherFix"
    ]


-- ════════════════════════════════════════════════════════════════════════════
-- HASJSONSCHEMA INSTANCES (TH-derived with doc validation)
-- ════════════════════════════════════════════════════════════════════════════

-- Order matters: derive base types before types that reference them.

-- FunctionSpec is used by TypesAgentOutput
$(deriveHasJSONSchema ''FunctionSpec)

-- Main agent outputs
$(deriveHasJSONSchema ''TypesAgentOutput)
$(deriveHasJSONSchema ''TestsAgentOutput)
$(deriveHasJSONSchema ''ImplAgentOutput)

-- Fix types
$(deriveHasJSONSchema ''FixApplied)
$(deriveHasJSONSchema ''FixAgentOutput)
