{-# LANGUAGE OverloadedStrings #-}

-- | Shared graph specifications for codegen and testing.
--
-- This module is the SINGLE SOURCE OF TRUTH for graph metadata used by:
--
-- 1. @generate-ts-package@ - To generate TypeScript types and dispatcher
-- 2. @CodegenSyncSpec@ test - To verify FFI output matches these specs
--
-- = Adding a New Graph
--
-- When adding a new graph to Ffi.hs, you MUST also add it here.
-- The CodegenSyncSpec test will fail if they're out of sync.
--
-- 1. Add the GraphFFISpec to Ffi.hs TH splice
-- 2. Add the corresponding GraphSpec here
-- 3. Run tests to verify sync: @cabal test tidepool-wasm-tests@
--
-- = Effect Specifications
--
-- Effect routing metadata is derived from 'Tidepool.Effect.Metadata.allEffectMeta',
-- which is the SINGLE SOURCE OF TRUTH for effect routing decisions.
-- Adding a new effect only requires updating that module.
module Tidepool.Generated.GraphSpecs
  ( -- * Graph Specifications
    allGraphSpecs
  , testGraphSpec
  , exampleGraphSpec
  , habiticaGraphSpec

    -- * Effect Specifications (derived from tidepool-core)
  , allEffectSpecs

    -- * Re-export types
  , GraphSpec(..)
  , EffectSpec(..)
  ) where

import Tidepool.Generated.Codegen (GraphSpec(..), EffectSpec(..))
import Tidepool.Effect.Metadata
  ( allEffectMeta
  , EffectMeta(..)
  , categoryToText
  , semanticsToText
  )


-- | All graph specifications. Keep in sync with Ffi.hs!
allGraphSpecs :: [GraphSpec]
allGraphSpecs =
  [ testGraphSpec
  , exampleGraphSpec
  , habiticaGraphSpec
  ]


-- | TestGraph specification.
-- Must match the GraphFFISpec in Ffi.hs.
testGraphSpec :: GraphSpec
testGraphSpec = GraphSpec
  { gsId = "test"
  , gsName = "TestGraph"
  , gsNodes = ["entry", "compute", "exit"]
  , gsEdges = [("entry", "compute"), ("compute", "exit")]
  }


-- | ExampleGraph specification.
-- Must match the GraphFFISpec in Ffi.hs.
exampleGraphSpec :: GraphSpec
exampleGraphSpec = GraphSpec
  { gsId = "example"
  , gsName = "ExampleGraph"
  , gsNodes = ["entry", "classify", "handleGreeting", "handleQuestion", "handleStatement", "exit"]
  , gsEdges =
      [ ("entry", "classify")
      , ("classify", "handleGreeting")
      , ("classify", "handleQuestion")
      , ("classify", "handleStatement")
      , ("handleGreeting", "exit")
      , ("handleQuestion", "exit")
      , ("handleStatement", "exit")
      ]
  }


-- | HabiticaRoutingGraph specification.
-- Must match the GraphFFISpec in Ffi.hs.
habiticaGraphSpec :: GraphSpec
habiticaGraphSpec = GraphSpec
  { gsId = "habitica"
  , gsName = "HabiticaRoutingGraph"
  , gsNodes = ["entry", "extractTask", "fetchExisting", "matchTask", "suggestAction", "confirmWithUser", "executeAction", "exit"]
  , gsEdges =
      [ ("entry", "extractTask")
      , ("extractTask", "fetchExisting")
      , ("fetchExisting", "matchTask")
      , ("matchTask", "suggestAction")
      , ("suggestAction", "confirmWithUser")
      , ("confirmWithUser", "executeAction")
      , ("confirmWithUser", "suggestAction")
      , ("confirmWithUser", "exit")
      , ("executeAction", "exit")
      ]
  }


-- ============================================================================
-- Effect Specifications (derived from tidepool-core)
-- ============================================================================

-- | All effect specifications for routing codegen.
--
-- DERIVED from 'Tidepool.Effect.Metadata.allEffectMeta' - the single source
-- of truth for effect routing decisions. Adding a new effect only requires
-- updating that module; both codegen and runtime use the same data.
allEffectSpecs :: [EffectSpec]
allEffectSpecs = map toEffectSpec allEffectMeta
  where
    toEffectSpec meta = EffectSpec
      { esType      = meta.emTypeName
      , esCategory  = categoryToText meta.emCategory
      , esSemantics = semanticsToText meta.emSemantics
      }
