{-# LANGUAGE OverloadedStrings #-}

-- | Effect specifications for codegen.
--
-- NOTE: Graph specifications are now provided by the Registry in exomonad-wasm.
-- The generate-ts-package executable imports directly from the Registry.
-- This module only contains effect specs (which come from exomonad-core).
--
-- = Effect Specifications
--
-- Effect routing metadata is derived from 'ExoMonad.Effect.Metadata.allEffectMeta',
-- which is the SINGLE SOURCE OF TRUTH for effect routing decisions.
module ExoMonad.Generated.GraphSpecs
  ( -- * Effect Specifications (derived from exomonad-core)
    allEffectSpecs

    -- * Re-export types
  , GraphSpec(..)
  , EffectSpec(..)
  ) where

import ExoMonad.Generated.Codegen (GraphSpec(..), EffectSpec(..))
import ExoMonad.Effect.Metadata
  ( allEffectMeta
  , EffectMeta(..)
  , categoryToText
  , semanticsToText
  )


-- ============================================================================
-- Effect Specifications (derived from exomonad-core)
-- ============================================================================

-- | All effect specifications for routing codegen.
--
-- DERIVED from 'ExoMonad.Effect.Metadata.allEffectMeta' - the single source
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
