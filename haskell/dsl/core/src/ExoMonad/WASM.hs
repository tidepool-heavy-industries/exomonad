-- |
-- Module: ExoMonad.WASM
-- Description: Minimal re-exports for WASM builds
--
-- This module provides a minimal surface area for WASM builds, excluding
-- native-only modules like documentation generators and dev-only types.
--
-- Usage in WASM packages:
-- @
-- import ExoMonad.WASM
-- @
--
-- Instead of:
-- @
-- import ExoMonad  -- pulls in everything
-- @
module ExoMonad.WASM
  ( -- * Effects
    module ExoMonad.Effect.Metadata,
    module ExoMonad.Effect.Types,
  )
where

-- Effect metadata
import ExoMonad.Effect.Metadata
import ExoMonad.Effect.Types
