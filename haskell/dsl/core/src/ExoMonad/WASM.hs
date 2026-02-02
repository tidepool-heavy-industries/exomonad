-- |
-- Module: ExoMonad.WASM
-- Description: Minimal re-exports for WASM builds
--
-- This module provides a minimal surface area for WASM builds, excluding:
-- - Documentation generators (Mermaid, Docs)
-- - Example graphs (Graph.Example)
-- - Dev-only types (Anthropic.Types, Question, Tool, Schema, Delta)
--
-- Expected savings: ~500 KB in WASM binary size.
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