{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Extensible effects API for external consumers.
--
-- This module provides 'yieldEffect', the mechanism for WASM guests to invoke
-- custom effects that are handled by user-registered Rust handlers.
--
-- Uses protobuf binary encoding for the wire format. Request/response payloads
-- are encoded as protobuf bytes, wrapped in an 'EffectEnvelope'/'EffectResponse'
-- envelope (also protobuf-encoded).
--
-- = Architecture
--
-- @
-- Haskell WASM Guest
--     │
--     │ yieldEffect "git.get_branch" (GetBranchRequest ".")
--     │   encodeMessage req → ByteString (protobuf binary)
--     │   wrap in EffectEnvelope { effect_type, payload }
--     │   encodeMessage envelope → ByteString
--     ▼
-- yield_effect host function (Rust)
--     │   EffectEnvelope::decode(bytes) → effect_type + payload
--     │   dispatch by namespace
--     ▼
-- EffectRegistry
--     └── "git.*" → GitHandler
--             └── "get_branch" → decode payload, execute, encode response
--     ▼
-- EffectResponse { result: payload | error }
--     │   encodeMessage → ByteString → WASM memory
--     ▼
-- Haskell decodes EffectResponse, then decodes inner payload → typed response
-- @
module ExoMonad.Guest.Effect
  ( -- * Error Types (proto-generated)
    EffectError (..),
    EffectErrorKind (..),
  )
where

import Data.ByteString (ByteString)
import Data.Text.Lazy (Text)
import Effects.EffectError (EffectError (..), EffectErrorKind (..))
