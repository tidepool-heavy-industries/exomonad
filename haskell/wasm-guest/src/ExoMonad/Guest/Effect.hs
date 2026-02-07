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
  ( -- * Core API
    yieldEffect,
    yieldEffect_,

    -- * Error Types (proto-generated)
    EffectError (..),
    EffectErrorKind (..),
  )
where

import Control.Exception (bracket)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as TL
import Data.Word (Word64)
import Effects.EffectError (EffectError (..), EffectErrorKind (..))
import Effects.EffectError qualified as EE
import Effects.Envelope
  ( EffectEnvelope (..),
    EffectResponse (..),
    EffectResponseResult (..),
  )
import ExoMonad.PDK.Memory (alloc, findMemory, free, load, memoryLength, memoryOffset)
import Proto3.Suite.Class (Message, fromByteString, toLazyByteString)

-- ============================================================================
-- Host Function Import
-- ============================================================================

-- | Raw FFI import for yield_effect host function.
foreign import ccall "yield_effect" host_yield_effect :: Word64 -> IO Word64

-- ============================================================================
-- Public API
-- ============================================================================

-- | Yield an effect to the host for interpretation.
--
-- This is the primary mechanism for invoking custom effects from WASM.
-- The effect type must include a namespace prefix (e.g., "git.get_branch").
--
-- Request and response types must have proto3-suite 'Message' instances
-- (generated from .proto files by compile-proto-file).
--
-- = Example
--
-- @
-- import Effects.Git (GetBranchRequest(..), GetBranchResponse(..))
--
-- result <- yieldEffect "git.get_branch" (GetBranchRequest ".")
-- case result of
--   Left err -> handleError err
--   Right resp -> use (getBranchResponseBranch resp)
-- @
yieldEffect ::
  forall req resp.
  (Message req, Message resp) =>
  -- | Effect type with namespace prefix (e.g., "git.get_branch")
  Text ->
  -- | Effect-specific request payload (protobuf Message)
  req ->
  IO (Either EffectError resp)
yieldEffect effectType request = do
  -- Encode request as protobuf binary
  let reqBytes = BL.toStrict (toLazyByteString request)

  -- Wrap in EffectEnvelope (protobuf binary)
  let envelope =
        EffectEnvelope
          { effectEnvelopeEffectType = effectType,
            effectEnvelopePayload = reqBytes
          }

  -- Encode envelope as protobuf binary, allocate in WASM memory
  let envelopeBytes = BL.toStrict (toLazyByteString envelope)

  reqMem <- alloc envelopeBytes
  bracket (pure reqMem) free $ \_ -> do
    -- Call host function
    respOffset <- host_yield_effect (memoryOffset reqMem)

    -- Read response from WASM memory
    respMem <- findMemory respOffset
    let respLen = memoryLength respMem
    bracket (pure respMem) free $ \_ -> do
      respBytes <- (load respMem :: IO (Either String ByteString))

      case respBytes of
        Left loadErr ->
          pure $ Left $ mkCustomError "load_error" (TL.pack loadErr)
        Right bytes ->
          -- Decode the EffectResponse envelope (protobuf binary)
          case fromByteString bytes of
            Left parseErr ->
              pure $
                Left $
                  mkCustomError
                    "decode_error"
                    ( "effect="
                        <> effectType
                        <> " offset="
                        <> TL.pack (show respOffset)
                        <> " extism_length="
                        <> TL.pack (show respLen)
                        <> " loaded_bytes="
                        <> TL.pack (show (BS.length bytes))
                        <> " first_bytes=["
                        <> TL.pack (showHexBytes (BS.take 32 bytes))
                        <> "] error="
                        <> TL.pack (show parseErr)
                    )
            Right (EffectResponse (Just (EffectResponseResultPayload payloadBytes))) ->
              -- Decode the inner response payload
              case fromByteString payloadBytes of
                Left innerErr ->
                  pure $
                    Left $
                      mkCustomError
                        "payload_decode_error"
                        ( "effect="
                            <> effectType
                            <> " payload_bytes="
                            <> TL.pack (show (BS.length payloadBytes))
                            <> " first_bytes=["
                            <> TL.pack (showHexBytes (BS.take 32 payloadBytes))
                            <> "] error="
                            <> TL.pack (show innerErr)
                        )
                Right resp ->
                  pure $ Right resp
            Right (EffectResponse (Just (EffectResponseResultError effErr))) ->
              pure $ Left effErr
            Right (EffectResponse Nothing) ->
              pure $ Left $ mkCustomError "empty_response" "EffectResponse had no result"

-- | Yield an effect that returns no meaningful response.
--
-- Use this for fire-and-forget effects where you only care about success/failure.
yieldEffect_ ::
  forall req.
  (Message req) =>
  -- | Effect type with namespace prefix
  Text ->
  -- | Effect-specific request payload
  req ->
  IO (Either EffectError ())
yieldEffect_ effectType request = do
  -- Use EffectEnvelope as a dummy response type (we discard the payload)
  result <- yieldEffect @req @EffectEnvelope effectType request
  pure $ () <$ result

-- ============================================================================
-- Helpers
-- ============================================================================

-- | Create a custom error with code and message.
mkCustomError :: Text -> Text -> EffectError
mkCustomError code msg =
  EffectError
    { effectErrorKind =
        Just $
          EE.EffectErrorKindCustom $
            EE.Custom
              { EE.customCode = code,
                EE.customMessage = msg,
                EE.customData = BS.empty
              }
    }

-- | Format bytes as hex string for diagnostics (e.g., "0a 05 68 65 6c").
showHexBytes :: ByteString -> String
showHexBytes bs = unwords [showHex2 b | b <- BS.unpack bs]
  where
    showHex2 b =
      let (hi, lo) = b `divMod` 16
       in [hexDigit hi, hexDigit lo]
    hexDigit n
      | n < 10 = toEnum (fromEnum '0' + fromIntegral n)
      | otherwise = toEnum (fromEnum 'a' + fromIntegral n - 10)
