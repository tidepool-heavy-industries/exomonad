{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Roundtrip serialization testing for WASM ↔ TypeScript boundary.
--
-- These exports allow TypeScript property tests to verify that
-- JSON serialization matches between both sides of the WASM boundary.
--
-- Each function deserializes JSON input, then reserializes it.
-- TypeScript can then compare input === output to verify agreement.
--
-- Return format: @{ok: true, value: ...}@ or @{ok: false, error: "..."}@
module Tidepool.Wasm.Roundtrip
  ( -- * Roundtrip FFI Exports
    roundtripSerializableEffect
  , roundtripEffectResult
  , roundtripExecutionPhase
  , roundtripGraphState
  , roundtripStepOutput
    -- * Pure implementation (for native testing)
  , roundtripImpl
  ) where

import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict, encode, object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import Tidepool.Wasm.WireTypes
  ( SerializableEffect
  , EffectResult
  , ExecutionPhase
  , GraphState
  , StepOutput
  )

#if defined(wasm32_HOST_ARCH)
import GHC.Wasm.Prim (JSString, fromJSString, toJSString)
#endif


-- ════════════════════════════════════════════════════════════════════════════
-- FFI EXPORTS (WASM target)
-- ════════════════════════════════════════════════════════════════════════════

#if defined(wasm32_HOST_ARCH)

foreign export javascript "roundtripSerializableEffect"
  roundtripSerializableEffect :: JSString -> IO JSString

foreign export javascript "roundtripEffectResult"
  roundtripEffectResult :: JSString -> IO JSString

foreign export javascript "roundtripExecutionPhase"
  roundtripExecutionPhase :: JSString -> IO JSString

foreign export javascript "roundtripGraphState"
  roundtripGraphState :: JSString -> IO JSString

foreign export javascript "roundtripStepOutput"
  roundtripStepOutput :: JSString -> IO JSString

#endif


-- ════════════════════════════════════════════════════════════════════════════
-- IMPLEMENTATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Generic roundtrip: decode JSON → encode back to JSON.
--
-- Returns @{ok: true, value: <reserialized>}@ on success,
-- or @{ok: false, error: "<message>"}@ on parse failure.
roundtripImpl :: forall a. (FromJSON a, ToJSON a) => Text -> Text
roundtripImpl input =
  case eitherDecodeStrict (encodeUtf8 input) of
    Left err -> TL.toStrict $ TLE.decodeUtf8 $ encode $
      object ["ok" .= False, "error" .= T.pack err]
    Right (val :: a) -> TL.toStrict $ TLE.decodeUtf8 $ encode $
      object ["ok" .= True, "value" .= val]


-- ════════════════════════════════════════════════════════════════════════════
-- TYPE-SPECIFIC EXPORTS
-- ════════════════════════════════════════════════════════════════════════════

-- | Roundtrip SerializableEffect through Haskell's Aeson.
#if defined(wasm32_HOST_ARCH)
roundtripSerializableEffect :: JSString -> IO JSString
roundtripSerializableEffect input =
  pure $ toJSString $ roundtripImpl @SerializableEffect $ fromJSString input
#else
roundtripSerializableEffect :: Text -> IO Text
roundtripSerializableEffect = pure . roundtripImpl @SerializableEffect
#endif


-- | Roundtrip EffectResult through Haskell's Aeson.
#if defined(wasm32_HOST_ARCH)
roundtripEffectResult :: JSString -> IO JSString
roundtripEffectResult input =
  pure $ toJSString $ roundtripImpl @EffectResult $ fromJSString input
#else
roundtripEffectResult :: Text -> IO Text
roundtripEffectResult = pure . roundtripImpl @EffectResult
#endif


-- | Roundtrip ExecutionPhase through Haskell's Aeson.
#if defined(wasm32_HOST_ARCH)
roundtripExecutionPhase :: JSString -> IO JSString
roundtripExecutionPhase input =
  pure $ toJSString $ roundtripImpl @ExecutionPhase $ fromJSString input
#else
roundtripExecutionPhase :: Text -> IO Text
roundtripExecutionPhase = pure . roundtripImpl @ExecutionPhase
#endif


-- | Roundtrip GraphState through Haskell's Aeson.
#if defined(wasm32_HOST_ARCH)
roundtripGraphState :: JSString -> IO JSString
roundtripGraphState input =
  pure $ toJSString $ roundtripImpl @GraphState $ fromJSString input
#else
roundtripGraphState :: Text -> IO Text
roundtripGraphState = pure . roundtripImpl @GraphState
#endif


-- | Roundtrip StepOutput through Haskell's Aeson.
#if defined(wasm32_HOST_ARCH)
roundtripStepOutput :: JSString -> IO JSString
roundtripStepOutput input =
  pure $ toJSString $ roundtripImpl @StepOutput $ fromJSString input
#else
roundtripStepOutput :: Text -> IO Text
roundtripStepOutput = pure . roundtripImpl @StepOutput
#endif
