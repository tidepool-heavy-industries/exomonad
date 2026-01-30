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
module ExoMonad.Wasm.Roundtrip
  ( -- * Roundtrip FFI Exports
    roundtripSerializableEffect,
    roundtripEffectResult,
    roundtripExecutionPhase,
    roundtripGraphState,
    roundtripStepOutput,

    -- ** Graph Info Types
    roundtripTypeInfoWire,
    roundtripGotoTargetWire,
    roundtripNodeInfoWire,
    roundtripEdgeInfoWire,
    roundtripGraphInfoWire,

    -- * Pure implementation (for native testing)
    roundtripImpl,
  )
where

import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict, encode, object, (.=))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import ExoMonad.Wasm.WireTypes
  ( EdgeInfoWire,
    EffectResult,
    ExecutionPhase,
    GotoTargetWire,
    GraphInfoWire,
    GraphState,
    NodeInfoWire,
    SerializableEffect,
    StepOutput,
    TypeInfoWire,
  )

#if defined(wasm32_HOST_ARCH)
import GHC.Wasm.Prim (JSString(..), fromJSString, toJSString)
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

foreign export javascript "roundtripTypeInfoWire"
  roundtripTypeInfoWire :: JSString -> IO JSString

foreign export javascript "roundtripGotoTargetWire"
  roundtripGotoTargetWire :: JSString -> IO JSString

foreign export javascript "roundtripNodeInfoWire"
  roundtripNodeInfoWire :: JSString -> IO JSString

foreign export javascript "roundtripEdgeInfoWire"
  roundtripEdgeInfoWire :: JSString -> IO JSString

foreign export javascript "roundtripGraphInfoWire"
  roundtripGraphInfoWire :: JSString -> IO JSString

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
    Left err ->
      TL.toStrict $
        TLE.decodeUtf8 $
          encode $
            object ["ok" .= False, "error" .= T.pack err]
    Right (val :: a) ->
      TL.toStrict $
        TLE.decodeUtf8 $
          encode $
            object ["ok" .= True, "value" .= val]

-- ════════════════════════════════════════════════════════════════════════════
-- TYPE-SPECIFIC EXPORTS
-- ════════════════════════════════════════════════════════════════════════════

-- | Roundtrip SerializableEffect through Haskell's Aeson.
#if defined(wasm32_HOST_ARCH)
roundtripSerializableEffect :: JSString -> IO JSString
roundtripSerializableEffect input =
  pure $ toJSString $ T.unpack $ roundtripImpl @SerializableEffect $ T.pack $ fromJSString input
#else
roundtripSerializableEffect :: Text -> IO Text
roundtripSerializableEffect = pure . roundtripImpl @SerializableEffect
#endif

-- | Roundtrip EffectResult through Haskell's Aeson.
#if defined(wasm32_HOST_ARCH)
roundtripEffectResult :: JSString -> IO JSString
roundtripEffectResult input =
  pure $ toJSString $ T.unpack $ roundtripImpl @EffectResult $ T.pack $ fromJSString input
#else
roundtripEffectResult :: Text -> IO Text
roundtripEffectResult = pure . roundtripImpl @EffectResult
#endif

-- | Roundtrip ExecutionPhase through Haskell's Aeson.
#if defined(wasm32_HOST_ARCH)
roundtripExecutionPhase :: JSString -> IO JSString
roundtripExecutionPhase input =
  pure $ toJSString $ T.unpack $ roundtripImpl @ExecutionPhase $ T.pack $ fromJSString input
#else
roundtripExecutionPhase :: Text -> IO Text
roundtripExecutionPhase = pure . roundtripImpl @ExecutionPhase
#endif

-- | Roundtrip GraphState through Haskell's Aeson.
#if defined(wasm32_HOST_ARCH)
roundtripGraphState :: JSString -> IO JSString
roundtripGraphState input =
  pure $ toJSString $ T.unpack $ roundtripImpl @GraphState $ T.pack $ fromJSString input
#else
roundtripGraphState :: Text -> IO Text
roundtripGraphState = pure . roundtripImpl @GraphState
#endif

-- | Roundtrip StepOutput through Haskell's Aeson.
#if defined(wasm32_HOST_ARCH)
roundtripStepOutput :: JSString -> IO JSString
roundtripStepOutput input =
  pure $ toJSString $ T.unpack $ roundtripImpl @StepOutput $ T.pack $ fromJSString input
#else
roundtripStepOutput :: Text -> IO Text
roundtripStepOutput = pure . roundtripImpl @StepOutput
#endif

-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH INFO TYPE EXPORTS
-- ════════════════════════════════════════════════════════════════════════════

-- | Roundtrip TypeInfoWire through Haskell's Aeson.
#if defined(wasm32_HOST_ARCH)
roundtripTypeInfoWire :: JSString -> IO JSString
roundtripTypeInfoWire input =
  pure $ toJSString $ T.unpack $ roundtripImpl @TypeInfoWire $ T.pack $ fromJSString input
#else
roundtripTypeInfoWire :: Text -> IO Text
roundtripTypeInfoWire = pure . roundtripImpl @TypeInfoWire
#endif

-- | Roundtrip GotoTargetWire through Haskell's Aeson.
#if defined(wasm32_HOST_ARCH)
roundtripGotoTargetWire :: JSString -> IO JSString
roundtripGotoTargetWire input =
  pure $ toJSString $ T.unpack $ roundtripImpl @GotoTargetWire $ T.pack $ fromJSString input
#else
roundtripGotoTargetWire :: Text -> IO Text
roundtripGotoTargetWire = pure . roundtripImpl @GotoTargetWire
#endif

-- | Roundtrip NodeInfoWire through Haskell's Aeson.
#if defined(wasm32_HOST_ARCH)
roundtripNodeInfoWire :: JSString -> IO JSString
roundtripNodeInfoWire input =
  pure $ toJSString $ T.unpack $ roundtripImpl @NodeInfoWire $ T.pack $ fromJSString input
#else
roundtripNodeInfoWire :: Text -> IO Text
roundtripNodeInfoWire = pure . roundtripImpl @NodeInfoWire
#endif

-- | Roundtrip EdgeInfoWire through Haskell's Aeson.
#if defined(wasm32_HOST_ARCH)
roundtripEdgeInfoWire :: JSString -> IO JSString
roundtripEdgeInfoWire input =
  pure $ toJSString $ T.unpack $ roundtripImpl @EdgeInfoWire $ T.pack $ fromJSString input
#else
roundtripEdgeInfoWire :: Text -> IO Text
roundtripEdgeInfoWire = pure . roundtripImpl @EdgeInfoWire
#endif

-- | Roundtrip GraphInfoWire through Haskell's Aeson.
#if defined(wasm32_HOST_ARCH)
roundtripGraphInfoWire :: JSString -> IO JSString
roundtripGraphInfoWire input =
  pure $ toJSString $ T.unpack $ roundtripImpl @GraphInfoWire $ T.pack $ fromJSString input
#else
roundtripGraphInfoWire :: Text -> IO Text
roundtripGraphInfoWire = pure . roundtripImpl @GraphInfoWire
#endif
