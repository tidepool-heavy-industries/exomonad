{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Native roundtrip tests for the Roundtrip module.
--
-- These tests verify that the roundtrip functions work correctly
-- before we involve WASM. Uses the same Arbitrary instances as
-- ProtocolPropertySpec.hs.
module RoundtripSpec (spec) where

import Data.Aeson (Value (..), decode, encode, toJSON)
import Data.Aeson.KeyMap qualified as KM
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import ExoMonad.Wasm.Roundtrip
import ExoMonad.Wasm.WireTypes
import ProtocolPropertySpec ()
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

-- Import orphan Arbitrary instances

spec :: Spec
spec = do
  roundtripFunctionSpec
  errorHandlingSpec

-- | Helper to check if result is ok=true
isOk :: Value -> Bool
isOk (Object obj) = case KM.lookup "ok" obj of
  Just (Bool True) -> True
  _ -> False
isOk _ = False

-- | Helper to get the "value" field from a result
getValue :: Value -> Maybe Value
getValue (Object obj) = KM.lookup "value" obj
getValue _ = Nothing

-- | Helper to get the "error" field from a result
getError :: Value -> Maybe Value
getError (Object obj) = KM.lookup "error" obj
getError _ = Nothing

roundtripFunctionSpec :: Spec
roundtripFunctionSpec = describe "Roundtrip functions" $ do
  describe "roundtripSerializableEffect" $ do
    prop "roundtrips correctly via native interface" $ \(effect :: SerializableEffect) -> do
      let input = TL.toStrict $ TLE.decodeUtf8 $ encode effect
      result <- roundtripSerializableEffect input
      case decode (TLE.encodeUtf8 $ TL.fromStrict result) of
        Just obj -> do
          obj `shouldSatisfy` isOk
          getValue obj `shouldBe` Just (toJSON effect)
        Nothing -> expectationFailure "Failed to parse roundtrip result as JSON"

  describe "roundtripEffectResult" $ do
    prop "roundtrips correctly via native interface" $ \(effectResult :: EffectResult) -> do
      let input = TL.toStrict $ TLE.decodeUtf8 $ encode effectResult
      result <- roundtripEffectResult input
      case decode (TLE.encodeUtf8 $ TL.fromStrict result) of
        Just obj -> do
          obj `shouldSatisfy` isOk
          getValue obj `shouldBe` Just (toJSON effectResult)
        Nothing -> expectationFailure "Failed to parse roundtrip result as JSON"

  describe "roundtripExecutionPhase" $ do
    prop "roundtrips correctly via native interface" $ \(phase :: ExecutionPhase) -> do
      let input = TL.toStrict $ TLE.decodeUtf8 $ encode phase
      result <- roundtripExecutionPhase input
      case decode (TLE.encodeUtf8 $ TL.fromStrict result) of
        Just obj -> do
          obj `shouldSatisfy` isOk
          getValue obj `shouldBe` Just (toJSON phase)
        Nothing -> expectationFailure "Failed to parse roundtrip result as JSON"

  describe "roundtripGraphState" $ do
    prop "roundtrips correctly via native interface" $ \(state :: GraphState) -> do
      let input = TL.toStrict $ TLE.decodeUtf8 $ encode state
      result <- roundtripGraphState input
      case decode (TLE.encodeUtf8 $ TL.fromStrict result) of
        Just obj -> do
          obj `shouldSatisfy` isOk
          getValue obj `shouldBe` Just (toJSON state)
        Nothing -> expectationFailure "Failed to parse roundtrip result as JSON"

  describe "roundtripStepOutput" $ do
    prop "roundtrips correctly via native interface" $ \(output :: StepOutput) -> do
      let input = TL.toStrict $ TLE.decodeUtf8 $ encode output
      result <- roundtripStepOutput input
      case decode (TLE.encodeUtf8 $ TL.fromStrict result) of
        Just obj -> do
          obj `shouldSatisfy` isOk
          getValue obj `shouldBe` Just (toJSON output)
        Nothing -> expectationFailure "Failed to parse roundtrip result as JSON"

  describe "roundtripTypeInfoWire" $ do
    prop "roundtrips correctly via native interface" $ \(ti :: TypeInfoWire) -> do
      let input = TL.toStrict $ TLE.decodeUtf8 $ encode ti
      result <- roundtripTypeInfoWire input
      case decode (TLE.encodeUtf8 $ TL.fromStrict result) of
        Just obj -> do
          obj `shouldSatisfy` isOk
          getValue obj `shouldBe` Just (toJSON ti)
        Nothing -> expectationFailure "Failed to parse roundtrip result as JSON"

  describe "roundtripGotoTargetWire" $ do
    prop "roundtrips correctly via native interface" $ \(gt :: GotoTargetWire) -> do
      let input = TL.toStrict $ TLE.decodeUtf8 $ encode gt
      result <- roundtripGotoTargetWire input
      case decode (TLE.encodeUtf8 $ TL.fromStrict result) of
        Just obj -> do
          obj `shouldSatisfy` isOk
          getValue obj `shouldBe` Just (toJSON gt)
        Nothing -> expectationFailure "Failed to parse roundtrip result as JSON"

  describe "roundtripNodeInfoWire" $ do
    prop "roundtrips correctly via native interface" $ \(ni :: NodeInfoWire) -> do
      let input = TL.toStrict $ TLE.decodeUtf8 $ encode ni
      result <- roundtripNodeInfoWire input
      case decode (TLE.encodeUtf8 $ TL.fromStrict result) of
        Just obj -> do
          obj `shouldSatisfy` isOk
          getValue obj `shouldBe` Just (toJSON ni)
        Nothing -> expectationFailure "Failed to parse roundtrip result as JSON"

  describe "roundtripEdgeInfoWire" $ do
    prop "roundtrips correctly via native interface" $ \(ei :: EdgeInfoWire) -> do
      let input = TL.toStrict $ TLE.decodeUtf8 $ encode ei
      result <- roundtripEdgeInfoWire input
      case decode (TLE.encodeUtf8 $ TL.fromStrict result) of
        Just obj -> do
          obj `shouldSatisfy` isOk
          getValue obj `shouldBe` Just (toJSON ei)
        Nothing -> expectationFailure "Failed to parse roundtrip result as JSON"

  describe "roundtripGraphInfoWire" $ do
    prop "roundtrips correctly via native interface" $ \(gi :: GraphInfoWire) -> do
      let input = TL.toStrict $ TLE.decodeUtf8 $ encode gi
      result <- roundtripGraphInfoWire input
      case decode (TLE.encodeUtf8 $ TL.fromStrict result) of
        Just obj -> do
          obj `shouldSatisfy` isOk
          getValue obj `shouldBe` Just (toJSON gi)
        Nothing -> expectationFailure "Failed to parse roundtrip result as JSON"

errorHandlingSpec :: Spec
errorHandlingSpec = describe "Error handling" $ do
  it "returns ok=false for invalid JSON" $ do
    result <- roundtripSerializableEffect "not valid json"
    case decode (TLE.encodeUtf8 $ TL.fromStrict result) of
      Just obj -> do
        obj `shouldSatisfy` (not . isOk)
        getError obj `shouldSatisfy` \case
          Just (String _) -> True
          _ -> False
      Nothing -> expectationFailure "Failed to parse error result as JSON"

  it "returns ok=false for wrong type" $ do
    -- Valid JSON but wrong type (number instead of SerializableEffect object)
    result <- roundtripSerializableEffect "42"
    case decode (TLE.encodeUtf8 $ TL.fromStrict result) of
      Just obj -> do
        obj `shouldSatisfy` (not . isOk)
        getError obj `shouldSatisfy` \case
          Just (String _) -> True
          _ -> False
      Nothing -> expectationFailure "Failed to parse error result as JSON"

  it "returns ok=false for missing required fields" $ do
    -- Object but missing 'type' field
    result <- roundtripSerializableEffect "{\"foo\": \"bar\"}"
    case decode (TLE.encodeUtf8 $ TL.fromStrict result) of
      Just obj -> do
        obj `shouldSatisfy` (not . isOk)
        getError obj `shouldSatisfy` \case
          Just (String _) -> True
          _ -> False
      Nothing -> expectationFailure "Failed to parse error result as JSON"
