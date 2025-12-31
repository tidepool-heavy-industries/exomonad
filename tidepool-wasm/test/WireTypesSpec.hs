{-# LANGUAGE OverloadedStrings #-}

-- | Tests for WireTypes JSON encoding/decoding.
--
-- These tests ensure that:
-- 1. Round-tripping (encode then decode) preserves values
-- 2. JSON structure matches protocol.ts expectations
module WireTypesSpec (spec) where

import Test.Hspec
import Data.Aeson (encode, decode, object, (.=), Value(..))
import Data.Aeson.KeyMap qualified as KM
import Data.Maybe (fromJust)

import Tidepool.Wasm.WireTypes


spec :: Spec
spec = do
  serializableEffectSpec
  effectResultSpec
  stepOutputSpec


-- ════════════════════════════════════════════════════════════════════════════
-- SerializableEffect
-- ════════════════════════════════════════════════════════════════════════════

serializableEffectSpec :: Spec
serializableEffectSpec = describe "SerializableEffect" $ do

  it "round-trips EffLogInfo" $ do
    let effect = EffLogInfo "test message"
    decode (encode effect) `shouldBe` Just effect

  it "round-trips EffLogInfo with empty message" $ do
    let effect = EffLogInfo ""
    decode (encode effect) `shouldBe` Just effect

  it "round-trips EffLogInfo with unicode" $ do
    let effect = EffLogInfo "Hello \x1F600 world"
    decode (encode effect) `shouldBe` Just effect

  it "encodes EffLogInfo with correct JSON structure" $ do
    let effect = EffLogInfo "test"
        json = decode (encode effect) :: Maybe Value
    case json of
      Just (Object obj) -> do
        KM.lookup "type" obj `shouldBe` Just (String "LogInfo")
        KM.lookup "eff_message" obj `shouldBe` Just (String "test")
      _ -> expectationFailure "Expected JSON object"


-- ════════════════════════════════════════════════════════════════════════════
-- EffectResult
-- ════════════════════════════════════════════════════════════════════════════

effectResultSpec :: Spec
effectResultSpec = describe "EffectResult" $ do

  it "round-trips ResSuccess with value" $ do
    let result = ResSuccess (Just (object ["foo" .= ("bar" :: String)]))
    decode (encode result) `shouldBe` Just result

  it "round-trips ResSuccess without value" $ do
    let result = ResSuccess Nothing
    decode (encode result) `shouldBe` Just result

  it "round-trips ResError" $ do
    let result = ResError "something went wrong"
    decode (encode result) `shouldBe` Just result

  it "encodes ResSuccess with correct JSON structure" $ do
    let result = ResSuccess (Just (Number 42))
        json = decode (encode result) :: Maybe Value
    case json of
      Just (Object obj) -> do
        KM.lookup "type" obj `shouldBe` Just (String "success")
        KM.lookup "value" obj `shouldBe` Just (Number 42)
      _ -> expectationFailure "Expected JSON object"

  it "encodes ResError with correct JSON structure" $ do
    let result = ResError "oops"
        json = decode (encode result) :: Maybe Value
    case json of
      Just (Object obj) -> do
        KM.lookup "type" obj `shouldBe` Just (String "error")
        KM.lookup "message" obj `shouldBe` Just (String "oops")
      _ -> expectationFailure "Expected JSON object"


-- ════════════════════════════════════════════════════════════════════════════
-- StepOutput
-- ════════════════════════════════════════════════════════════════════════════

stepOutputSpec :: Spec
stepOutputSpec = describe "StepOutput" $ do

  it "round-trips StepOutput with effect, not done" $ do
    let output = StepOutput
          { soEffect = Just (EffLogInfo "computing")
          , soDone = False
          , soStepResult = Nothing
          }
    decode (encode output) `shouldBe` Just output

  it "round-trips StepOutput done with result" $ do
    let output = StepOutput
          { soEffect = Nothing
          , soDone = True
          , soStepResult = Just (Number 42)
          }
    decode (encode output) `shouldBe` Just output

  it "round-trips StepOutput with all fields populated" $ do
    let output = StepOutput
          { soEffect = Just (EffLogInfo "msg")
          , soDone = True
          , soStepResult = Just (String "result")
          }
    decode (encode output) `shouldBe` Just output

  it "encodes StepOutput with correct JSON structure" $ do
    let output = StepOutput
          { soEffect = Just (EffLogInfo "test")
          , soDone = False
          , soStepResult = Nothing
          }
        json = decode (encode output) :: Maybe Value
    case json of
      Just (Object obj) -> do
        KM.lookup "done" obj `shouldBe` Just (Bool False)
        case KM.lookup "effect" obj of
          Just (Object effObj) ->
            KM.lookup "type" effObj `shouldBe` Just (String "LogInfo")
          _ -> expectationFailure "Expected effect object"
      _ -> expectationFailure "Expected JSON object"
