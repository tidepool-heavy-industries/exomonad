{-# LANGUAGE OverloadedStrings #-}

-- | Protocol conformance tests ensuring Haskell WireTypes match
-- TypeScript protocol.ts EXACTLY.
--
-- These are golden tests that verify the JSON shape produced by Haskell
-- matches what TypeScript expects. Type mismatches will cause runtime failures.
--
-- Source of truth: deploy/src/protocol.ts
module ProtocolConformanceSpec (spec) where

import Test.Hspec
import Data.Aeson (encode, decode, object, (.=), Value(..))
import Data.Aeson.KeyMap qualified as KM

import Tidepool.Wasm.WireTypes


spec :: Spec
spec = do
  serializableEffectConformanceSpec
  effectResultConformanceSpec
  executionPhaseConformanceSpec
  graphStateConformanceSpec
  stepOutputConformanceSpec


-- ════════════════════════════════════════════════════════════════════════════
-- SerializableEffect conformance with protocol.ts
-- ════════════════════════════════════════════════════════════════════════════

serializableEffectConformanceSpec :: Spec
serializableEffectConformanceSpec = describe "SerializableEffect matches protocol.ts" $ do

  describe "LlmCompleteEffect" $ do
    it "encodes with correct field names: type, eff_node, eff_system_prompt, eff_user_content, eff_schema" $ do
      let effect = EffLlmComplete
            { effNode = "classify"
            , effSystemPrompt = "You are a classifier."
            , effUserContent = "Classify this text."
            , effSchema = Just (object ["type" .= ("string" :: String)])
            }
          json = decode (encode effect) :: Maybe Value
      case json of
        Just (Object obj) -> do
          -- Verify all field names match protocol.ts LlmCompleteEffect
          KM.lookup "type" obj `shouldBe` Just (String "LlmComplete")
          KM.lookup "eff_node" obj `shouldBe` Just (String "classify")
          KM.lookup "eff_system_prompt" obj `shouldBe` Just (String "You are a classifier.")
          KM.lookup "eff_user_content" obj `shouldBe` Just (String "Classify this text.")
          case KM.lookup "eff_schema" obj of
            Just (Object schemaObj) ->
              KM.lookup "type" schemaObj `shouldBe` Just (String "string")
            _ -> expectationFailure "Expected eff_schema to be an object"
        _ -> expectationFailure "Expected JSON object"

    it "omits eff_schema field when Nothing" $ do
      let effect = EffLlmComplete
            { effNode = "node"
            , effSystemPrompt = "sys"
            , effUserContent = "user"
            , effSchema = Nothing
            }
          json = decode (encode effect) :: Maybe Value
      case json of
        Just (Object obj) -> do
          KM.lookup "eff_schema" obj `shouldBe` Nothing
        _ -> expectationFailure "Expected JSON object"

  describe "LogInfoEffect" $ do
    it "encodes with correct field names: type, eff_message" $ do
      let effect = EffLogInfo "Processing request..."
          json = decode (encode effect) :: Maybe Value
      case json of
        Just (Object obj) -> do
          KM.lookup "type" obj `shouldBe` Just (String "LogInfo")
          KM.lookup "eff_message" obj `shouldBe` Just (String "Processing request...")
        _ -> expectationFailure "Expected JSON object"

  describe "LogErrorEffect" $ do
    it "encodes with correct field names: type, eff_message" $ do
      let effect = EffLogError "Connection failed"
          json = decode (encode effect) :: Maybe Value
      case json of
        Just (Object obj) -> do
          KM.lookup "type" obj `shouldBe` Just (String "LogError")
          KM.lookup "eff_message" obj `shouldBe` Just (String "Connection failed")
        _ -> expectationFailure "Expected JSON object"


-- ════════════════════════════════════════════════════════════════════════════
-- EffectResult conformance with protocol.ts
-- ════════════════════════════════════════════════════════════════════════════

effectResultConformanceSpec :: Spec
effectResultConformanceSpec = describe "EffectResult matches protocol.ts" $ do

  describe "success" $ do
    it "encodes as {type: \"success\", value: ...}" $ do
      let result = ResSuccess (Just (Number 42))
          json = decode (encode result) :: Maybe Value
      case json of
        Just (Object obj) -> do
          KM.lookup "type" obj `shouldBe` Just (String "success")
          KM.lookup "value" obj `shouldBe` Just (Number 42)
        _ -> expectationFailure "Expected JSON object"

    it "omits value field when Nothing" $ do
      let result = ResSuccess Nothing
          json = decode (encode result) :: Maybe Value
      case json of
        Just (Object obj) -> do
          KM.lookup "type" obj `shouldBe` Just (String "success")
          KM.lookup "value" obj `shouldBe` Nothing
        _ -> expectationFailure "Expected JSON object"

  describe "error" $ do
    it "encodes as {type: \"error\", message: ...}" $ do
      let result = ResError "Something went wrong"
          json = decode (encode result) :: Maybe Value
      case json of
        Just (Object obj) -> do
          KM.lookup "type" obj `shouldBe` Just (String "error")
          KM.lookup "message" obj `shouldBe` Just (String "Something went wrong")
        _ -> expectationFailure "Expected JSON object"


-- ════════════════════════════════════════════════════════════════════════════
-- ExecutionPhase conformance with protocol.ts
-- ════════════════════════════════════════════════════════════════════════════

executionPhaseConformanceSpec :: Spec
executionPhaseConformanceSpec = describe "ExecutionPhase matches protocol.ts" $ do

  it "encodes idle as {type: \"idle\"}" $ do
    let phase = PhaseIdle
        json = decode (encode phase) :: Maybe Value
    case json of
      Just (Object obj) -> do
        KM.lookup "type" obj `shouldBe` Just (String "idle")
        -- Should have NO other fields
        KM.size obj `shouldBe` 1
      _ -> expectationFailure "Expected JSON object"

  it "encodes in_node as {type: \"in_node\", nodeName: ...}" $ do
    let phase = PhaseInNode "classify"
        json = decode (encode phase) :: Maybe Value
    case json of
      Just (Object obj) -> do
        KM.lookup "type" obj `shouldBe` Just (String "in_node")
        KM.lookup "nodeName" obj `shouldBe` Just (String "classify")
      _ -> expectationFailure "Expected JSON object"

  it "encodes transitioning as {type: \"transitioning\", fromNode: ..., toNode: ...}" $ do
    let phase = PhaseTransitioning "classify" "respond"
        json = decode (encode phase) :: Maybe Value
    case json of
      Just (Object obj) -> do
        KM.lookup "type" obj `shouldBe` Just (String "transitioning")
        KM.lookup "fromNode" obj `shouldBe` Just (String "classify")
        KM.lookup "toNode" obj `shouldBe` Just (String "respond")
      _ -> expectationFailure "Expected JSON object"

  it "encodes completed as {type: \"completed\", result: ...}" $ do
    let phase = PhaseCompleted (object ["answer" .= ("42" :: String)])
        json = decode (encode phase) :: Maybe Value
    case json of
      Just (Object obj) -> do
        KM.lookup "type" obj `shouldBe` Just (String "completed")
        case KM.lookup "result" obj of
          Just (Object resultObj) ->
            KM.lookup "answer" resultObj `shouldBe` Just (String "42")
          _ -> expectationFailure "Expected result object"
      _ -> expectationFailure "Expected JSON object"

  it "encodes failed as {type: \"failed\", error: ...}" $ do
    let phase = PhaseFailed "Node execution timeout"
        json = decode (encode phase) :: Maybe Value
    case json of
      Just (Object obj) -> do
        KM.lookup "type" obj `shouldBe` Just (String "failed")
        KM.lookup "error" obj `shouldBe` Just (String "Node execution timeout")
      _ -> expectationFailure "Expected JSON object"

  describe "round-trip" $ do
    it "round-trips all ExecutionPhase variants" $ do
      let phases =
            [ PhaseIdle
            , PhaseInNode "test"
            , PhaseTransitioning "a" "b"
            , PhaseCompleted (String "done")
            , PhaseFailed "err"
            ]
      mapM_ (\p -> decode (encode p) `shouldBe` Just p) phases


-- ════════════════════════════════════════════════════════════════════════════
-- GraphState conformance with protocol.ts
-- ════════════════════════════════════════════════════════════════════════════

graphStateConformanceSpec :: Spec
graphStateConformanceSpec = describe "GraphState matches protocol.ts" $ do

  it "encodes as {phase: {...}, completedNodes: [...]}" $ do
    let state = GraphState
          { gsPhase = PhaseInNode "classify"
          , gsCompletedNodes = ["entry", "validate"]
          }
        json = decode (encode state) :: Maybe Value
    case json of
      Just (Object obj) -> do
        -- Verify field names match protocol.ts
        case KM.lookup "phase" obj of
          Just (Object phaseObj) ->
            KM.lookup "type" phaseObj `shouldBe` Just (String "in_node")
          _ -> expectationFailure "Expected phase object"
        case KM.lookup "completedNodes" obj of
          Just (Array nodes) ->
            length nodes `shouldBe` 2
          _ -> expectationFailure "Expected completedNodes array"
      _ -> expectationFailure "Expected JSON object"

  it "encodes empty completedNodes as empty array" $ do
    let state = GraphState
          { gsPhase = PhaseIdle
          , gsCompletedNodes = []
          }
        json = decode (encode state) :: Maybe Value
    case json of
      Just (Object obj) -> do
        case KM.lookup "completedNodes" obj of
          Just (Array nodes) ->
            length nodes `shouldBe` 0
          _ -> expectationFailure "Expected completedNodes array"
      _ -> expectationFailure "Expected JSON object"

  it "round-trips GraphState" $ do
    let state = GraphState
          { gsPhase = PhaseTransitioning "a" "b"
          , gsCompletedNodes = ["x", "y", "z"]
          }
    decode (encode state) `shouldBe` Just state


-- ════════════════════════════════════════════════════════════════════════════
-- StepOutput conformance with protocol.ts
-- ════════════════════════════════════════════════════════════════════════════

stepOutputConformanceSpec :: Spec
stepOutputConformanceSpec = describe "StepOutput matches protocol.ts" $ do

  it "has all required fields: effect, done, stepResult, graphState" $ do
    let output = StepYield (EffLogInfo "computing") (GraphState (PhaseInNode "compute") [])
        json = decode (encode output) :: Maybe Value
    case json of
      Just (Object obj) -> do
        -- Verify ALL fields are present (this is the key conformance check)
        KM.member "effect" obj `shouldBe` True
        KM.member "done" obj `shouldBe` True
        KM.member "stepResult" obj `shouldBe` True
        KM.member "graphState" obj `shouldBe` True

        -- Verify field values
        KM.lookup "done" obj `shouldBe` Just (Bool False)
        KM.lookup "stepResult" obj `shouldBe` Just Null
        case KM.lookup "graphState" obj of
          Just (Object gsObj) ->
            KM.member "phase" gsObj `shouldBe` True
          _ -> expectationFailure "Expected graphState object"
      _ -> expectationFailure "Expected JSON object"

  it "encodes effect correctly when present" $ do
    let output = StepYield (EffLogInfo "test") (GraphState PhaseIdle [])
        json = decode (encode output) :: Maybe Value
    case json of
      Just (Object obj) -> do
        case KM.lookup "effect" obj of
          Just (Object effObj) -> do
            KM.lookup "type" effObj `shouldBe` Just (String "LogInfo")
            KM.lookup "eff_message" effObj `shouldBe` Just (String "test")
          _ -> expectationFailure "Expected effect object"
      _ -> expectationFailure "Expected JSON object"

  it "encodes null effect correctly" $ do
    let output = StepDone (Number 42) (GraphState (PhaseCompleted (Number 42)) ["compute"])
        json = decode (encode output) :: Maybe Value
    case json of
      Just (Object obj) -> do
        KM.lookup "effect" obj `shouldBe` Just Null
        KM.lookup "done" obj `shouldBe` Just (Bool True)
        KM.lookup "stepResult" obj `shouldBe` Just (Number 42)
      _ -> expectationFailure "Expected JSON object"

  it "round-trips StepOutput with graphState" $ do
    let output = StepYield (EffLogInfo "msg") (GraphState (PhaseInNode "test") ["a", "b"])
    decode (encode output) `shouldBe` Just output

  describe "graphState field (critical for TypeScript compatibility)" $ do
    it "is always present in output, never omitted" $ do
      -- Test all three StepOutput variants - graphState is always present
      let outputs =
            [ StepYield (EffLogInfo "x") (GraphState PhaseIdle [])
            , StepDone (String "done") (GraphState (PhaseCompleted (String "done")) [])
            , StepFailed "error" (GraphState (PhaseFailed "error") [])
            ]
      forM_ outputs $ \output -> do
        let json = decode (encode output) :: Maybe Value
        case json of
          Just (Object obj) ->
            KM.member "graphState" obj `shouldBe` True
          _ -> expectationFailure "Expected JSON object"

    it "phase field uses correct discriminator names" $ do
      -- This tests the critical mapping between Haskell constructors and TS type literals
      let testCases =
            [ (PhaseIdle, "idle")
            , (PhaseInNode "x", "in_node")
            , (PhaseTransitioning "a" "b", "transitioning")
            , (PhaseCompleted Null, "completed")
            , (PhaseFailed "e", "failed")
            ]
      forM_ testCases $ \(phase, expectedType) -> do
        let state = GraphState phase []
            -- Use StepYield to test graphState encoding for each phase
            output = StepYield (EffLogInfo "test") state
            json = decode (encode output) :: Maybe Value
        case json of
          Just (Object obj) -> do
            case KM.lookup "graphState" obj of
              Just (Object gsObj) -> do
                case KM.lookup "phase" gsObj of
                  Just (Object phaseObj) ->
                    KM.lookup "type" phaseObj `shouldBe` Just (String expectedType)
                  _ -> expectationFailure "Expected phase object"
              _ -> expectationFailure "Expected graphState object"
          _ -> expectationFailure "Expected JSON object"


-- Helper for iteration in tests
forM_ :: (Monad m) => [a] -> (a -> m b) -> m ()
forM_ xs f = mapM_ f xs
