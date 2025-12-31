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
import Data.Vector qualified as V

import Tidepool.Wasm.WireTypes


spec :: Spec
spec = do
  serializableEffectSpec
  effectResultSpec
  stepOutputSpec
  graphStateSpec
  habiticaEffectSpec


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
  let idleState = GraphState PhaseIdle []

  it "round-trips StepOutput with effect, not done" $ do
    let output = StepOutput
          { soEffect = Just (EffLogInfo "computing")
          , soDone = False
          , soStepResult = Nothing
          , soGraphState = idleState
          }
    decode (encode output) `shouldBe` Just output

  it "round-trips StepOutput done with result" $ do
    let output = StepOutput
          { soEffect = Nothing
          , soDone = True
          , soStepResult = Just (Number 42)
          , soGraphState = GraphState (PhaseCompleted (Number 42)) []
          }
    decode (encode output) `shouldBe` Just output

  it "round-trips StepOutput with all fields populated" $ do
    let output = StepOutput
          { soEffect = Just (EffLogInfo "msg")
          , soDone = True
          , soStepResult = Just (String "result")
          , soGraphState = GraphState (PhaseInNode "test") ["a", "b"]
          }
    decode (encode output) `shouldBe` Just output

  it "encodes StepOutput with correct JSON structure" $ do
    let output = StepOutput
          { soEffect = Just (EffLogInfo "test")
          , soDone = False
          , soStepResult = Nothing
          , soGraphState = idleState
          }
        json = decode (encode output) :: Maybe Value
    case json of
      Just (Object obj) -> do
        KM.lookup "done" obj `shouldBe` Just (Bool False)
        case KM.lookup "effect" obj of
          Just (Object effObj) ->
            KM.lookup "type" effObj `shouldBe` Just (String "LogInfo")
          _ -> expectationFailure "Expected effect object"
        -- Verify graphState is present
        case KM.lookup "graphState" obj of
          Just (Object gsObj) ->
            KM.lookup "completedNodes" gsObj `shouldBe` Just (Array V.empty)
          _ -> expectationFailure "Expected graphState object"
      _ -> expectationFailure "Expected JSON object"


graphStateSpec :: Spec
graphStateSpec = describe "GraphState" $ do

  it "round-trips GraphState with PhaseIdle" $ do
    let gs = GraphState PhaseIdle []
    decode (encode gs) `shouldBe` Just gs

  it "round-trips GraphState with PhaseInNode" $ do
    let gs = GraphState (PhaseInNode "compute") []
    decode (encode gs) `shouldBe` Just gs

  it "round-trips GraphState with PhaseTransitioning" $ do
    let gs = GraphState (PhaseTransitioning "entry" "compute") []
    decode (encode gs) `shouldBe` Just gs

  it "round-trips GraphState with PhaseCompleted" $ do
    let gs = GraphState (PhaseCompleted (Number 42)) ["compute"]
    decode (encode gs) `shouldBe` Just gs

  it "round-trips GraphState with PhaseFailed" $ do
    let gs = GraphState (PhaseFailed "something went wrong") []
    decode (encode gs) `shouldBe` Just gs

  it "round-trips GraphState with multiple completed nodes" $ do
    let gs = GraphState (PhaseCompleted (String "done")) ["entry", "compute", "validate"]
    decode (encode gs) `shouldBe` Just gs

  it "encodes ExecutionPhase with correct type field" $ do
    let gs = GraphState (PhaseInNode "mynode") []
        json = decode (encode gs) :: Maybe Value
    case json of
      Just (Object obj) ->
        case KM.lookup "phase" obj of
          Just (Object phaseObj) -> do
            KM.lookup "type" phaseObj `shouldBe` Just (String "in_node")
            KM.lookup "nodeName" phaseObj `shouldBe` Just (String "mynode")
          _ -> expectationFailure "Expected phase object"
      _ -> expectationFailure "Expected JSON object"


-- ════════════════════════════════════════════════════════════════════════════
-- EffHabitica
-- ════════════════════════════════════════════════════════════════════════════

habiticaEffectSpec :: Spec
habiticaEffectSpec = describe "EffHabitica" $ do

  it "round-trips EffHabitica GetUser" $ do
    let effect = EffHabitica "GetUser" (object [])
    decode (encode effect) `shouldBe` Just effect

  it "round-trips EffHabitica ScoreTask" $ do
    let payload = object ["taskId" .= ("abc123" :: String), "direction" .= ("Up" :: String)]
        effect = EffHabitica "ScoreTask" payload
    decode (encode effect) `shouldBe` Just effect

  it "round-trips EffHabitica GetTasks" $ do
    let payload = object ["taskType" .= ("Todos" :: String)]
        effect = EffHabitica "GetTasks" payload
    decode (encode effect) `shouldBe` Just effect

  it "round-trips EffHabitica FetchTodos" $ do
    let effect = EffHabitica "FetchTodos" (object [])
    decode (encode effect) `shouldBe` Just effect

  it "round-trips EffHabitica CreateTodo" $ do
    let payload = object ["title" .= ("My new task" :: String)]
        effect = EffHabitica "CreateTodo" payload
    decode (encode effect) `shouldBe` Just effect

  it "round-trips EffHabitica AddChecklistItem" $ do
    let payload = object ["todoId" .= ("xyz789" :: String), "item" .= ("Subtask" :: String)]
        effect = EffHabitica "AddChecklistItem" payload
    decode (encode effect) `shouldBe` Just effect

  it "encodes EffHabitica with correct JSON structure" $ do
    let payload = object ["taskId" .= ("xyz" :: String)]
        effect = EffHabitica "ScoreTask" payload
        json = decode (encode effect) :: Maybe Value
    case json of
      Just (Object obj) -> do
        KM.lookup "type" obj `shouldBe` Just (String "Habitica")
        KM.lookup "eff_hab_op" obj `shouldBe` Just (String "ScoreTask")
        case KM.lookup "eff_hab_payload" obj of
          Just (Object p) -> KM.lookup "taskId" p `shouldBe` Just (String "xyz")
          _ -> expectationFailure "Expected payload object"
      _ -> expectationFailure "Expected JSON object"

  it "encodes EffHabitica GetUser with empty payload" $ do
    let effect = EffHabitica "GetUser" (object [])
        json = decode (encode effect) :: Maybe Value
    case json of
      Just (Object obj) -> do
        KM.lookup "type" obj `shouldBe` Just (String "Habitica")
        KM.lookup "eff_hab_op" obj `shouldBe` Just (String "GetUser")
        KM.lookup "eff_hab_payload" obj `shouldBe` Just (Object KM.empty)
      _ -> expectationFailure "Expected JSON object"
