{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Tests for ExampleGraph - the multi-node message classifier.
--
-- These tests verify:
-- 1. Each branch path through the graph
-- 2. Correct classification logic
-- 3. Effect yields at each step
-- 4. Proper handler responses
module ExampleGraphSpec (spec) where

import Test.Hspec
import qualified Data.Text as T

import Tidepool.Graph.Goto (GotoChoice(..), OneOf(..), To)
import Tidepool.Graph.Types (Exit, Self)
import Tidepool.Wasm.Effect (WasmM)
import Tidepool.Wasm.ExampleGraph
  ( UserMessage(..)
  , Response(..)
  , classifyHandlerWasm
  , greetingHandlerWasm
  , questionHandlerWasm
  , statementHandlerWasm
  , runExampleGraph
  )
import Tidepool.Wasm.Runner (initializeWasm, WasmResult(..))
import Tidepool.Wasm.WireTypes (SerializableEffect(..), EffectResult(..))
import Data.Aeson (Value(..))


spec :: Spec
spec = do
  classifySpec
  greetingHandlerSpec
  questionHandlerSpec
  statementHandlerSpec
  fullGraphSpec


-- ============================================================================
-- Classify Handler Tests
-- ============================================================================

classifySpec :: Spec
classifySpec = describe "classifyHandlerWasm" $ do

  it "routes greetings to handleGreeting" $ do
    let msg = UserMessage "Hello there!"
    case initializeWasm (classifyHandlerWasm msg) of
      WasmYield (EffLogInfo logMsg) resume -> do
        T.unpack logMsg `shouldContain` "Classifying message"
        T.unpack logMsg `shouldContain` "Hello there!"
        -- Resume the first log, get the second
        case resume (ResSuccess Nothing) of
          WasmYield (EffLogInfo logMsg2) resume2 -> do
            T.unpack logMsg2 `shouldContain` "Greeting"
            -- Resume and check final result
            case resume2 (ResSuccess Nothing) of
              WasmComplete (GotoChoice (Here _)) -> pure ()  -- handleGreeting = Here
              WasmComplete _ -> expectationFailure "Expected route to handleGreeting (Here)"
              _ -> expectationFailure "Expected WasmComplete"
          _ -> expectationFailure "Expected second log yield"
      _ -> expectationFailure "Expected Log effect yield"

  it "routes questions to handleQuestion" $ do
    let msg = UserMessage "What is the meaning of life?"
    result <- runClassifyToCompletion msg
    case result of
      GotoChoice (There (Here _)) -> pure ()  -- handleQuestion = There (Here _)
      _ -> expectationFailure "Expected route to handleQuestion"

  it "routes statements to handleStatement" $ do
    let msg = UserMessage "The sky is blue."
    result <- runClassifyToCompletion msg
    case result of
      GotoChoice (There (There (Here _))) -> pure ()  -- handleStatement
      _ -> expectationFailure "Expected route to handleStatement"

  it "classifies 'hi' as greeting" $ do
    let msg = UserMessage "hi"
    result <- runClassifyToCompletion msg
    case result of
      GotoChoice (Here _) -> pure ()
      _ -> expectationFailure "Expected greeting classification"

  it "classifies 'how are you?' as question" $ do
    let msg = UserMessage "how are you?"
    result <- runClassifyToCompletion msg
    case result of
      GotoChoice (There (Here _)) -> pure ()
      _ -> expectationFailure "Expected question classification"

  it "classifies 'Is this working?' as question" $ do
    let msg = UserMessage "Is this working?"
    result <- runClassifyToCompletion msg
    case result of
      GotoChoice (There (Here _)) -> pure ()
      _ -> expectationFailure "Expected question classification"


-- | Run classify handler to completion, resuming all log effects.
runClassifyToCompletion
  :: UserMessage
  -> IO (GotoChoice '[To "handleGreeting" UserMessage
                    , To "handleQuestion" UserMessage
                    , To "handleStatement" UserMessage])
runClassifyToCompletion msg = go (initializeWasm (classifyHandlerWasm msg))
  where
    go :: WasmResult (GotoChoice '[To "handleGreeting" UserMessage
                                 , To "handleQuestion" UserMessage
                                 , To "handleStatement" UserMessage])
       -> IO (GotoChoice '[To "handleGreeting" UserMessage
                         , To "handleQuestion" UserMessage
                         , To "handleStatement" UserMessage])
    go (WasmYield _ resume) = go (resume (ResSuccess Nothing))
    go (WasmComplete choice) = pure choice
    go (WasmError err) = error $ "Unexpected error: " <> T.unpack err


-- ============================================================================
-- Greeting Handler Tests
-- ============================================================================

greetingHandlerSpec :: Spec
greetingHandlerSpec = describe "greetingHandlerWasm" $ do

  it "yields log effect before responding" $ do
    let msg = UserMessage "Hello!"
    case initializeWasm (greetingHandlerWasm msg) of
      WasmYield (EffLogInfo logMsg) _ -> do
        T.unpack logMsg `shouldContain` "Handling greeting"
        T.unpack logMsg `shouldContain` "Hello!"
      _ -> expectationFailure "Expected Log effect yield"

  it "returns response containing original message" $ do
    let msg = UserMessage "Greetings!"
    response <- runSimpleHandlerToResponse (greetingHandlerWasm msg)
    T.unpack response.unResponse `shouldContain` "Greetings!"

  it "returns friendly greeting response" $ do
    let msg = UserMessage "Hi there"
    response <- runSimpleHandlerToResponse (greetingHandlerWasm msg)
    T.unpack response.unResponse `shouldContain` "Hello!"
    T.unpack response.unResponse `shouldContain` "nice to hear from you"


-- ============================================================================
-- Question Handler Tests
-- ============================================================================

questionHandlerSpec :: Spec
questionHandlerSpec = describe "questionHandlerWasm" $ do

  it "yields log effect first" $ do
    let msg = UserMessage "What time is it?"
    case initializeWasm (questionHandlerWasm msg) of
      WasmYield (EffLogInfo logMsg) _ -> do
        T.unpack logMsg `shouldContain` "Handling question"
      _ -> expectationFailure "Expected Log effect yield"

  it "yields LlmComplete effect after log" $ do
    let msg = UserMessage "Why is the sky blue?"
    case initializeWasm (questionHandlerWasm msg) of
      WasmYield _ resume ->
        case resume (ResSuccess Nothing) of
          WasmYield (EffLlmComplete node sys user _schema) _ -> do
            node `shouldBe` "question_handler"
            T.unpack sys `shouldContain` "helpful assistant"
            user `shouldBe` "Why is the sky blue?"
          _ -> expectationFailure "Expected LlmComplete effect"
      _ -> expectationFailure "Expected first Log yield"

  it "uses LLM response in output" $ do
    let msg = UserMessage "What is 2+2?"
    -- Simulate LLM returning a specific answer
    let llmResponse = String "The answer is 4."
    response <- runQuestionHandlerWithLLM msg llmResponse
    response.unResponse `shouldBe` "The answer is 4."

  it "handles non-string LLM response gracefully" $ do
    let msg = UserMessage "What is this?"
    -- Simulate LLM returning a non-string value
    let llmResponse = Number 42
    response <- runQuestionHandlerWithLLM msg llmResponse
    T.unpack response.unResponse `shouldContain` "What is this?"


-- | Run question handler with a mock LLM response.
runQuestionHandlerWithLLM :: UserMessage -> Value -> IO Response
runQuestionHandlerWithLLM msg llmResponse = go (initializeWasm (questionHandlerWasm msg))
  where
    go :: WasmResult (GotoChoice '[To Exit Response, To Self UserMessage]) -> IO Response
    go (WasmYield (EffLogInfo _) resume) = go (resume (ResSuccess Nothing))
    go (WasmYield (EffLlmComplete _ _ _ _) resume) = go (resume (ResSuccess (Just llmResponse)))
    go (WasmYield _ resume) = go (resume (ResSuccess Nothing))  -- Handle any other effects
    go (WasmComplete (GotoChoice (Here response))) = pure response
    go (WasmComplete _) = error "Expected Exit, got Self"
    go (WasmError err) = error $ "Unexpected error: " <> T.unpack err


-- ============================================================================
-- Statement Handler Tests
-- ============================================================================

statementHandlerSpec :: Spec
statementHandlerSpec = describe "statementHandlerWasm" $ do

  it "yields log effect before responding" $ do
    let msg = UserMessage "The weather is nice today."
    case initializeWasm (statementHandlerWasm msg) of
      WasmYield (EffLogInfo logMsg) _ -> do
        T.unpack logMsg `shouldContain` "Handling statement"
        T.unpack logMsg `shouldContain` "The weather is nice today."
      _ -> expectationFailure "Expected Log effect yield"

  it "returns acknowledgment response" $ do
    let msg = UserMessage "I like pizza."
    response <- runSimpleHandlerToResponse (statementHandlerWasm msg)
    T.unpack response.unResponse `shouldContain` "I understand"
    T.unpack response.unResponse `shouldContain` "I like pizza."


-- ============================================================================
-- Full Graph Integration Tests
-- ============================================================================

fullGraphSpec :: Spec
fullGraphSpec = describe "runExampleGraph (full integration)" $ do

  it "processes greeting through full graph" $ do
    let msg = UserMessage "Hello!"
    response <- runFullGraph msg
    T.unpack response.unResponse `shouldContain` "Hello!"
    T.unpack response.unResponse `shouldContain` "nice to hear from you"

  it "processes question through full graph with LLM" $ do
    let msg = UserMessage "What is Haskell?"
    let llmResponse = String "Haskell is a functional programming language."
    response <- runFullGraphWithLLM msg llmResponse
    response.unResponse `shouldBe` "Haskell is a functional programming language."

  it "processes statement through full graph" $ do
    let msg = UserMessage "Today is a good day."
    response <- runFullGraph msg
    T.unpack response.unResponse `shouldContain` "I understand"
    T.unpack response.unResponse `shouldContain` "Today is a good day."

  it "handles 'good morning' as greeting" $ do
    let msg = UserMessage "Good morning!"
    response <- runFullGraph msg
    T.unpack response.unResponse `shouldContain` "nice to hear from you"

  it "handles 'bye' as greeting" $ do
    let msg = UserMessage "bye"
    response <- runFullGraph msg
    T.unpack response.unResponse `shouldContain` "nice to hear from you"


-- ============================================================================
-- Test Helpers
-- ============================================================================

-- | Run a simple handler (greeting/statement) to get the response.
runSimpleHandlerToResponse :: WasmM (GotoChoice '[To Exit Response]) -> IO Response
runSimpleHandlerToResponse handler = go (initializeWasm handler)
  where
    go :: WasmResult (GotoChoice '[To Exit Response]) -> IO Response
    go (WasmYield _ resume) = go (resume (ResSuccess Nothing))
    go (WasmComplete (GotoChoice (Here response))) = pure response
    go (WasmComplete (GotoChoice (There impossible))) = case impossible of {}
    go (WasmError err) = error $ "Unexpected error: " <> T.unpack err

-- | Run the full graph with default effect responses.
runFullGraph :: UserMessage -> IO Response
runFullGraph msg = runFullGraphWithLLM msg (String "Default LLM response")

-- | Run the full graph with a specific LLM response for questions.
runFullGraphWithLLM :: UserMessage -> Value -> IO Response
runFullGraphWithLLM msg llmResponse = go (initializeWasm (runExampleGraph msg))
  where
    go :: WasmResult Response -> IO Response
    go (WasmYield (EffLlmComplete _ _ _ _) resume) = go (resume (ResSuccess (Just llmResponse)))
    go (WasmYield _ resume) = go (resume (ResSuccess Nothing))
    go (WasmComplete response) = pure response
    go (WasmError err) = error $ "Unexpected error: " <> T.unpack err
