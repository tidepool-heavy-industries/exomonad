{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests for LlmTestGraph structure and WasmM handler behavior.
--
-- These tests verify that:
-- 1. LlmTestGraph compiles (passes graph validation constraints)
-- 2. The echoHandlerWasm handler works with WasmM effects
-- 3. The handler correctly uses the LlmComplete effect
module LlmTestGraphSpec (spec) where

import Test.Hspec
import Data.Aeson (object, (.=), Value(..))
import qualified Data.Text as T

import Tidepool.Graph.Goto (GotoChoice, OneOf)
import Tidepool.Graph.Goto.Internal (GotoChoice(..), OneOf(..))  -- For test assertions
import Tidepool.Wasm.LlmTestGraph (echoHandlerWasm)
import Tidepool.Wasm.Runner (initializeWasm, WasmResult(..))
import Tidepool.Wasm.WireTypes (SerializableEffect(..), EffectResult(..))


spec :: Spec
spec = do
  echoHandlerSpec
  graphStructureSpec


-- ════════════════════════════════════════════════════════════════════════════
-- Echo Handler
-- ════════════════════════════════════════════════════════════════════════════

echoHandlerSpec :: Spec
echoHandlerSpec = describe "echoHandlerWasm" $ do

  it "returns LLM response for 'hello'" $ do
    let mockResponse = object ["response" .= ("Echo: hello" :: String)]
    runEchoHandler "hello" mockResponse `shouldBe` "Echo: hello"

  it "extracts 'text' field from response" $ do
    let mockResponse = object ["text" .= ("Echoed text" :: String)]
    runEchoHandler "test" mockResponse `shouldBe` "Echoed text"

  it "extracts 'content' field from response" $ do
    let mockResponse = object ["content" .= ("Content here" :: String)]
    runEchoHandler "test" mockResponse `shouldBe` "Content here"

  it "handles direct String response" $ do
    runEchoHandler "direct" (String "Direct response") `shouldBe` "Direct response"

  it "handles empty input" $ do
    let mockResponse = object ["response" .= ("Echo: " :: String)]
    runEchoHandler "" mockResponse `shouldBe` "Echo: "


-- | Helper to run the echo handler through full yield/resume cycle.
--
-- The handler makes an LLM call (yield), we resume with mock response, and extract the result.
runEchoHandler :: T.Text -> Value -> T.Text
runEchoHandler input mockResponse =
  case initializeWasm (echoHandlerWasm input) of
    WasmYield _ resume ->
      case resume (ResSuccess (Just mockResponse)) of
        WasmComplete (GotoChoice (Here result)) -> result
        WasmError msg -> error $ "runEchoHandler: WasmError after resume: " <> T.unpack msg
        _ -> error "runEchoHandler: expected WasmComplete after resume"
    WasmComplete (GotoChoice (Here result)) ->
      -- Handler completed without yielding (shouldn't happen for current impl)
      error $ "runEchoHandler: handler completed without yielding, got: " <> T.unpack result
    WasmError msg ->
      error $ "runEchoHandler: WasmError: " <> T.unpack msg
    _ -> error "runEchoHandler: unexpected result"


-- ════════════════════════════════════════════════════════════════════════════
-- Graph Structure
-- ════════════════════════════════════════════════════════════════════════════

graphStructureSpec :: Spec
graphStructureSpec = describe "LlmTestGraph structure" $ do

  it "handler yields LlmComplete effect" $ do
    case initializeWasm (echoHandlerWasm "test message") of
      WasmYield (EffLlmComplete{}) _ -> pure ()
      WasmYield eff _ ->
        expectationFailure $ "Expected EffLlmComplete, got: " <> show eff
      _ -> expectationFailure "Expected yield"

  it "handler includes node name 'echo'" $ do
    case initializeWasm (echoHandlerWasm "test") of
      WasmYield (EffLlmComplete node _ _ _) _ ->
        node `shouldBe` "echo"
      _ -> expectationFailure "Expected EffLlmComplete yield"

  it "handler includes user message in content" $ do
    case initializeWasm (echoHandlerWasm "hello world") of
      WasmYield (EffLlmComplete _ _ userContent _) _ -> do
        T.unpack userContent `shouldContain` "hello world"
      _ -> expectationFailure "Expected EffLlmComplete yield"

  it "handler uses system prompt for echo bot" $ do
    case initializeWasm (echoHandlerWasm "test") of
      WasmYield (EffLlmComplete _ sysPrompt _ _) _ -> do
        T.unpack sysPrompt `shouldContain` "echo"
      _ -> expectationFailure "Expected EffLlmComplete yield"

  it "handler uses no schema (free-form response)" $ do
    case initializeWasm (echoHandlerWasm "test") of
      WasmYield (EffLlmComplete _ _ _ schema) _ ->
        schema `shouldBe` Nothing
      _ -> expectationFailure "Expected EffLlmComplete yield"
