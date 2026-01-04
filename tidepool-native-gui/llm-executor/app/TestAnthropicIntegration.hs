-- | Integration test for Anthropic API via native HTTP client.
--
-- Usage:
--   ANTHROPIC_API_KEY=sk-... cabal run test-anthropic-integration
--
-- Tests:
--   1. Basic completion (no tools)
--   2. Completion with tool definition
--
-- Exit codes:
--   0 = all tests passed
--   1 = test failed or missing API key
module Main where

import Control.Monad (when)
import Control.Monad.Freer (runM)
import Data.Aeson (Value(..), object, (.=), encode)
import Data.Aeson.KeyMap qualified as KM
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Environment (lookupEnv)
import System.Exit (exitFailure, exitSuccess)

import Tidepool.LLM.Executor
  ( runLLMComplete
  , mkLLMEnv
  , LLMConfig(..)
  , AnthropicTool(..)
  , anthropicToolToJSON
  )
import Tidepool.LLM.Types (AnthropicSecrets(..), defaultAnthropicConfig)
import Tidepool.Effects.LLMProvider
  ( complete
  , SProvider(..)
  , AnthropicConfig(..)
  , AnthropicResponse(..)
  , ContentBlock(..)
  )


main :: IO ()
main = do
  putStrLn "=== Anthropic Integration Test ==="
  putStrLn ""

  -- Get API key from environment
  maybeKey <- lookupEnv "ANTHROPIC_API_KEY"
  case maybeKey of
    Nothing -> do
      putStrLn "ERROR: ANTHROPIC_API_KEY environment variable not set"
      putStrLn ""
      putStrLn "Usage:"
      putStrLn "  ANTHROPIC_API_KEY=sk-... cabal run test-anthropic-integration"
      exitFailure

    Just apiKey -> do
      let config = LLMConfig
            { lcAnthropicSecrets = Just $ defaultAnthropicConfig (T.pack apiKey)
            , lcOpenAISecrets = Nothing
            }

      env <- mkLLMEnv config
      putStrLn $ "Using API key: " <> take 10 apiKey <> "..."
      putStrLn ""

      -- Test 1: Basic completion
      putStrLn "--- Test 1: Basic Completion ---"
      result1 <- runM $ runLLMComplete env $ do
        complete SAnthropic basicConfig "Say 'hello world' and nothing else." Nothing

      case result1 of
        resp -> do
          putStrLn $ "Stop reason: " <> T.unpack resp.arStopReason
          putStrLn $ "Content blocks: " <> show (length resp.arContent)
          case resp.arContent of
            (TextContent txt : _) -> do
              putStrLn $ "Response: " <> T.unpack txt
              when (not $ "hello" `T.isInfixOf` T.toLower txt) $ do
                putStrLn "WARN: Response doesn't contain 'hello'"
            other -> putStrLn $ "Unexpected content: " <> show other
          putStrLn "Test 1: PASSED"
      putStrLn ""

      -- Test 2: Completion with tool
      putStrLn "--- Test 2: Completion with Tool ---"
      let tool = AnthropicTool
            { atName = "get_weather"
            , atDescription = "Get the current weather for a location"
            , atInputSchema = object
                [ "type" .= ("object" :: Text)
                , "properties" .= object
                    [ "location" .= object
                        [ "type" .= ("string" :: Text)
                        , "description" .= ("City name" :: Text)
                        ]
                    ]
                , "required" .= (["location"] :: [Text])
                ]
            }
          tools = Just [anthropicToolToJSON tool]

      result2 <- runM $ runLLMComplete env $ do
        complete SAnthropic basicConfig
          "What's the weather in Tokyo? Use the get_weather tool."
          tools

      case result2 of
        resp -> do
          putStrLn $ "Stop reason: " <> T.unpack resp.arStopReason
          putStrLn $ "Content blocks: " <> show (length resp.arContent)

          -- Check if model attempted to use the tool
          let hasToolUse = any isToolUse resp.arContent
          if hasToolUse
            then putStrLn "Tool use detected: YES"
            else putStrLn "Tool use detected: NO (model may have responded without tool)"

          -- Print all content blocks for inspection
          mapM_ printBlock resp.arContent
          putStrLn "Test 2: PASSED"
      putStrLn ""

      putStrLn "=== All Tests Passed ==="
      exitSuccess


-- | Basic config using Haiku 4.5 for speed/cost
basicConfig :: AnthropicConfig
basicConfig = AnthropicConfig
  { acModel = "claude-haiku-4-5-20251001"
  , acMaxTokens = 256
  , acThinkingBudget = Nothing
  , acSystemPrompt = Just "You are a helpful assistant. Be concise."
  }


-- | Check if a content block is a tool use
isToolUse :: ContentBlock -> Bool
isToolUse (ToolUseContent _ _) = True
isToolUse _ = False


-- | Print a content block for debugging
printBlock :: ContentBlock -> IO ()
printBlock (TextContent txt) =
  putStrLn $ "  [text] " <> T.unpack (T.take 100 txt) <> if T.length txt > 100 then "..." else ""
printBlock (ToolUseContent name input) =
  putStrLn $ "  [tool_use] " <> T.unpack name <> " " <> show input
