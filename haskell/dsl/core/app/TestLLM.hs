-- | Integration test: extended thinking + tools + structured output
module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (lookupEnv)
import Data.Aeson (Value, object, (.=), encode)
import qualified Data.ByteString.Lazy.Char8 as LBS

import ExoMonad.Anthropic.Client

main :: IO ()
main = do
  putStrLn "Integration Test: Thinking + Tools + Structured Output"
  putStrLn "======================================================="

  maybeKey <- lookupEnv "ANTHROPIC_API_KEY"
  case maybeKey of
    Nothing -> putStrLn "Set ANTHROPIC_API_KEY and run: cabal run test-llm"
    Just apiKey -> do
      let config = ClientConfig
            { apiKey = T.pack apiKey
            , defaultModel = "claude-haiku-4-5-20251001"
            , defaultMaxTokens = 4096
            }

      -- Test with tool + thinking
      putStrLn "\n[1] Testing tool use + extended thinking..."
      testToolUse config

      -- Test with structured output (no thinking - not compatible)
      putStrLn "\n[2] Testing structured output..."
      testStructuredOutput config

      putStrLn "\nAll tests complete!"

-- | Test tool use with extended thinking
testToolUse :: ClientConfig -> IO ()
testToolUse config = do
  let calculatorTool = object
        [ "name" .= ("calculator" :: Text)
        , "description" .= ("Perform arithmetic. Use this for any math." :: Text)
        , "input_schema" .= object
            [ "type" .= ("object" :: Text)
            , "properties" .= object
                [ "expression" .= object
                    [ "type" .= ("string" :: Text)
                    , "description" .= ("Math expression like '2 + 2'" :: Text)
                    ]
                ]
            , "required" .= (["expression"] :: [Text])
            ]
        ]

      req = SingleCallRequest
        { scrMessages = [Message User [TextBlock "What is 15 * 7? Use the calculator tool."]]
        , scrSystemPrompt = Just "You have a calculator tool. Always use it for math."
        , scrOutputSchema = Nothing
        , scrTools = [calculatorTool]
        , scrThinkingBudget = Just 1024
        }

  result <- callMessagesOnce config req
  case result of
    Left err -> putStrLn $ "  Error: " <> show err
    Right resp -> do
      putStrLn $ "  Stop reason: " <> show resp.scrStopReason
      mapM_ printBlock resp.scrContent
      putStrLn "  ✓ Tool use test passed"

-- | Test structured output WITH extended thinking (they're compatible!)
testStructuredOutput :: ClientConfig -> IO ()
testStructuredOutput config = do
  let schema = object
        [ "type" .= ("object" :: Text)
        , "properties" .= object
            [ "answer" .= object ["type" .= ("integer" :: Text)]
            , "explanation" .= object ["type" .= ("string" :: Text)]
            ]
        , "required" .= (["answer", "explanation"] :: [Text])
        , "additionalProperties" .= False
        ]

      req = SingleCallRequest
        { scrMessages = [Message User [TextBlock "What is 6 * 9?"]]
        , scrSystemPrompt = Nothing
        , scrOutputSchema = Just schema
        , scrTools = []
        , scrThinkingBudget = Just 1024  -- Thinking + structured output work together!
        }

  result <- callMessagesOnce config req
  case result of
    Left err -> putStrLn $ "  Error: " <> show err
    Right resp -> do
      putStrLn $ "  Stop reason: " <> show resp.scrStopReason
      mapM_ printBlock resp.scrContent
      putStrLn "  ✓ Structured output test passed"

printBlock :: ContentBlock -> IO ()
printBlock (TextBlock t) = do
  TIO.putStrLn $ "  [text] " <> t
printBlock (ThinkingBlock tc) = do
  TIO.putStrLn $ "  [thinking] " <> T.take 80 tc.thinkingText <> "..."
printBlock (RedactedThinkingBlock _) =
  putStrLn "  [redacted thinking]"
printBlock (ToolUseBlock tu) = do
  TIO.putStrLn $ "  [tool_use] " <> tu.toolName <> " -> " <> T.pack (LBS.unpack $ encode tu.toolInput)
printBlock (ToolResultBlock _) =
  putStrLn "  [tool_result]"
