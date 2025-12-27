-- | Minimal test: single LLM call with extended thinking
module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (lookupEnv)

import Tidepool.Anthropic.Client
import Tidepool.Anthropic.Http (ApiError(..))

main :: IO ()
main = do
  putStrLn "Single LLM call test"
  putStrLn "===================="

  maybeKey <- lookupEnv "ANTHROPIC_API_KEY"
  case maybeKey of
    Nothing -> putStrLn "Set ANTHROPIC_API_KEY and run: cabal run test-llm"
    Just apiKey -> do
      let config = ClientConfig
            { apiKey = T.pack apiKey
            , defaultModel = "claude-haiku-4-5-20251001"  -- Haiku 4.5 supports extended thinking!
            , defaultMaxTokens = 2048  -- Must be > thinking budget
            }

          req = SingleCallRequest
            { scrMessages = [Message User [TextBlock "What is 2+2? Reply with just the number."]]
            , scrSystemPrompt = Nothing
            , scrOutputSchema = Nothing
            , scrTools = []
            , scrThinkingBudget = Just 1024  -- Minimum is 1024
            }

      putStrLn "Calling API with extended thinking..."
      result <- callMessagesOnce config req

      case result of
        Left err -> putStrLn $ "Error: " <> show err
        Right resp -> do
          putStrLn $ "Stop reason: " <> show resp.scrStopReason
          putStrLn "Content blocks:"
          mapM_ printBlock resp.scrContent
  where
    printBlock (TextBlock t) = TIO.putStrLn $ "  [text] " <> t
    printBlock (ThinkingBlock tc) = TIO.putStrLn $ "  [thinking] " <> T.take 100 tc.thinkingText <> "..."
    printBlock (RedactedThinkingBlock _) = putStrLn "  [redacted thinking]"
    printBlock (ToolUseBlock tu) = TIO.putStrLn $ "  [tool_use] " <> tu.toolName
    printBlock (ToolResultBlock _) = putStrLn "  [tool_result]"
