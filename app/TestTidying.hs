{-# LANGUAGE OverloadedStrings #-}
-- | Test the tidying agent flow
module Main where

import System.Environment (getEnv)
import Data.Text qualified as T

import Tidying (runTidyingSession, newSession, TidyingEvent(..), SessionState(..))
import Tidepool.Effect (LLMConfig(..))

main :: IO ()
main = do
  putStrLn "=== Tidying Agent Test ==="
  putStrLn "Type 'done', 'quit', or 'exit' to end session"
  putStrLn ""

  apiKey <- T.pack <$> getEnv "ANTHROPIC_API_KEY"
  let llmConfig = LLMConfig
        { llmApiKey = apiKey
        , llmModel = "claude-sonnet-4-20250514"
        , llmMaxTokens = 4096
        , llmThinkingBudget = Nothing
        }

  finalState <- runTidyingSession newSession handleEvent llmConfig

  putStrLn ""
  putStrLn "=== Session Complete ==="
  let SessionState { itemsProcessed = processed } = finalState
  putStrLn $ "Items processed: " <> show processed
  where
    handleEvent :: TidyingEvent -> IO ()
    handleEvent (SituationClassified msg) = putStrLn $ "[EVENT] " <> T.unpack msg
    handleEvent (PhotoAnalyzed msg) = putStrLn $ "[PHOTO] " <> T.unpack msg
    handleEvent (ActionTaken action) = putStrLn $ "[ACTION] " <> show action
    handleEvent (PhaseChanged from to) = putStrLn $ "[PHASE] " <> show from <> " -> " <> show to
    handleEvent (SessionEnded items) = putStrLn $ "[END] Processed " <> show items <> " items"
