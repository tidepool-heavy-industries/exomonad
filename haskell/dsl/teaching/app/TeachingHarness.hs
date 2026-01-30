{-# LANGUAGE OverloadedStrings #-}

-- | Test harness for teaching infrastructure.
--
-- This standalone executable tests the teaching pipeline without needing
-- the full native server. It:
--
-- 1. Loads TeachingConfig from environment
-- 2. Initializes TeachingEnv with session directory
-- 3. Sends a test LLM call through runLLMWithTeaching
-- 4. Verifies output in anthropic.jsonl
--
-- = Usage
--
-- @
-- TEACHING_ENABLED=true \
-- ANTHROPIC_API_KEY=sk-ant-... \
-- TEACHING_OUTPUT_DIR=./test-training \
-- cabal run teaching-harness
-- @
--
-- = Expected Output
--
-- @
-- [Teaching] Loading config...
-- [Teaching] Session ID: <uuid>
-- [Teaching] Output dir: ./test-training/session-<uuid>
-- [Teaching] RunTurnOp intercepted for node: test-node in graph: TestGraph
-- [Teaching] Calling Haiku...
-- [Teaching] Recorded turn for: test-node
-- [Teaching] Success! Check ./test-training/session-<uuid>/anthropic.jsonl
-- @
module Main where

import Control.Monad.Freer (Eff, Member, runM, send)
import Data.Aeson (Value, object, (.=))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Effect.NodeMeta (NodeMetadata (..))
import ExoMonad.Effect.Types
  ( ContentBlock (..),
    LLM (..),
    TurnOutcome (..),
    TurnResult (..),
  )
import ExoMonad.Teaching.LLM
  ( TeachingConfig (..),
    loadTeachingConfig,
    runLLMWithTeaching,
    withTeaching,
  )
import System.Directory (doesFileExist)
import System.Exit (exitFailure)

main :: IO ()
main = do
  putStrLn "[Teaching Harness] Loading config from environment..."

  mConfig <- loadTeachingConfig
  case mConfig of
    Nothing -> do
      putStrLn "[Teaching Harness] ERROR: Teaching not enabled or ANTHROPIC_API_KEY not set"
      putStrLn ""
      putStrLn "Set environment variables:"
      putStrLn "  TEACHING_ENABLED=true"
      putStrLn "  ANTHROPIC_API_KEY=sk-ant-..."
      putStrLn "  TEACHING_OUTPUT_DIR=./test-training  (optional)"
      exitFailure
    Just config -> do
      putStrLn $ "[Teaching Harness] Session ID: " <> show (tcSessionId config)
      putStrLn $ "[Teaching Harness] Output dir: " <> tcOutputDir config

      -- Use guidance that helps Haiku produce quality training data
      let guidance =
            T.unlines
              [ "You are generating training data for a small function-calling model.",
                "Focus on:",
                "1. Clear, step-by-step reasoning in <thinking> blocks",
                "2. Precise tool argument formatting",
                "3. Explicit connections between reasoning and tool choice"
              ]

      withTeaching config guidance $ \env -> do
        putStrLn "[Teaching Harness] Environment initialized"
        putStrLn "[Teaching Harness] Running test LLM call..."

        -- Run a simple LLM turn through the teaching interpreter
        result <- runM $ runLLMWithTeaching env testLLMCall

        -- Report results
        case result of
          TurnCompleted (TurnResult {trNarrative = narrative, trThinking = thinking}) -> do
            putStrLn "[Teaching Harness] LLM call completed!"
            putStrLn $ "[Teaching Harness] Narrative: " <> T.unpack narrative
            let thinkingPreview = T.take 200 thinking
            if T.null thinking
              then putStrLn "[Teaching Harness] Thinking: (empty - this may indicate thinking is disabled or not returned)"
              else putStrLn $ "[Teaching Harness] Thinking preview: " <> T.unpack thinkingPreview <> "..."
          TurnBroken reason -> do
            putStrLn $ "[Teaching Harness] ERROR: Turn broken: " <> T.unpack reason
            exitFailure
          TurnTransitionHint target _payload -> do
            putStrLn $ "[Teaching Harness] Unexpected transition hint to: " <> T.unpack target
            exitFailure

        -- Verify output file exists
        let sessionDir = tcOutputDir config <> "/session-" <> show (tcSessionId config)
        let outputFile = sessionDir <> "/anthropic.jsonl"
        exists <- doesFileExist outputFile
        if exists
          then do
            putStrLn ""
            putStrLn "[Teaching Harness] SUCCESS! Training data written to:"
            putStrLn $ "  " <> outputFile
            putStrLn ""
            putStrLn "Verify with:"
            putStrLn $ "  cat " <> outputFile <> " | jq ."
            putStrLn $ "  cat " <> outputFile <> " | jq '.ttResponse.content[] | select(.type==\"thinking\")'"
          else do
            putStrLn $ "[Teaching Harness] WARNING: Output file not found: " <> outputFile
            putStrLn "This may indicate a recording issue."

-- | Test LLM call that exercises the teaching pipeline.
--
-- Sends a simple RunTurnOp with:
-- - Test node metadata
-- - Simple system prompt
-- - User content asking for classification
-- - Basic output schema
testLLMCall :: (Member LLM effs) => Eff effs (TurnOutcome (TurnResult Value))
testLLMCall =
  send $
    RunTurnOp
      testMeta
      testSystemPrompt
      (Text {text = testUserContent} :| [])
      testSchema
      [] -- No tools for this simple test
  where
    testMeta =
      NodeMetadata
        { nmNodeName = "test-node",
          nmGraphName = "TestGraph"
        }

    testSystemPrompt =
      T.unlines
        [ "You are a helpful assistant that classifies user intents.",
          "",
          "Given a user message, classify it as one of:",
          "- QUESTION: User is asking a question",
          "- STATEMENT: User is making a statement",
          "- REQUEST: User is making a request",
          "",
          "Respond with a JSON object containing:",
          "- intent: The classified intent",
          "- confidence: A number 0-1 indicating confidence",
          "- reasoning: Brief explanation of classification"
        ]

    testUserContent =
      "Can you help me understand how the teaching infrastructure works?"

    testSchema :: Value
    testSchema =
      object
        [ "type" .= ("object" :: Text),
          "properties"
            .= object
              [ "intent"
                  .= object
                    [ "type" .= ("string" :: Text),
                      "enum" .= (["QUESTION", "STATEMENT", "REQUEST"] :: [Text])
                    ],
                "confidence"
                  .= object
                    [ "type" .= ("number" :: Text),
                      "minimum" .= (0 :: Int),
                      "maximum" .= (1 :: Int)
                    ],
                "reasoning" .= object ["type" .= ("string" :: Text)]
              ],
          "required" .= (["intent", "confidence", "reasoning"] :: [Text])
        ]
