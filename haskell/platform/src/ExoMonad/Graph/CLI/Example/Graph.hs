{-# LANGUAGE TypeApplications #-}

-- | Example CLI handler with LLM effects.
--
-- This module shows how to create a simple CLI tool that calls the LLM API.
-- It uses the Anthropic client directly for simplicity.
module ExoMonad.Graph.CLI.Example.Graph
  ( -- * Handler
    summarize
  ) where

import Data.Aeson (fromJSON, Result(..))
import qualified Data.Text as T

import ExoMonad.Anthropic.Client
  ( ClientConfig(..)
  , SingleCallRequest(..)
  , SingleCallResponse(..)
  , callMessagesOnce
  , ContentBlock(..)
  , Message(..)
  , Role(..)
  )
import ExoMonad.Schema (schemaToValue, jsonSchema)

import ExoMonad.Graph.CLI.Example.Types (SummaryInput(..), SummaryOutput(..))

-- ════════════════════════════════════════════════════════════════════════════
-- HANDLER
-- ════════════════════════════════════════════════════════════════════════════

-- | Summarize text using Claude.
--
-- This handler:
--
-- 1. Constructs a prompt from the input
-- 2. Calls the Anthropic API with a JSON schema for structured output
-- 3. Returns the parsed SummaryOutput
--
-- @
-- main = do
--   apiKey <- getEnv "ANTHROPIC_API_KEY"
--   let config = ClientConfig apiKey "claude-sonnet-4-20250514" 2048
--   result <- summarize config (SummaryInput "Long text here..." 50)
--   print result
-- @
summarize :: ClientConfig -> SummaryInput -> IO SummaryOutput
summarize config input = do
  let systemPrompt = T.unlines
        [ "You are a text summarizer. Given text content, produce a concise summary."
        , "Your summary should be clear, accurate, and capture the key points."
        , "Aim to keep the summary under " <> T.pack (show input.maxWords) <> " words."
        , ""
        , "Return your response as JSON with fields:"
        , "- summary: The summarized text"
        , "- wordCount: The number of words in your summary"
        ]
      userPrompt = T.unlines
        [ "Please summarize the following text:"
        , ""
        , input.textToSummarize
        ]
      schema = schemaToValue (jsonSchema @SummaryOutput)
      request = SingleCallRequest
        { scrMessages = [Message User [TextBlock userPrompt]]
        , scrSystemPrompt = Just systemPrompt
        , scrOutputSchema = Just schema
        , scrTools = []
        , scrThinkingBudget = Nothing
        }

  result <- callMessagesOnce config request
  case result of
    Left err -> error $ "LLM API error: " <> show err
    Right resp -> do
      -- Extract JSON from response
      case extractOutput resp.scrContent of
        Nothing -> error "No JSON output in response"
        Just so -> pure so

-- | Extract SummaryOutput from response content blocks.
extractOutput :: [ContentBlock] -> Maybe SummaryOutput
extractOutput blocks =
  -- Look for JsonBlock first
  case [v | JsonBlock v <- blocks] of
    (v:_) -> case fromJSON v of
      Success so -> Just so
      Error _ -> Nothing
    [] -> Nothing
