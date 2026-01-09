{-# LANGUAGE TemplateHaskell #-}

-- | Summarizer CLI - Example CLI tool using LLM effects.
--
-- This demonstrates the full pipeline:
--
-- 1. Parse CLI args via deriveCLIParser (TH-generated from Haddock docs)
-- 2. Call Anthropic API with structured output
-- 3. Format result as JSON or text
--
-- = Usage
--
-- @
-- # Set API key
-- export ANTHROPIC_API_KEY=sk-ant-...
--
-- # Run with text input
-- cabal run summarizer-cli -- --text-to-summarize "Your long text here..." --max-words 50
--
-- # Get JSON output
-- cabal run summarizer-cli -- --text-to-summarize "Your text..." --max-words 50 --format json
-- @
module Main (main) where

import qualified Data.Text as T
import System.Environment (getEnv)

import Tidepool.Graph.CLI (deriveCLIParser)
import Tidepool.Graph.CLI.Runner (runCLIWithLLM, makeClientConfig)
import Tidepool.Graph.CLI.Example.Types (SummaryInput(..))
import Tidepool.Graph.CLI.Example.Graph (summarize)

main :: IO ()
main = do
  -- Run the CLI with TH-derived parser
  -- API key is fetched inside the handler, after args are parsed
  runCLIWithLLM
    "Summarize text using Claude"
    $(deriveCLIParser ''SummaryInput)
    (\input -> do
      -- Get API key from environment (only after args parsed)
      apiKey <- T.pack <$> getEnv "ANTHROPIC_API_KEY"
      let config = makeClientConfig apiKey
      summarize config input)
