-- | CLI runner with LLM effects.
--
-- This module provides a simple way to build CLI tools that use the LLM API.
-- It uses the Anthropic client directly for simplicity.
--
-- Note: This does NOT use the freer-simple based graph framework from exomonad-core.
-- The graph execution machinery hasn't been wired to effect runners yet.
-- Instead, this provides direct IO-based execution.
--
-- = Usage
--
-- @
-- main :: IO ()
-- main = do
--   apiKey <- getEnv "ANTHROPIC_API_KEY"
--   let config = makeClientConfig (T.pack apiKey)
--   runCLIWithLLM
--     "Summarize text using Claude"
--     $(deriveCLIParser ''SummaryInput)
--     (summarize config)
-- @
module ExoMonad.Graph.CLI.Runner
  ( -- * CLI Runner
    runCLIWithLLM
    -- * Client Config Helpers
  , makeClientConfig
  ) where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Options.Applicative (Parser, execParser, info, helper, fullDesc, progDesc, (<**>))
import qualified Data.Text as T

import ExoMonad.Anthropic.Client (ClientConfig(..))
import ExoMonad.Graph.CLI (outputFormatParser, formatOutput)

-- ════════════════════════════════════════════════════════════════════════════
-- CLI RUNNER
-- ════════════════════════════════════════════════════════════════════════════

-- | Run a CLI application with LLM support.
--
-- This is a simple wrapper that:
--
-- 1. Parses CLI arguments using the provided parser
-- 2. Adds @--format@ flag for output formatting
-- 3. Runs the provided handler with the parsed input
-- 4. Formats and prints the output
--
-- The handler is responsible for calling the LLM API.
--
-- = Example
--
-- @
-- main :: IO ()
-- main = do
--   apiKey <- getEnv "ANTHROPIC_API_KEY"
--   let config = makeClientConfig (T.pack apiKey)
--   runCLIWithLLM
--     "Summarize text using Claude"
--     $(deriveCLIParser ''SummaryInput)
--     (summarize config)
-- @
runCLIWithLLM
  :: (Show output, ToJSON output)
  => Text                     -- ^ Description for --help
  -> Parser input             -- ^ Input parser (use deriveCLIParser)
  -> (input -> IO output)     -- ^ Handler that calls LLM API
  -> IO ()
runCLIWithLLM desc parser handler = do
  let combinedParser = (,) <$> parser <*> outputFormatParser
      parserInfo' = info (combinedParser <**> helper)
        ( fullDesc
        <> progDesc (T.unpack desc)
        )
  (input, fmt) <- execParser parserInfo'
  result <- handler input
  TIO.putStrLn (formatOutput fmt result)

-- ════════════════════════════════════════════════════════════════════════════
-- CLIENT CONFIG HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Create a default ClientConfig for CLI tools.
--
-- Uses Claude Sonnet 4 with sensible defaults:
--
-- * Model: claude-sonnet-4-20250514
-- * Max tokens: 2048
--
-- @
-- config = makeClientConfig "sk-ant-..."
-- @
makeClientConfig :: Text -> ClientConfig
makeClientConfig apiKey = ClientConfig
  { apiKey = apiKey
  , defaultModel = "claude-sonnet-4-20250514"
  , defaultMaxTokens = 2048
  }
