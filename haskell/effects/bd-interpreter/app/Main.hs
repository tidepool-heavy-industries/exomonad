-- | Urchin CLI - Context generation for Claude Code sessions.
--
-- Uses the Tidepool graph DSL with Git and BD effects for context gathering.
module Main where

import Data.Text.IO qualified as TIO
import Options.Applicative

import Tidepool.BD.Prime.Runner
  ( runUrchinPrime
  , runUrchinPrimeJSON
  )


-- | Output format for prime command.
data OutputFormat = FormatMarkdown | FormatJSON
  deriving (Show, Eq)

-- | Options for the prime subcommand.
data PrimeOptions = PrimeOptions
  { poFormat   :: OutputFormat
    -- ^ Output format (markdown or json)
  }
  deriving (Show)

-- | Top-level command.
data Command
  = CmdPrime PrimeOptions
  deriving (Show)


-- | Parse output format.
parseFormat :: ReadM OutputFormat
parseFormat = eitherReader $ \s ->
  case s of
    "markdown" -> Right FormatMarkdown
    "md"       -> Right FormatMarkdown
    "json"     -> Right FormatJSON
    _          -> Left $ "Unknown format: " ++ s ++ " (expected: markdown, json)"


-- | Parser for prime options.
primeOptionsParser :: Parser PrimeOptions
primeOptionsParser = PrimeOptions
  <$> option parseFormat
        ( long "format"
       <> short 'f'
       <> metavar "FORMAT"
       <> value FormatMarkdown
       <> help "Output format: markdown (default), json"
        )


-- | Parser for the prime subcommand.
primeCommand :: Mod CommandFields Command
primeCommand = command "prime" $
  info (CmdPrime <$> primeOptionsParser)
       (progDesc "Generate context for Claude Code session")


-- | Top-level parser.
commandParser :: Parser Command
commandParser = hsubparser primeCommand


-- | Full parser with helper.
opts :: ParserInfo Command
opts = info (commandParser <**> helper)
  ( fullDesc
 <> progDesc "Urchin - Worktree-aware context generator for Claude Code"
 <> header "urchin - context generation for agent sessions"
  )


-- | Run the prime command using graph-based runner.
runPrime :: PrimeOptions -> IO ()
runPrime options = do
  output <- case options.poFormat of
    FormatMarkdown -> runUrchinPrime
    FormatJSON     -> runUrchinPrimeJSON
  TIO.putStr output


-- | Main entry point.
main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    CmdPrime options -> runPrime options
