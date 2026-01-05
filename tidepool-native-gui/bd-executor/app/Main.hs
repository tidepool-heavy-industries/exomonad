-- | Urchin CLI - Context generation for Claude Code sessions.
module Main where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Options.Applicative
import System.Exit (exitFailure)

import Tidepool.BD.Prime (gatherContext, gatherContextFromCwd, renderPrime, renderPrimeJSON)
import Tidepool.BD.Prime.Worktree (detectWorktree, WorktreeInfo(..))
import Tidepool.BD.Executor (BDConfig(..), defaultBDConfig)


-- | Output format for prime command.
data OutputFormat = FormatMarkdown | FormatJSON
  deriving (Show, Eq)

-- | Options for the prime subcommand.
data PrimeOptions = PrimeOptions
  { poWorktree :: Maybe Text
    -- ^ Override worktree detection
  , poFormat   :: OutputFormat
    -- ^ Output format (markdown or json)
  , poEpic     :: Maybe Text
    -- ^ Focus on specific epic (TODO: not yet implemented)
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
  <$> optional (strOption
        ( long "worktree"
       <> short 'w'
       <> metavar "NAME"
       <> help "Override worktree detection (e.g., 'bd', 'native-server')"
        ))
  <*> option parseFormat
        ( long "format"
       <> short 'f'
       <> metavar "FORMAT"
       <> value FormatMarkdown
       <> help "Output format: markdown (default), json"
        )
  <*> optional (strOption
        ( long "epic"
       <> short 'e'
       <> metavar "ID"
       <> help "Focus on specific epic's children (e.g., 'tidepool-6qa')"
        ))


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


-- | Run the prime command.
runPrime :: PrimeOptions -> IO ()
runPrime options = do
  -- Detect or use explicit worktree
  mWorktree <- case options.poWorktree of
    Nothing -> detectWorktree
    Just name -> do
      -- If explicit name given, still detect but could override
      -- For now, just detect and warn if mismatch
      mDetected <- detectWorktree
      case mDetected of
        Nothing -> do
          TIO.putStrLn $ "Warning: Not in a git repository, using name: " <> name
          pure Nothing
        Just wt ->
          if wt.wiName == name
            then pure (Just wt)
            else do
              TIO.putStrLn $ "Warning: Detected worktree '" <> wt.wiName
                          <> "' but --worktree=" <> name <> " specified"
              pure (Just wt)

  case mWorktree of
    Nothing -> do
      TIO.putStrLn "Error: Not in a git repository"
      exitFailure
    Just worktree -> do
      -- Gather context
      ctx <- gatherContext defaultBDConfig worktree

      -- Render output
      let output = case options.poFormat of
            FormatMarkdown -> renderPrime ctx
            FormatJSON     -> renderPrimeJSON ctx

      TIO.putStr output


-- | Main entry point.
main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    CmdPrime options -> runPrime options
