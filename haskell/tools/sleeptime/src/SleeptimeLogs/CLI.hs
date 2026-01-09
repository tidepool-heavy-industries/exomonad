-- | Command-line interface parser for sleeptime-logs.
--
-- Provides commands for querying Loki logs:
--   transitions - Graph transitions
--   llm-calls   - LLM API calls with latency info
--   errors      - Error events
--   session     - All events for a session
module SleeptimeLogs.CLI
  ( Command(..)
  , Options(..)
  , Duration(..)
  , parseOptions
  , parseDuration
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative
import Data.Time.Clock (NominalDiffTime)

-- | Time duration for --since flags.
data Duration = Duration
  { durationSeconds :: NominalDiffTime
  , durationText :: Text
  }
  deriving (Show)

-- | Parse a duration string like "24h", "1h", "30m".
parseDuration :: Text -> Either String Duration
parseDuration t =
  case T.unsnoc t of
    Just (numT, unit) ->
      case (reads (T.unpack numT), unit) of
        ([(n, "")], 'h') -> Right $ Duration (fromIntegral (n :: Int) * 3600) t
        ([(n, "")], 'm') -> Right $ Duration (fromIntegral (n :: Int) * 60) t
        ([(n, "")], 's') -> Right $ Duration (fromIntegral (n :: Int)) t
        ([(n, "")], 'd') -> Right $ Duration (fromIntegral (n :: Int) * 86400) t
        _ -> Left $ "Invalid duration: " <> T.unpack t
    Nothing -> Left $ "Empty duration"

-- | Duration argument reader for optparse.
durationReader :: ReadM Duration
durationReader = eitherReader $ \s ->
  case parseDuration (T.pack s) of
    Left err -> Left err
    Right d -> Right d

-- | CLI commands.
data Command
  = Transitions
      { since :: Duration
      , limit :: Maybe Int
      }
  | LLMCalls
      { minLatency :: Maybe Int
      , since :: Duration
      , limit :: Maybe Int
      }
  | Errors
      { since :: Duration
      , limit :: Maybe Int
      }
  | Session
      { sessionId :: Text
      , limit :: Maybe Int
      }
  deriving (Show)

-- | Global options.
data Options = Options
  { optCommand :: Command
  , optJson :: Bool
  , optVerbose :: Bool
  }
  deriving (Show)

-- | Parse CLI options.
parseOptions :: IO Options
parseOptions = execParser opts
  where
    opts = info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc "Query Grafana/Loki logs for sleeptime evolution analysis"
     <> header "sleeptime-logs - Tidepool observability CLI"
      )

optionsParser :: Parser Options
optionsParser = Options
  <$> commandParser
  <*> switch
      ( long "json"
     <> short 'j'
     <> help "Output as JSON (for piping to jq)"
      )
  <*> switch
      ( long "verbose"
     <> short 'v'
     <> help "Verbose output"
      )

commandParser :: Parser Command
commandParser = hsubparser
  ( command "transitions" (info transitionsParser (progDesc "Fetch graph transitions"))
 <> command "llm-calls" (info llmCallsParser (progDesc "Fetch LLM calls with latency info"))
 <> command "errors" (info errorsParser (progDesc "Fetch error events"))
 <> command "session" (info sessionParser (progDesc "Fetch all events for a session"))
  )

sinceOption :: Parser Duration
sinceOption = option durationReader
  ( long "since"
 <> short 's'
 <> metavar "DURATION"
 <> help "Time range (e.g., 24h, 1h, 30m)"
 <> value (Duration 3600 "1h")
 <> showDefaultWith (\d -> T.unpack d.durationText)
  )

limitOption :: Parser (Maybe Int)
limitOption = optional $ option auto
  ( long "limit"
 <> short 'n'
 <> metavar "N"
 <> help "Maximum number of results"
  )

transitionsParser :: Parser Command
transitionsParser = Transitions
  <$> sinceOption
  <*> limitOption

llmCallsParser :: Parser Command
llmCallsParser = LLMCalls
  <$> optional (option auto
      ( long "min-latency"
     <> metavar "MS"
     <> help "Filter by minimum latency in milliseconds"
      ))
  <*> sinceOption
  <*> limitOption

errorsParser :: Parser Command
errorsParser = Errors
  <$> sinceOption
  <*> limitOption

sessionParser :: Parser Command
sessionParser = Session
  <$> argument (T.pack <$> str)
      ( metavar "SESSION-ID"
     <> help "Session ID to query"
      )
  <*> limitOption
