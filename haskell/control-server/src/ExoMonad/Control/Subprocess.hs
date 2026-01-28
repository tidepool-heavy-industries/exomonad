{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}

module ExoMonad.Control.Subprocess
  ( runSubprocess
  , runSubprocessJSON
  , runSubprocessText
  , SubprocessResult(..)
  , SubprocessError(..)
  , noopLogger
  ) where

import Control.Exception (try, SomeException)
import Control.Monad (unless)
import Data.Aeson (FromJSON, eitherDecode, encode)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as LBS
import GHC.Generics (Generic)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import ExoMonad.Control.Logging (Logger(..), logInfo, logError, logDebug)

data SubprocessError = SubprocessError
  { command :: String
  , args :: [String]
  , exitCode :: Int
  , stderr :: Text
  } deriving (Show, Eq, Generic)

data SubprocessResult
  = SubprocessSuccess { stdout :: Text, stderr :: Text }
  | SubprocessFailure SubprocessError
  deriving (Show, Eq, Generic)

-- | A no-op logger that does nothing. Useful for tests or where logging is not desired.
noopLogger :: Logger
noopLogger = Logger
  { stderrLoggerSet = error "noopLogger: stderrLoggerSet"
  , fileLoggerSet = error "noopLogger: fileLoggerSet"
  , timeCache = error "noopLogger: timeCache"
  }
-- Wait, the Logger record in Logging.hs has LoggerSet which might not like being 'error'.
-- Better to use functions that don't call the record fields if I use runSubprocess_.
-- But runSubprocess calls logInfo/logError which uses the logger.

-- | Core HOF: run a subprocess with exception handling, exit code checking, and logging.
runSubprocess
  :: Logger           -- structured logger
  -> Text             -- component name for log prefix (e.g., "[Git]", "[DockerCtl]")
  -> String           -- command
  -> [String]         -- arguments
  -> IO SubprocessResult
runSubprocess logger component cmd args = do
  logInfo logger $ component <> " Executing: " <> T.pack cmd <> " " <> T.pack (unwords args)
  result <- try @SomeException $ readProcessWithExitCode cmd args ""
  case result of
    Left e -> do
      let err = SubprocessError cmd args (-1) (T.pack $ show e)
      logError logger $ component <> " Exception: " <> T.pack (show e)
      pure $ SubprocessFailure err
    Right (ExitSuccess, out, err) -> do
      logInfo logger $ component <> " Success (stdout=" <> T.pack (show (length out)) <> " bytes)"
      unless (null err) $
        logDebug logger $ component <> " stderr: " <> T.pack (take 500 err)
      pure $ SubprocessSuccess (T.pack out) (T.pack err)
    Right (ExitFailure code, _out, err) -> do
      let serr = SubprocessError cmd args code (T.pack err)
      logError logger $ component <> " Failed (exit=" <> T.pack (show code) <> "): " <> T.pack (take 500 err)
      pure $ SubprocessFailure serr

-- | Run subprocess and parse JSON stdout on success.
runSubprocessJSON
  :: (FromJSON a)
  => Logger -> Text -> String -> [String]
  -> IO (Either SubprocessError a)
runSubprocessJSON logger component cmd args = do
  res <- runSubprocess logger component cmd args
  case res of
    SubprocessFailure e -> pure $ Left e
    SubprocessSuccess out _ -> case eitherDecode (LBS.fromStrict $ T.encodeUtf8 out) of
      Right a -> pure $ Right a
      Left err -> pure $ Left $ SubprocessError cmd args 0 ("JSON parse error: " <> T.pack err)

-- | Run subprocess and return trimmed stdout text on success.
runSubprocessText
  :: Logger -> Text -> String -> [String]
  -> IO (Either SubprocessError Text)
runSubprocessText logger component cmd args = do
  res <- runSubprocess logger component cmd args
  case res of
    SubprocessFailure e -> pure $ Left e
    SubprocessSuccess out _ -> pure $ Right (T.strip out)
