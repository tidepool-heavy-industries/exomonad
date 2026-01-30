{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module ExoMonad.Control.Subprocess
  ( runSubprocess,
    runSubprocessJSON,
    runSubprocessText,
    SubprocessResult (..),
    SubprocessError (..),
  )
where

import Control.Exception (SomeException, try)
import Control.Monad (unless)
import Data.Aeson (FromJSON, eitherDecode)
import Data.ByteString.Lazy qualified as LBS
import Data.List (isInfixOf)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import ExoMonad.Control.Logging (Logger (..), logDebug, logError, logInfo)
import GHC.Generics (Generic)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

data SubprocessError = SubprocessError
  { command :: String,
    args :: [String],
    exitCode :: Int,
    stderr :: Text
  }
  deriving (Show, Eq, Generic)

data SubprocessResult
  = SubprocessSuccess {stdout :: Text, stderr :: Text}
  | SubprocessFailure SubprocessError
  deriving (Show, Eq, Generic)

-- | Core HOF: run a subprocess with exception handling, exit code checking, and logging.
runSubprocess ::
  Logger -> -- structured logger
  Text -> -- component name for log prefix (e.g., "[Git]", "[DockerCtl]")
  String -> -- command
  [String] -> -- arguments
  IO SubprocessResult
runSubprocess logger component cmd args = do
  let redactedArgs = redactArgs args
  logInfo logger $ component <> " Executing: " <> T.pack cmd <> " " <> T.pack (unwords redactedArgs)
  result <- try @SomeException $ readProcessWithExitCode cmd args ""
  case result of
    Left e -> do
      let err = SubprocessError cmd redactedArgs (-1) (T.pack $ show e)
      logError logger $ component <> " Exception: " <> T.pack (show e)
      pure $ SubprocessFailure err
    Right (ExitSuccess, out, err) -> do
      logInfo logger $ component <> " Success (stdout=" <> T.pack (show (length out)) <> " bytes)"
      unless (null err) $
        logDebug logger $
          component <> " stderr: " <> T.pack (take 500 err)
      pure $ SubprocessSuccess (T.pack out) (T.pack err)
    Right (ExitFailure code, _out, err) -> do
      let serr = SubprocessError cmd redactedArgs code (T.pack err)
      logError logger $ component <> " Failed (exit=" <> T.pack (show code) <> "): " <> T.pack (take 500 err)
      pure $ SubprocessFailure serr

-- | Redact sensitive arguments (env vars).
redactArgs :: [String] -> [String]
redactArgs [] = []
redactArgs ("-e" : kv : rest) = "-e" : redactKV kv : redactArgs rest
redactArgs ("--env" : kv : rest) = "--env" : redactKV kv : redactArgs rest
redactArgs (x : xs) = x : redactArgs xs

redactKV :: String -> String
redactKV s = case break (== '=') s of
  (key, '=' : _) | isSensitiveKey key -> key <> "=[REDACTED]"
  _ -> s

isSensitiveKey :: String -> Bool
isSensitiveKey k = any (`isInfixOf` k) ["TOKEN", "KEY", "SECRET", "PASSWORD", "AUTH"]

-- | Run subprocess and parse JSON stdout on success.
runSubprocessJSON ::
  (FromJSON a) =>
  Logger ->
  Text ->
  String ->
  [String] ->
  IO (Either SubprocessError a)
runSubprocessJSON logger component cmd args = do
  res <- runSubprocess logger component cmd args
  case res of
    SubprocessFailure e -> pure $ Left e
    SubprocessSuccess out _ -> case eitherDecode (LBS.fromStrict $ T.encodeUtf8 out) of
      Right a -> pure $ Right a
      Left err -> pure $ Left $ SubprocessError cmd args 0 ("JSON parse error: " <> T.pack err)

-- | Run subprocess and return trimmed stdout text on success.
runSubprocessText ::
  Logger ->
  Text ->
  String ->
  [String] ->
  IO (Either SubprocessError Text)
runSubprocessText logger component cmd args = do
  res <- runSubprocess logger component cmd args
  case res of
    SubprocessFailure e -> pure $ Left e
    SubprocessSuccess out _ -> pure $ Right (T.strip out)
