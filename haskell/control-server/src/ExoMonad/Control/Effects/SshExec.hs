{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module ExoMonad.Control.Effects.SshExec
  ( SshExec(..)
  , ExecRequest(..)
  , ExecResult(..)
  , execCommand
  , runSshExec
  ) where

import Control.Monad.Freer (Eff, interpret, send, Member, LastMember, sendM)
import Data.Aeson (FromJSON(..), ToJSON(..), withObject, object, (.:), (.:?), (.=))
import Data.Text (Text)
import qualified Data.Text as T

import GHC.Generics (Generic)

import ExoMonad.Control.Logging (Logger)
import ExoMonad.Control.Subprocess (runSubprocessJSON, SubprocessError(..))

-- | Low-level remote command execution effect (formerly via SSH, now via docker-ctl)
data SshExec a where
  ExecCommand :: ExecRequest -> SshExec ExecResult

-- | Request to execute a command in a remote container
data ExecRequest = ExecRequest
  { container :: Maybe Text     -- ^ Container hostname (e.g., "agent-1") or Nothing for local
  , command :: Text             -- ^ Command to execute
  , args :: [Text]              -- ^ Command arguments
  , workingDir :: FilePath      -- ^ Working directory
  , env :: [(Text, Text)]       -- ^ Environment variables
  , timeout :: Int              -- ^ Timeout in seconds (NOTE: not currently enforced)
  } deriving (Show, Generic)

instance ToJSON ExecRequest where
  toJSON r = object
    [ "container" .= r.container
    , "command" .= r.command
    , "args" .= r.args
    , "working_dir" .= r.workingDir
    , "env" .= r.env
    , "timeout" .= r.timeout
    ]

instance FromJSON ExecRequest where
  parseJSON = withObject "ExecRequest" $ \v -> ExecRequest
    <$> v .:? "container"
    <*> v .: "command"
    <*> v .: "args"
    <*> v .: "working_dir"
    <*> v .: "env"
    <*> v .: "timeout"

-- | Result of command execution
-- IMPORTANT: Field names must match docker-ctl exec output (Rust ExecResponse)
-- Expected JSON: {"exit_code": N | null, "stdout": "...", "stderr": "..."}
data ExecResult = ExecResult
  { exitCode :: Maybe Int  -- null when docker-ctl fails to parse output
  , stdout :: Text
  , stderr :: Text
  } deriving (Show, Generic)

instance ToJSON ExecResult where
  toJSON r = object
    [ "exit_code" .= r.exitCode
    , "stdout" .= r.stdout
    , "stderr" .= r.stderr
    ]

-- | Parse ExecResult from docker-ctl JSON output
-- If parsing fails, exitCode will be Nothing in the error handler in runSshExec
instance FromJSON ExecResult where
  parseJSON = withObject "ExecResult" $ \v -> ExecResult
    <$> v .:? "exit_code"  -- Optional: null or missing becomes Nothing
    <*> v .: "stdout"
    <*> v .: "stderr"

-- | Send an ExecCommand effect
execCommand :: Member SshExec effs => ExecRequest -> Eff effs ExecResult
execCommand = send . ExecCommand

-- | Interpreter: calls docker-ctl binary
runSshExec :: LastMember IO effs => Logger -> FilePath -> Eff (SshExec ': effs) a -> Eff effs a
runSshExec logger binPath = interpret $ \case
  ExecCommand req -> sendM $ do
    let containerArgs = case req.container of
          Just container -> [T.unpack container]
          Nothing -> ["--local"]
    let args = ["exec"]
             ++ containerArgs
             ++ ["--workdir", req.workingDir]
             ++ concatMap (\(k, v) -> ["--env", T.unpack $ k <> "=" <> v]) req.env
             ++ ["--"] ++ T.unpack req.command : map T.unpack req.args
    
    runSubprocessJSON logger "[SshExec]" binPath args >>= \case
      Right (res :: ExecResult) -> pure res
      Left err -> pure $ ExecResult
        { exitCode = Nothing  -- Parse error, no valid exit code
        , stdout = ""
        , stderr = "docker-ctl exec failure: " <> err.stderr
        }
