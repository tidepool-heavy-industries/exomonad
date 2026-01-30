{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module ExoMonad.Control.Effects.SshExec
  ( SshExec (..),
    ExecRequest (..),
    ExecResult (..),
    execCommand,
    runSshExec,
  )
where

import Control.Monad.Freer (Eff, LastMember, Member, interpret, send, sendM)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.=))
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Control.Logging (Logger)
import ExoMonad.Control.Subprocess (SubprocessError (..), runSubprocessJSON)
import GHC.Generics (Generic)

-- | Low-level remote command execution effect (formerly via SSH, now via docker-ctl)
data SshExec a where
  ExecCommand :: ExecRequest -> SshExec ExecResult

-- | Request to execute a command in a remote container
data ExecRequest = ExecRequest
  { -- | Container hostname (e.g., "agent-1") or Nothing for local
    container :: Maybe Text,
    -- | Command to execute
    command :: Text,
    -- | Command arguments
    args :: [Text],
    -- | Working directory
    workingDir :: FilePath,
    -- | Environment variables
    env :: [(Text, Text)],
    -- | Timeout in seconds (NOTE: not currently enforced)
    timeout :: Int
  }
  deriving (Show, Generic)

-- | Result of command execution
-- IMPORTANT: Field names must match docker-ctl exec output (Rust ExecResponse)
-- Expected JSON: {"exit_code": N | null, "stdout": "...", "stderr": "..."}
data ExecResult = ExecResult
  { exitCode :: Maybe Int, -- null when docker-ctl fails to parse output
    stdout :: Text,
    stderr :: Text
  }
  deriving (Show, Generic)

instance ToJSON ExecResult where
  toJSON r =
    object
      [ "exit_code" .= r.exitCode,
        "stdout" .= r.stdout,
        "stderr" .= r.stderr
      ]

-- | Parse ExecResult from docker-ctl JSON output
-- If parsing fails, exitCode will be Nothing in the error handler in runSshExec
instance FromJSON ExecResult where
  parseJSON = withObject "ExecResult" $ \v ->
    ExecResult
      <$> v .:? "exit_code" -- Optional: null or missing becomes Nothing
      <*> v .: "stdout"
      <*> v .: "stderr"

-- | Send an ExecCommand effect
execCommand :: (Member SshExec effs) => ExecRequest -> Eff effs ExecResult
execCommand = send . ExecCommand

-- | Interpreter: calls docker-ctl binary
runSshExec :: (LastMember IO effs) => Logger -> FilePath -> Eff (SshExec ': effs) a -> Eff effs a
runSshExec logger binPath = interpret $ \case
  ExecCommand req -> sendM $ do
    let containerArgs = case req.container of
          Just container -> [T.unpack container]
          Nothing -> ["--local"]
    let args =
          ["exec"]
            ++ containerArgs
            ++ ["--workdir", req.workingDir]
            ++ concatMap (\(k, v) -> ["--env", T.unpack $ k <> "=" <> v]) req.env
            ++ ["--"]
            ++ T.unpack req.command
            : map T.unpack req.args

    runSubprocessJSON logger "[SshExec]" binPath args >>= \case
      Right (res :: ExecResult) -> pure res
      Left err ->
        pure $
          ExecResult
            { exitCode = Nothing, -- Parse error, no valid exit code
              stdout = "",
              stderr = "docker-ctl exec failure: " <> err.stderr
            }
