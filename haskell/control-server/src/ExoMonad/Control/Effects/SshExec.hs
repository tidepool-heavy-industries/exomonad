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
import Data.Aeson (FromJSON(..), ToJSON(..), eitherDecode, withObject, object, (.:), (.:?), (.=))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import GHC.Generics (Generic)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

-- | Low-level remote command execution effect (formerly via SSH, now via docker-ctl)
data SshExec a where
  ExecCommand :: ExecRequest -> SshExec ExecResult

-- | Request to execute a command in a remote container
data ExecRequest = ExecRequest
  { erContainer :: Maybe Text     -- ^ Container hostname (e.g., "agent-1") or Nothing for local
  , erCommand :: Text             -- ^ Command to execute
  , erArgs :: [Text]              -- ^ Command arguments
  , erWorkingDir :: FilePath      -- ^ Working directory
  , erEnv :: [(Text, Text)]       -- ^ Environment variables
  , erTimeout :: Int              -- ^ Timeout in seconds (NOTE: not currently enforced)
  } deriving (Show, Generic)

instance ToJSON ExecRequest where
  toJSON r = object
    [ "container" .= erContainer r
    , "command" .= erCommand r
    , "args" .= erArgs r
    , "working_dir" .= erWorkingDir r
    , "env" .= erEnv r
    , "timeout" .= erTimeout r
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
  { exExitCode :: Maybe Int  -- null when docker-ctl fails to parse output
  , exStdout :: Text
  , exStderr :: Text
  } deriving (Show, Generic)

instance ToJSON ExecResult where
  toJSON r = object
    [ "exit_code" .= exExitCode r
    , "stdout" .= exStdout r
    , "stderr" .= exStderr r
    ]

-- | Parse ExecResult from docker-ctl JSON output
-- If parsing fails, exExitCode will be Nothing in the error handler in runSshExec
instance FromJSON ExecResult where
  parseJSON = withObject "ExecResult" $ \v -> ExecResult
    <$> v .:? "exit_code"  -- Optional: null or missing becomes Nothing
    <*> v .: "stdout"
    <*> v .: "stderr"

-- | Send an ExecCommand effect
execCommand :: Member SshExec effs => ExecRequest -> Eff effs ExecResult
execCommand = send . ExecCommand

-- | Interpreter: calls docker-ctl binary
runSshExec :: LastMember IO effs => FilePath -> Eff (SshExec ': effs) a -> Eff effs a
runSshExec binPath = interpret $ \case
  ExecCommand req -> sendM $ do
    let containerArgs = case req.erContainer of
          Just container -> [T.unpack container]
          Nothing -> ["--local"]
    let args = ["exec"]
             ++ containerArgs
             ++ ["--workdir", req.erWorkingDir]
             ++ concatMap (\(k, v) -> ["--env", T.unpack $ k <> "=" <> v]) req.erEnv
             ++ ["--"] ++ T.unpack req.erCommand : map T.unpack req.erArgs
    
    (code, stdout, stderr) <- readProcessWithExitCode binPath args ""
    case code of
      ExitSuccess -> case eitherDecode (BL.fromStrict $ TE.encodeUtf8 $ T.pack stdout) of
        Right (res :: ExecResult) -> pure res
        Left err -> pure $ ExecResult
          { exExitCode = Nothing  -- Parse error, no valid exit code
          , exStdout = ""
          , exStderr = "JSON parse error from docker-ctl: " <> T.pack err <> "\nOutput: " <> T.pack stdout
          }
      ExitFailure _ -> pure $ ExecResult
        { exExitCode = Nothing  -- docker-ctl itself failed
        , exStdout = ""
        , exStderr = "docker-ctl failed: " <> T.pack stderr
        }
