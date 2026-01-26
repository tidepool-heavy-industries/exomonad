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
import Data.Aeson (FromJSON(..), ToJSON(..), eitherDecode, genericParseJSON, genericToJSON)
import Data.Aeson.Casing (aesonPrefix, snakeCase)
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
  { erContainer :: Text           -- ^ Container hostname (e.g., "agent-1")
  , erCommand :: Text             -- ^ Command to execute
  , erArgs :: [Text]              -- ^ Command arguments
  , erWorkingDir :: FilePath      -- ^ Working directory
  , erEnv :: [(Text, Text)]       -- ^ Environment variables
  , erTimeout :: Int              -- ^ Timeout in seconds (NOTE: not currently enforced)
  } deriving (Show, Generic)

instance ToJSON ExecRequest where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON ExecRequest where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

-- | Result of command execution
-- Matches docker-ctl exec output: {"exit_code": ..., "stdout": ..., "stderr": ...}
data ExecResult = ExecResult
  { exExitCode :: Maybe Int  -- Changed to Maybe Int to match docker-ctl output
  , exStdout :: Text
  , exStderr :: Text
  } deriving (Show, Generic)

instance ToJSON ExecResult where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON ExecResult where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

-- | Send an ExecCommand effect
execCommand :: Member SshExec effs => ExecRequest -> Eff effs ExecResult
execCommand = send . ExecCommand

-- | Interpreter: calls docker-ctl binary
runSshExec :: LastMember IO effs => FilePath -> Eff (SshExec ': effs) a -> Eff effs a
runSshExec binPath = interpret $ \case
  ExecCommand req -> sendM $ do
    let args = ["exec", T.unpack req.erContainer]
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
