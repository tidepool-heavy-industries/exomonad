{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Tidepool.Control.Effects.SshExec
  ( SshExec(..)
  , ExecRequest(..)
  , ExecResult(..)
  , execCommand
  , runSshExec
  ) where

import Control.Exception (try)
import Control.Monad.Freer (Eff, interpret, send, Member, LastMember, sendM)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text, unpack, pack)
import GHC.Generics (Generic)
import Network.HTTP.Client (HttpException, responseTimeoutMicro)
import Network.HTTP.Simple (httpJSON, setRequestBodyJSON, setRequestMethod, parseRequest_, getResponseBody, setRequestResponseTimeout)

-- | Low-level SSH command execution effect
data SshExec a where
  ExecCommand :: ExecRequest -> SshExec ExecResult

-- | Request to execute a command via ssh-proxy
data ExecRequest = ExecRequest
  { erContainer :: Text           -- ^ Container hostname (e.g., "agent-1")
  , erCommand :: Text             -- ^ Command to execute
  , erArgs :: [Text]              -- ^ Command arguments
  , erWorkingDir :: FilePath      -- ^ Working directory
  , erEnv :: [(Text, Text)]       -- ^ Environment variables
  , erTimeout :: Int              -- ^ Timeout in seconds
  } deriving (Show, Generic, ToJSON, FromJSON)

-- | Result of command execution
data ExecResult = ExecResult
  { exExitCode :: Int
  , exStdout :: Text
  , exStderr :: Text
  } deriving (Show, Generic, ToJSON, FromJSON)

-- | Send an ExecCommand effect
execCommand :: Member SshExec effs => ExecRequest -> Eff effs ExecResult
execCommand = send . ExecCommand

-- | Interpreter: calls ssh-proxy HTTP API
runSshExec :: LastMember IO effs => Text -> Eff (SshExec ': effs) a -> Eff effs a
runSshExec sshProxyUrl = interpret $ \case
  ExecCommand req -> sendM $ do
    let url = unpack $ sshProxyUrl <> "/exec"
    let timeoutMicros = req.erTimeout * 1000000 + 5000000 -- Command timeout + 5s buffer
    let request = setRequestResponseTimeout (responseTimeoutMicro timeoutMicros)
                $ setRequestMethod "POST"
                $ setRequestBodyJSON req
                $ parseRequest_ url
    
    result <- try (httpJSON request)
    case result of
      Left (e :: HttpException) -> pure $ ExecResult
        { exExitCode = -1
        , exStdout = ""
        , exStderr = "SSH Proxy connection failed: " <> pack (show e)
        }
      Right response -> pure $ getResponseBody response
