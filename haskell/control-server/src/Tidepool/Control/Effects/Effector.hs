{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Tidepool.Control.Effects.Effector
  ( runEffectorViaSsh
  , runEffectorIO
  ) where

import Control.Monad.Freer (Eff, Member, LastMember, interpret, sendM)
import Data.Text (Text)
import qualified Data.Text as T
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

import Tidepool.Control.Effects.SshExec (SshExec, ExecRequest(..), ExecResult(..), execCommand)
import Tidepool.Effects.Effector (Effector(..))

-- | Interpreter: uses SshExec to run effector commands remotely
runEffectorViaSsh :: Member SshExec effs => Text -> FilePath -> Eff (Effector ': effs) a -> Eff effs a
runEffectorViaSsh container workingDir = interpret $ \case
  RunEffector cmd args -> do
    result <- execCommand $ ExecRequest
      { erContainer = container
      , erCommand = "effector"
      , erArgs = cmd : args
      , erWorkingDir = workingDir
      , erEnv = []
      , erTimeout = 60
      }
    -- Return combined stdout and stderr for now, similar to how it would be used
    pure $ exStdout result <> exStderr result

-- | Interpreter: runs effector commands locally
runEffectorIO :: LastMember IO effs => FilePath -> Eff (Effector ': effs) a -> Eff effs a
runEffectorIO workingDir = interpret $ \case
  RunEffector cmd args -> do
    (code, stdout, stderr) <- sendM $ readProcessWithExitCode "effector" (T.unpack cmd : map T.unpack args) ""
    -- We ignore exit code for now because Effector results are often parsed from JSON in stdout even on failure
    pure $ T.pack stdout <> T.pack stderr
