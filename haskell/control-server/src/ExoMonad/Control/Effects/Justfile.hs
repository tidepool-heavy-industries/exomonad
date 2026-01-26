{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module ExoMonad.Control.Effects.Justfile
  ( runJustfileRemote
  ) where

import Control.Monad.Freer (Eff, Member, interpret)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import ExoMonad.Control.Effects.SshExec (SshExec, ExecRequest(..), ExecResult(..), execCommand)
import ExoMonad.Effects.Justfile (Justfile(..), JustResult(..))

-- | Interpreter: uses SshExec to run just commands remotely
-- Takes a container name and a working directory (relative to container root).
runJustfileRemote :: Member SshExec effs => Text -> FilePath -> Eff (Justfile ': effs) a -> Eff effs a
runJustfileRemote container workDir = interpret $ \case
  RunRecipe recipe args -> do
    result <- execCommand $ ExecRequest
      { erContainer = container
      , erCommand = "just"
      , erArgs = recipe : args
      , erWorkingDir = workDir
      , erEnv = []
      , erTimeout = 300
      }
    pure $ JustResult
      { stdout = exStdout result
      , stderr = exStderr result
      , exitCode = fromMaybe (-1) (exExitCode result)
      }