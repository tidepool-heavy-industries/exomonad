{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Tidepool.Control.Effects.Justfile
  ( runJustfileViaSsh
  ) where

import Control.Monad.Freer (Eff, Member, interpret)
import Data.Text (Text)

import Tidepool.Control.Effects.SshExec (SshExec, ExecRequest(..), ExecResult(..), execCommand)
import Tidepool.Effects.Justfile (Justfile(..), JustResult(..))

-- | Interpreter: uses SshExec to run just commands
runJustfileViaSsh :: Member SshExec effs => Text -> Eff (Justfile ': effs) a -> Eff effs a
runJustfileViaSsh container = interpret $ \case
  RunRecipe recipe args -> do
    result <- execCommand $ ExecRequest
      { erContainer = container
      , erCommand = "just"
      , erArgs = recipe : args
      , erWorkingDir = "."
      , erEnv = []
      , erTimeout = 300
      }
    pure $ JustResult
      { stdout = exStdout result
      , stderr = exStderr result
      , exitCode = exExitCode result
      }
