{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module ExoMonad.Control.Effects.Cabal
  ( runCabalRemote
  ) where

import Control.Monad (void)
import Control.Monad.Freer (Eff, Member, interpret)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import ExoMonad.Control.Effects.SshExec (SshExec, ExecRequest(..), ExecResult(..), execCommand)
import ExoMonad.Effects.Cabal (Cabal(..), CabalResult(..))

-- | Interpreter: uses SshExec to run cabal commands remotely
runCabalRemote :: Member SshExec effs => Maybe Text -> Eff (Cabal ': effs) a -> Eff effs a
runCabalRemote mContainer = interpret $ \case
  CabalBuild path -> do
    result <- execCommand $ ExecRequest
      { erContainer = mContainer
      , erCommand = "cabal"
      , erArgs = ["build", "all", "-v0"]
      , erWorkingDir = path
      , erEnv = []
      , erTimeout = 600
      }
    case exExitCode result of
      Nothing -> pure $ CabalInfraError { cieError = exStderr result }
      Just 0 -> pure CabalSuccess
      Just code -> pure $ CabalBuildFailure
        { cbfExitCode = code
        , cbfStderr = exStderr result
        , cbfStdout = exStdout result
        }

  CabalTest path -> do
    result <- execCommand $ ExecRequest
      { erContainer = mContainer
      , erCommand = "cabal"
      , erArgs = ["test", "--test-show-details=always", "-v0"]
      , erWorkingDir = path
      , erEnv = []
      , erTimeout = 600
      }
    let output = exStdout result <> exStderr result
    case exExitCode result of
      Nothing -> pure $ CabalInfraError { cieError = exStderr result }
      Just 0 -> pure $ CabalTestSuccess { ctsOutput = output }
      Just testCode ->
        -- Simple check if it's a build failure (could be improved)
        if "error:" `T.isInfixOf` exStderr result
          then pure $ CabalBuildFailure
            { cbfExitCode = testCode
            , cbfStderr = exStderr result
            , cbfStdout = exStdout result
            }
          else pure $ CabalTestFailure
            { ctfRawOutput = output
            }

  CabalClean path -> do
    void $ execCommand $ ExecRequest
      { erContainer = mContainer
      , erCommand = "cabal"
      , erArgs = ["clean"]
      , erWorkingDir = path
      , erEnv = []
      , erTimeout = 60
      }
    pure CabalSuccess
