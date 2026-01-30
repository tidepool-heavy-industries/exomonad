{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module ExoMonad.Control.Effects.Cabal
  ( Cabal (..),
    CabalResult (..),
    runCabalRemote,
  )
where

import Control.Monad (void)
import Control.Monad.Freer (Eff, Member, interpret)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Control.Effects.SshExec (ExecRequest (..), ExecResult (..), SshExec, execCommand)
import ExoMonad.Effects.Cabal (Cabal (..), CabalResult (..))

-- | Interpreter: uses SshExec to run cabal commands remotely
runCabalRemote :: (Member SshExec effs) => Maybe Text -> Eff (Cabal ': effs) a -> Eff effs a
runCabalRemote mContainer = interpret $ \case
  CabalBuild path -> do
    result <-
      execCommand $
        ExecRequest
          { container = mContainer,
            command = "cabal",
            args = ["build", "all", "-v0"],
            workingDir = path,
            env = [],
            timeout = 600
          }
    case result.exitCode of
      Nothing -> pure $ CabalInfraError {cieError = result.stderr}
      Just 0 -> pure CabalSuccess
      Just code ->
        pure $
          CabalBuildFailure
            { cbfExitCode = code,
              cbfStderr = result.stderr,
              cbfStdout = result.stdout
            }
  CabalTest path -> do
    result <-
      execCommand $
        ExecRequest
          { container = mContainer,
            command = "cabal",
            args = ["test", "--test-show-details=always", "-v0"],
            workingDir = path,
            env = [],
            timeout = 600
          }
    let output = result.stdout <> result.stderr
    case result.exitCode of
      Nothing -> pure $ CabalInfraError {cieError = result.stderr}
      Just 0 -> pure $ CabalTestSuccess {ctsOutput = output}
      Just testCode ->
        -- Simple check if it's a build failure (could be improved)
        if "error:" `T.isInfixOf` result.stderr
          then
            pure $
              CabalBuildFailure
                { cbfExitCode = testCode,
                  cbfStderr = result.stderr,
                  cbfStdout = result.stdout
                }
          else
            pure $
              CabalTestFailure
                { ctfRawOutput = output
                }
  CabalClean path -> do
    void $
      execCommand $
        ExecRequest
          { container = mContainer,
            command = "cabal",
            args = ["clean"],
            workingDir = path,
            env = [],
            timeout = 60
          }
    pure CabalSuccess
