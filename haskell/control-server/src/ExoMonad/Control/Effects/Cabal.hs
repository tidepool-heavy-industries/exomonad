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
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Regex.TDFA ((=~))

import ExoMonad.Control.Effects.SshExec (SshExec, ExecRequest(..), ExecResult(..), execCommand)
import ExoMonad.Effects.Cabal (Cabal(..), CabalResult(..), RawCompileError(..))

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
    let code = fromMaybe (-1) (exExitCode result)
    pure $ case code of
      0 -> CabalSuccess
      _ -> CabalBuildFailure
        { cbfExitCode = code
        , cbfStderr = exStderr result
        , cbfStdout = exStdout result
        , cbfParsedErrors = parseGhcErrors (exStderr result)
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
        testCode = fromMaybe (-1) (exExitCode result)
    pure $ case testCode of
      0 -> CabalTestSuccess { ctsOutput = output }
      _ ->
        -- Simple check if it's a build failure (could be improved)
        if "error:" `T.isInfixOf` exStderr result
          then CabalBuildFailure
            { cbfExitCode = testCode
            , cbfStderr = exStderr result
            , cbfStdout = exStdout result
            , cbfParsedErrors = parseGhcErrors (exStderr result)
            }
          else CabalTestFailure
            { ctfParsedFailures = [] -- Output parsing could be added here
            , ctfRawOutput = output
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

-- | Parse GHC errors from stderr.
-- Example line: "src/Foo.hs:42:5: error: ..."
parseGhcErrors :: Text -> [RawCompileError]
parseGhcErrors stderr = mapMaybe parseErrorLine (T.lines stderr)
  where
    parseErrorLine :: Text -> Maybe RawCompileError
    parseErrorLine line =
      let matches = (T.unpack line :: String) =~ ("^([^:]+):([0-9]+):([0-9]+): (error|warning): (.*)$" :: String) :: [[String]]
      in case matches of
        [[_, file, lineNum, _col, _, msg]] -> Just RawCompileError
          { rceFile = file
          , rceLine = read lineNum
          , rceMessage = T.pack msg
          }
        _ -> Nothing
