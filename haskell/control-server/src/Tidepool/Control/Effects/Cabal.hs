{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Tidepool.Control.Effects.Cabal
  ( runCabalViaSsh
  ) where

import Control.Monad (void)
import Control.Monad.Freer (Eff, Member, interpret)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Regex.TDFA ((=~))

import Tidepool.Control.Effects.SshExec (SshExec, ExecRequest(..), ExecResult(..), execCommand)
import Tidepool.Effects.Cabal (Cabal(..), CabalResult(..), RawCompileError(..))

-- | Interpreter: uses SshExec to run cabal commands
runCabalViaSsh :: Member SshExec effs => Text -> Eff (Cabal ': effs) a -> Eff effs a
runCabalViaSsh container = interpret $ \case
  CabalBuild path -> do
    result <- execCommand $ ExecRequest
      { erContainer = container
      , erCommand = "cabal"
      , erArgs = ["build", "all", "-v0"]
      , erWorkingDir = path
      , erEnv = []
      , erTimeout = 600
      }
    pure $ case exExitCode result of
      0 -> CabalSuccess
      code -> CabalBuildFailure
        { cbfExitCode = code
        , cbfStderr = exStderr result
        , cbfStdout = exStdout result
        , cbfParsedErrors = parseGhcErrors (exStderr result)
        }

  CabalTest path -> do
    result <- execCommand $ ExecRequest
      { erContainer = container
      , erCommand = "cabal"
      , erArgs = ["test", "--test-show-details=always", "-v0"]
      , erWorkingDir = path
      , erEnv = []
      , erTimeout = 600
      }
    let output = exStdout result <> exStderr result
    pure $ case exExitCode result of
      0 -> CabalTestSuccess { ctsOutput = output }
      code ->
        -- Simple check if it's a build failure (could be improved)
        if "error:" `T.isInfixOf` exStderr result
          then CabalBuildFailure
            { cbfExitCode = code
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
      { erContainer = container
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
