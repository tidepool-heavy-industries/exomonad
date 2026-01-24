{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad.Freer

import Tidepool.Control.Effects.SshExec (SshExec(..), ExecResult(..))
import Tidepool.Control.Effects.Cabal (runCabalViaSsh)
import Tidepool.Effects.Cabal (CabalResult(..), RawCompileError(..), cabalBuild)

main :: IO ()
main = defaultMain spec

spec :: TestTree
spec = testGroup "SSH Execution Effects"
  [ testCase "Cabal Build Success" $ do
      let mockResult = ExecResult 0 "Build OK" ""
      let action = cabalBuild "path/to/project"
      result <- runM $ runMockSshExec mockResult $ runCabalViaSsh "test-agent" action
      case result of
        CabalSuccess -> pure ()
        _ -> assertFailure $ "Expected CabalSuccess, got " ++ show result

  , testCase "Cabal Build Failure with Parsing" $ do
      let stderr = "src/Main.hs:10:1: error: Variable not in scope: main"
      let mockResult = ExecResult 1 "" stderr
      let action = cabalBuild "path/to/project"
      result <- runM $ runMockSshExec mockResult $ runCabalViaSsh "test-agent" action
      case result of
        CabalBuildFailure _ _ _ errors -> do
          length errors @?= 1
          let err = head errors
          err.rceFile @?= "src/Main.hs"
          err.rceLine @?= 10
        _ -> assertFailure $ "Expected CabalBuildFailure, got " ++ show result
  ]

runMockSshExec :: ExecResult -> Eff (SshExec ': effs) a -> Eff effs a
runMockSshExec result = interpret $ \case
  ExecCommand _ -> pure result
