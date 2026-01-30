{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad.Freer
import ExoMonad.Control.Effects.Cabal (runCabalRemote)
import ExoMonad.Control.Effects.Git (runGitRemote)
import ExoMonad.Control.Effects.Justfile (runJustfileRemote)
import ExoMonad.Control.Effects.SshExec (ExecRequest (..), ExecResult (..), SshExec (..))
import ExoMonad.Effects.Cabal (CabalResult (..), cabalBuild)
import ExoMonad.Effects.Git (WorktreeInfo (..), getWorktreeInfo)
import ExoMonad.Effects.Justfile (JustResult (..), runRecipe)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain spec

spec :: TestTree
spec =
  testGroup
    "Remote Execution Effects"
    [ testGroup
        "Cabal Interpreter"
        [ testCase "Build Success" $ do
            let mockResult = ExecResult (Just 0) "Build OK" ""
            let action = cabalBuild "path/to/project"
            result <- runM $ runMockSshExec mockResult $ runCabalRemote (Just "test-agent") action
            case result of
              CabalSuccess -> pure ()
              _ -> assertFailure $ "Expected CabalSuccess, got " ++ show result,
          testCase "Build Failure" $ do
            let stderr = "src/Main.hs:10:1: error: Variable not in scope: main"
            let mockResult = ExecResult (Just 1) "" stderr
            let action = cabalBuild "path/to/project"
            result <- runM $ runMockSshExec mockResult $ runCabalRemote (Just "test-agent") action
            case result of
              CabalBuildFailure code err out -> do
                code @?= 1
                err @?= stderr
              _ -> assertFailure $ "Expected CabalBuildFailure, got " ++ show result
        ],
      testGroup
        "Git Interpreter"
        [ testCase "GetWorktreeInfo Success" $ do
            let mockHandler req = case req.args of
                  ["rev-parse", "--show-toplevel"] -> ExecResult (Just 0) "/repo/root\n" ""
                  ["branch", "--show-current"] -> ExecResult (Just 0) "feature-branch\n" ""
                  ["rev-parse", "--git-dir"] -> ExecResult (Just 0) "/repo/root/.git\n" ""
                  _ -> ExecResult (Just 1) "" "Unknown command"

            let action = getWorktreeInfo
            result <- runM $ runMockSshExecSmart mockHandler $ runGitRemote "test-agent" "." action
            case result of
              Just wt -> do
                wt.wiBranch @?= "feature-branch"
                wt.wiPath @?= "/repo/root"
                wt.wiIsWorktree @?= False
              Nothing -> assertFailure "Expected Just WorktreeInfo"
        ],
      testGroup
        "Justfile Interpreter"
        [ testCase "Run Recipe Success" $ do
            let mockResult = ExecResult (Just 0) "Recipe output" ""
            let action = runRecipe "build" ["--release"]
            result <- runM $ runMockSshExec mockResult $ runJustfileRemote "test-agent" "." action
            result.exitCode @?= 0
            result.stdout @?= "Recipe output",
          testCase "Run Recipe Failure" $ do
            let mockResult = ExecResult (Just 1) "" "Recipe failed"
            let action = runRecipe "test" []
            result <- runM $ runMockSshExec mockResult $ runJustfileRemote "test-agent" "." action
            result.exitCode @?= 1
            result.stderr @?= "Recipe failed"
        ]
    ]

-- | Mock SshExec returning a single result
runMockSshExec :: ExecResult -> Eff (SshExec ': effs) a -> Eff effs a
runMockSshExec result = interpret $ \case
  ExecCommand _ -> pure result

-- | Smart mock that inspects request to determine response
runMockSshExecSmart :: (ExecRequest -> ExecResult) -> Eff (SshExec ': effs) a -> Eff effs a
runMockSshExecSmart f = interpret $ \case
  ExecCommand req -> pure $ f req
