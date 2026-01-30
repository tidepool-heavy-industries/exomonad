{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad.Freer
import ExoMonad.Control.Effects.Git (runGitRemote)
import ExoMonad.Control.Effects.Justfile (runJustfileRemote)
import ExoMonad.Control.Effects.SshExec (ExecRequest (..), ExecResult (..), SshExec (..))
import ExoMonad.Effects.Git (WorktreeInfo (..), fetchRemote, getWorktreeInfo)
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
        "Git Interpreter"
        [ testCase "GetWorktreeInfo Success" $ do
            let mockHandler req = case req.args of
                  ["rev-parse", "--show-toplevel"] -> ExecResult (Just 0) "/repo/root\n" ""
                  ["branch", "--show-current"] -> ExecResult (Just 0) "feature-branch\n" ""
                  ["rev-parse", "--git-dir"] -> ExecResult (Just 0) "/repo/root/.git\n" ""
                  _ -> ExecResult (Just 1) "" "Unknown command"

            let action = getWorktreeInfo
            result <- runM $ runMockSshExecSmart mockHandler $ runGitRemote (Just "test-agent") "." action
            case result of
              Just wt -> do
                wt.wiBranch @?= "feature-branch"
                wt.wiPath @?= "/repo/root"
                wt.wiIsWorktree @?= False
              Nothing -> assertFailure "Expected Just WorktreeInfo",
          testCase "FetchRemote Success" $ do
            let mockHandler req = case req.args of
                  ["fetch", "origin"] -> ExecResult (Just 0) "" ""
                  _ -> ExecResult (Just 1) "" "Unknown command"

            let action = fetchRemote "origin" Nothing
            -- Expect no error
            runM $ runMockSshExecSmart mockHandler $ runGitRemote (Just "test-agent") "." action
        ],
      testGroup
        "Justfile Interpreter"
        [ testCase "Run Recipe Success" $ do
            let mockResult = ExecResult (Just 0) "Recipe output" ""
            let action = runRecipe "build" ["--release"]
            result <- runM $ runMockSshExec mockResult $ runJustfileRemote (Just "test-agent") "." action
            result.exitCode @?= 0
            result.stdout @?= "Recipe output",
          testCase "Run Recipe Failure" $ do
            let mockResult = ExecResult (Just 1) "" "Recipe failed"
            let action = runRecipe "test" []
            result <- runM $ runMockSshExec mockResult $ runJustfileRemote (Just "test-agent") "." action
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
