{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Main entry point for the ClaudeCode test.
--
-- This executable demonstrates running a graph with ClaudeCode nodes.
-- It explores a test directory and takes follow-up action based on findings.
--
-- Usage:
--   ZELLIJ_SESSION=claude-code-test cabal run claude-code-test [directory]
--
-- Prerequisites:
--   1. zellij-cc binary in PATH
--   2. Running zellij session: zellij -s claude-code-test
module Main (main) where

import Control.Monad.Freer (runM)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs, lookupEnv)
import System.IO (hFlush, stdout)

import Tidepool.ClaudeCode.Config (mkClaudeCodeConfig, ClaudeCodeConfig(..))
import Tidepool.ClaudeCode.Effect (runClaudeCodeExecIO)
import Tidepool.Graph.Execute (CallHandler(..), DispatchGoto(..))

import ClaudeCodeTest.Graph (ClaudeCodeTestGraph(..))
import ClaudeCodeTest.Handlers (testHandlers)
import ClaudeCodeTest.Types (ExploreInput(..), ActionResult(..))


main :: IO ()
main = do
  -- Parse command line args
  args <- getArgs
  let testDir = case args of
        (d:_) -> d
        []    -> "./test-fixtures"

  -- Get zellij session from environment or use default
  mSession <- lookupEnv "ZELLIJ_SESSION"
  let sessionName = maybe "claude-code-test" T.pack mSession

  -- Build config
  let config = (mkClaudeCodeConfig sessionName)
        { ccDefaultTimeout = 600  -- 10 minutes for exploration tasks
        }

  -- Build input
  let input = ExploreInput
        { eiDirectory = testDir
        , eiObjective = "Find and analyze all files in the directory. Summarize their contents and recommend what to do with them."
        }

  -- Print startup info
  putStrLn "=============================================="
  putStrLn "ClaudeCode Test Graph"
  putStrLn "=============================================="
  putStrLn $ "Directory: " <> testDir
  putStrLn $ "Zellij session: " <> T.unpack sessionName
  putStrLn ""
  putStrLn "Starting graph execution..."
  putStrLn "(Watch the zellij pane for Claude Code activity)"
  putStrLn ""
  hFlush stdout

  -- Run the graph manually (bypassing runGraph's HasField constraint)
  --
  -- 1. Get the explore handler from the graph record
  -- 2. Call it with input using callHandler
  -- 3. Dispatch through the graph until exit
  result <- runM
    . runClaudeCodeExecIO config
    $ do
        -- Call the explore handler
        choice <- callHandler (explore testHandlers) input
        -- Dispatch through the rest of the graph
        dispatchGoto testHandlers choice

  -- Print result
  putStrLn ""
  putStrLn "=============================================="
  putStrLn "Result"
  putStrLn "=============================================="
  printResult result


-- | Pretty print the action result.
printResult :: ActionResult -> IO ()
printResult result = do
  putStrLn $ "Success: " <> show result.arSuccess
  putStrLn ""
  putStrLn "Description:"
  TIO.putStrLn result.arDescription
  case result.arArtifact of
    Nothing -> pure ()
    Just artifact -> do
      putStrLn ""
      putStrLn "Artifact:"
      TIO.putStrLn artifact
