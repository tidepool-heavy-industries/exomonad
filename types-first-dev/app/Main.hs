-- | Types-first development workflow runner (parallel version).
--
-- This runner executes the full graph:
--   Entry(StackSpec) → types(ClaudeCode) → fork → (tests || impl) → merge → Exit
--
-- It demonstrates parallel agent execution with git worktrees for isolation.
module Main where

import Control.Monad (unless)
import Control.Monad.Freer (runM)
import Control.Monad.Freer.Reader (runReader)
import Data.Text qualified as T
import System.Directory (getCurrentDirectory)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.Process (readProcess)

import Tidepool.ClaudeCode.Config (mkClaudeCodeConfig, ClaudeCodeConfig(..))
import Tidepool.ClaudeCode.Effect (runClaudeCodeExecIO)
import Tidepool.Graph.Execute (callHandler, dispatchGoto)
import Tidepool.Worktree.Executor (runWorktreeIO, defaultWorktreeConfig)

import TypesFirstDev.Graph (TypesFirstGraph(..))
import TypesFirstDev.Handlers (typesFirstHandlers)
import TypesFirstDev.Types (StackSpec(..), ParallelResults(..), TestsResult(..), ImplResult(..))


-- | Main entry point.
--
-- Runs the types-first workflow to generate type definitions for a Stack.
main :: IO ()
main = do
  putStrLn "Types-First Development Workflow (Parallel)"
  putStrLn "============================================"
  putStrLn ""

  -- Get zellij session from env or use default
  sessionName <- maybe "types-first-dev" T.pack <$> lookupEnv "ZELLIJ_SESSION"

  -- Validate that the Zellij session exists (fail fast instead of hanging)
  validateZellijSession sessionName

  let claudeConfig = (mkClaudeCodeConfig sessionName)
        { ccDefaultTimeout = 600 }  -- 10 min timeout

  -- Get project path from env or use current directory
  projectPath <- maybe getCurrentDirectory pure =<< lookupEnv "PROJECT_PATH"
  let worktreeConfig = defaultWorktreeConfig projectPath

  -- Define what we want to implement
  let spec = StackSpec
        { ssProjectPath = projectPath
        , ssModuleName = "Data.Stack"
        , ssDescription = "A LIFO (last-in-first-out) stack data structure with operations: \
                          \push (add element to top), pop (remove and return top element), \
                          \peek (view top element without removing), isEmpty (check if empty)"
        , ssAcceptanceCriteria =
            [ "LIFO order: push a,b,c then pop returns c,b,a"
            , "push then pop is identity: pop (push x s) == Just (x, s)"
            , "empty stack invariants: isEmpty empty, pop empty == Nothing"
            ]
        }

  putStrLn $ "Project: " <> projectPath
  putStrLn $ "Module: " <> T.unpack spec.ssModuleName
  putStrLn $ "Session: " <> T.unpack sessionName
  putStrLn ""
  putStrLn "Starting Claude Code execution..."
  putStrLn ""

  -- Run the graph with full effect stack
  -- Note: Interpreter order must match DevEffects = '[Reader ClaudeCodeConfig, Reader StackSpec, ...]
  -- Innermost interpreter handles last effect in the list.
  result <- runM
    . runWorktreeIO worktreeConfig
    . runClaudeCodeExecIO claudeConfig
    . runReader spec          -- Reader StackSpec (second in DevEffects)
    . runReader claudeConfig  -- Reader ClaudeCodeConfig (first in DevEffects)
    $ do
        choice <- callHandler (types typesFirstHandlers) spec
        dispatchGoto typesFirstHandlers choice

  printResult result


-- | Print the parallel results.
--
-- Now prints metadata (commit messages, design notes) instead of code.
-- Code lives in the project files after agents edit them.
printResult :: ParallelResults -> IO ()
printResult pr = do
  putStrLn ""
  putStrLn "=== Parallel Results ==="
  putStrLn ""

  -- Impl result
  putStrLn "Implementation:"
  putStrLn $ "  Build passed: " <> show pr.prImplResult.irBuildPassed
  putStrLn $ "  All functions: " <> show pr.prImplResult.irAllFunctionsImplemented
  putStrLn $ "  Commit msg: " <> T.unpack pr.prImplResult.irCommitMessage
  putStrLn $ "  Design notes: " <> T.unpack pr.prImplResult.irDesignNotes
  case pr.prImplResult.irBlocker of
    Nothing -> pure ()
    Just blocker -> putStrLn $ "  BLOCKED: " <> T.unpack blocker
  putStrLn ""

  -- Tests result
  putStrLn "Tests:"
  putStrLn $ "  Build passed: " <> show pr.prTestsResult.trBuildPassed
  putStrLn $ "  All properties: " <> show pr.prTestsResult.trAllPropertiesWritten
  putStrLn $ "  Commit msg: " <> T.unpack pr.prTestsResult.trCommitMessage
  putStrLn $ "  Strategy: " <> T.unpack pr.prTestsResult.trTestingStrategy
  case pr.prTestsResult.trBlocker of
    Nothing -> pure ()
    Just blocker -> putStrLn $ "  BLOCKED: " <> T.unpack blocker
  putStrLn ""

  putStrLn "Work committed and merged. Check git log for agent commits."


-- | Validate that the Zellij session exists before proceeding.
--
-- Fails fast with a clear error message if the session doesn't exist,
-- rather than hanging indefinitely waiting for a non-existent session.
validateZellijSession :: T.Text -> IO ()
validateZellijSession sessionName = do
  sessions <- readProcess "zellij" ["list-sessions"] ""
  -- Strip ANSI codes and extract just the session name (first word of each line)
  let sessionLines = lines sessions
      -- Session name is the first word after stripping ANSI codes
      extractName line = head $ words $ stripAnsi line
      knownSessions = map extractName $ filter (not . null) sessionLines
      sessionExists = T.unpack sessionName `elem` knownSessions
  unless sessionExists $ do
    putStrLn ""
    putStrLn "ERROR: Zellij session not found!"
    putStrLn ""
    putStrLn $ "  Expected session: " <> T.unpack sessionName
    putStrLn ""
    putStrLn "Available sessions:"
    mapM_ (\s -> putStrLn $ "  - " <> s) knownSessions
    putStrLn ""
    putStrLn "To create the session, run:"
    putStrLn $ "  zellij --session " <> T.unpack sessionName
    putStrLn ""
    exitFailure

-- | Strip ANSI escape codes from a string.
stripAnsi :: String -> String
stripAnsi [] = []
stripAnsi ('\ESC':'[':rest) = stripAnsi $ drop 1 $ dropWhile (/= 'm') rest
stripAnsi (c:rest) = c : stripAnsi rest
