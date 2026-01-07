-- | Types-first development workflow runner (parallel version).
--
-- This runner executes the full graph:
--   Entry(StackSpec) → types(ClaudeCode) → fork → (tests || impl) → merge → Exit
--
-- It demonstrates parallel agent execution with git worktrees for isolation.
module Main where

import Control.Monad.Freer (runM)
import Control.Monad.Freer.Reader (runReader)
import Data.Text qualified as T
import System.Directory (getCurrentDirectory)
import System.Environment (lookupEnv)

import Tidepool.ClaudeCode.Config (mkClaudeCodeConfig, ClaudeCodeConfig(..))
import Tidepool.ClaudeCode.Effect (runClaudeCodeExecIO)
import Tidepool.Graph.Execute (callHandler, dispatchGoto)
import Tidepool.Worktree.Executor (runWorktreeIO, defaultWorktreeConfig)

import TypesFirstDev.Graph (TypesFirstGraph(..))
import TypesFirstDev.Handlers (typesFirstHandlers)
import TypesFirstDev.Types (StackSpec(..), ParallelResults(..), TestDefinitions(..), ImplementationCode(..))


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
printResult :: ParallelResults -> IO ()
printResult pr = do
  putStrLn ""
  putStrLn "=== Parallel Results ==="
  putStrLn ""
  putStrLn "Tests worktree:"
  putStrLn $ "  " <> pr.prTestsWorktree
  putStrLn ""
  putStrLn "Impl worktree:"
  putStrLn $ "  " <> pr.prImplWorktree
  putStrLn ""
  putStrLn "=== Test Module ==="
  putStrLn $ T.unpack pr.prTestDefs.testModuleCode
  putStrLn ""
  putStrLn "=== Implementation Module ==="
  putStrLn $ T.unpack pr.prImplCode.implModuleCode
