-- | Types-first development workflow runner (sequential version).
--
-- This runner executes a simplified graph:
--   Entry(StackSpec) → types(ClaudeCode) → Exit(TypeDefinitions)
--
-- It validates the ClaudeCode execution path end-to-end without
-- parallel agents, worktrees, or fork/merge nodes.
module Main where

import Control.Monad.Freer (runM)
import Data.Text qualified as T
import System.Environment (lookupEnv)

import Tidepool.ClaudeCode.Config (mkClaudeCodeConfig, ClaudeCodeConfig(..))
import Tidepool.ClaudeCode.Effect (runClaudeCodeExecIO)
import Tidepool.Graph.Execute (callHandler, dispatchGoto)

import TypesFirstDev.Graph (TypesFirstGraph(..))
import TypesFirstDev.Handlers (typesFirstHandlers)
import TypesFirstDev.Types (StackSpec(..), TypeDefinitions(..))


-- | Main entry point.
--
-- Runs the types-first workflow to generate type definitions for a Stack.
main :: IO ()
main = do
  putStrLn "Types-First Development Workflow (Sequential)"
  putStrLn "============================================="
  putStrLn ""

  -- Get zellij session from env or use default
  sessionName <- maybe "types-first-dev" T.pack <$> lookupEnv "ZELLIJ_SESSION"

  let config = (mkClaudeCodeConfig sessionName)
        { ccDefaultTimeout = 600 }  -- 10 min timeout

  -- Define what we want to implement
  let spec = StackSpec
        { ssProjectPath = "."
        , ssModuleName = "Data.Stack"
        , ssDescription = "A LIFO (last-in-first-out) stack data structure with operations: \
                          \push (add element to top), pop (remove and return top element), \
                          \peek (view top element without removing), isEmpty (check if empty)"
        }

  putStrLn $ "Target: " <> T.unpack spec.ssModuleName
  putStrLn $ "Description: " <> T.unpack spec.ssDescription
  putStrLn ""
  putStrLn "Starting Claude Code execution..."
  putStrLn ""

  -- Run the graph
  result <- runM
    . runClaudeCodeExecIO config
    $ do
        choice <- callHandler (types typesFirstHandlers) spec
        dispatchGoto typesFirstHandlers choice

  printResult result


-- | Print the type definitions result.
printResult :: TypeDefinitions -> IO ()
printResult td = do
  putStrLn ""
  putStrLn "=== Type Definitions ==="
  putStrLn ""
  putStrLn "Data type:"
  putStrLn $ T.unpack td.tdDataType
  putStrLn ""
  putStrLn "Signatures:"
  mapM_ (\s -> putStrLn $ "  " <> T.unpack s) td.tdSignatures
  putStrLn ""
  putStrLn "Module header:"
  putStrLn $ T.unpack td.tdModuleHeader
