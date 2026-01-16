{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Semantic Scout MCP Server Entry Point
--
-- Exposes the scout exploration as an MCP tool for semantic code exploration.
-- Generates training examples during exploration for later fine-tuning.
--
-- Architecture:
--   * Scout is NOT a graph - it's a single exploration function
--   * FunctionGemma (future) only scores locations (outputs Rubric)
--   * Haskell heuristics decide expansion based on Rubric
--   * LSP provides the actual code navigation
module Main (main) where

import Control.Exception (try, SomeException)
import Control.Monad.Freer (runM)
import Data.Proxy (Proxy(..))
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

import Tidepool.LSP.Interpreter (withLSPSession, runLSP, LSPSession)
import Tidepool.MCP.Server (runMcpServer, makeMcpTool, McpConfig(..))
import Tidepool.Agents.Scout.Types
import Tidepool.Agents.Scout.Explore (exploreEff, defaultExploreConfig)
import Tidepool.Agents.Scout.Gemma (runGemmaHeuristic)


main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--mcp"] -> runScoutMCP
    ["--gen-training", nStr] -> generateTraining (read nStr)
    ["--explore"] -> runExploreDemo
    _ -> putStrLn $ unlines
      [ "Usage:"
      , "  semantic-scout --mcp           Run as MCP server"
      , "  semantic-scout --gen-training N  Generate N training examples"
      , "  semantic-scout --explore       Run exploration demo"
      ]


-- | Run as MCP server.
runScoutMCP :: IO ()
runScoutMCP = do
  cwd <- getCurrentDirectory

  -- Initialize LSP session once for the entire MCP server lifetime
  withLSPSession cwd $ \session -> do
    let scoutTool = makeMcpTool
          (Proxy @ScoutQuery)
          "scout"
          "Explore codebase to answer semantic questions about code."
          (executeScout session)  -- Pass session via closure

    let config = McpConfig
          { mcName = "semantic-scout"
          , mcVersion = "0.1.0"
          , mcTools = [scoutTool]
          }

    runMcpServer config


-- | Execute a scout query.
--
-- This runs the exploration loop with real LSP code intelligence.
-- No fallback - will error if LSP fails so we can debug issues.
executeScout :: LSPSession -> ScoutQuery -> IO ScoutResponse
executeScout session query = do
  runM $ runGemmaHeuristic $ runLSP session $ exploreEff defaultExploreConfig query


-- | Demo mode: run exploration and print results.
runExploreDemo :: IO ()
runExploreDemo = do
  putStrLn "Running exploration demo..."
  hFlush stdout
  cwd <- getCurrentDirectory
  putStrLn $ "Working directory: " ++ cwd
  hFlush stdout

  let query = ScoutQuery
        { sqQuery = "What breaks if I add a variant to LLMKind?"
        , sqSymbols = ["LLMKind"]  -- Entry point symbols
        , sqDepth = Low            -- Quick scan (~10 nodes)
        }

  putStrLn "Initializing LSP session..."
  hFlush stdout

  -- Initialize LSP session for demo
  result <- try $ withLSPSession cwd $ \session -> do
    putStrLn "LSP session initialized, executing scout..."
    hFlush stdout
    executeScout session query

  case result of
    Left (err :: SomeException) -> do
      putStrLn $ "ERROR: " ++ show err
      hFlush stdout
    Right response -> do
      putStrLn $ "Summary:\n" ++ show (srSummary response)
      putStrLn $ "\nNodes visited: " ++ show (srNodesVisited response)
      putStrLn $ "\nPointers found: " ++ show (length (srPointers response))
      putStrLn $ "\nTraining examples: " ++ show (length (srTrainingExamples response))

      putStrLn "\n--- Pointers ---"
      mapM_ printPointer (srPointers response)

  where
    printPointer p = putStrLn $ unlines
      [ "  Location: " ++ show (pLocation p)
      , "  What: " ++ show (pWhat p)
      , "  Risk: " ++ show (pRisk p)
      , "  Relevance: " ++ show (pRelevance p)
      , "  Tags: " ++ show (pTags p)
      , ""
      ]


-- | Generate training examples from exploration runs.
-- This mode outputs JSONL training data for fine-tuning FunctionGemma.
generateTraining :: Int -> IO ()
generateTraining n = do
  putStrLn $ "TODO: Generate " ++ show n ++ " training examples"
  putStrLn "Use 'training-generator' for synthetic data: cabal run training-generator -- N"
  putStrLn ""
  putStrLn "For exploration-based training data:"
  putStrLn "  1. Run semantic-scout --explore on real codebases"
  putStrLn "  2. Collect srTrainingExamples from responses"
  putStrLn "  3. Format as FunctionGemma Turn 1-5 JSONL"
