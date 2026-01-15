{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Semantic Scout MCP Server Entry Point
module Main (main) where

import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (getArgs)

import Tidepool.MCP.Server (runMcpServer, makeMcpTool, McpConfig(..))
import Tidepool.Agents.Scout.Types

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--mcp"] -> runScoutMCP
    _ -> putStrLn "Usage: semantic-scout --mcp"

runScoutMCP :: IO ()
runScoutMCP = do
  let scoutTool = makeMcpTool
        (Proxy @ScoutQuery)
        "scout"
        "Explore codebase to answer semantic questions about code"
        executeScout

  let config = McpConfig
        { mcName = "semantic-scout"
        , mcVersion = "0.1.0"
        , mcTools = [scoutTool]
        }

  runMcpServer config

-- | Execute a scout query
--
-- For MVP: Returns a placeholder response.
-- TODO: Integrate with LLM + LSP tools
executeScout :: ScoutQuery -> IO ScoutResponse
executeScout query = do
  let defaultBudget = maybe 20 id (sqBudget query)

  -- Placeholder response for testing
  pure $ ScoutResponse
    { srSummary = T.unwords
        [ "This is a placeholder response for the query:"
        , sqQuery query
        , "\n\nInterest tags:"
        , T.intercalate ", " (sqTags query)
        , "\n\nBudget:"
        , T.pack (show defaultBudget)
        , "\n\n**TODO**: Integrate with LLM + LSP tools for real semantic exploration."
        ]
    , srPointers =
        [ Pointer
            { pLocation = "placeholder.hs:1"
            , pWhat = "Placeholder pointer"
            , pRisk = "low"
            , pAction = Just "Replace with real LSP-based exploration"
            }
        ]
    , srNodesVisited = 0
    }
