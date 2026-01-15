{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | MCP server harness using mcp-server library
--
-- Exposes Tidepool LLMNode as MCP tools over stdio transport.
module Tidepool.MCP.Server
  ( -- * Running Servers
    runMcpServer

    -- * Tool Definition Helpers
  , makeMcpTool

    -- * Re-exports
  , module Tidepool.MCP.Types
  ) where

import Data.Aeson (FromJSON, ToJSON, Value, Result(..), fromJSON, toJSON)
import Data.List (find)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import MCP.Server (runMcpServerStdio)
import MCP.Server.Types
  ( McpServerInfo(..)
  , McpServerHandlers(..)
  , ToolDefinition(..)
  , InputSchemaDefinition(..)
  , Content(..)
  , Error(..)
  )

import Tidepool.MCP.Types
import Tidepool.Schema (HasJSONSchema(..), schemaToValue)

-- | Run MCP server with stdio transport
--
-- Blocks until EOF on stdin. Handles:
-- - initialize handshake
-- - tools/list
-- - tools/call
--
-- Example:
-- @
-- tools <- buildTools
-- runMcpServer (McpConfig "scout" "0.1" tools)
-- @
runMcpServer :: McpConfig -> IO ()
runMcpServer cfg = runMcpServerStdio serverInfo handlers
  where
    serverInfo = McpServerInfo
      { serverName = cfg.mcName
      , serverVersion = cfg.mcVersion
      , serverInstructions = "Tidepool MCP Server - LLMNode as MCP tools"
      }

    handlers = McpServerHandlers
      { tools = Just (listTools cfg, callTool cfg)
      , resources = Nothing
      , prompts = Nothing
      }

-- | List available tools
listTools :: McpConfig -> IO [ToolDefinition]
listTools cfg = pure $ map mcpToolToToolDef cfg.mcTools

-- | Convert our McpTool to mcp-server's ToolDefinition
--
-- Note: We need to convert the JSON Schema Value to InputSchemaDefinition
-- For now, we'll create a simple schema structure
mcpToolToToolDef :: McpTool -> ToolDefinition
mcpToolToToolDef (McpTool name desc schema _) =
  ToolDefinition
    { toolDefinitionName = name
    , toolDefinitionDescription = desc
    , toolDefinitionInputSchema = schemaToInputDef schema
    , toolDefinitionTitle = Nothing
    }
  where
    -- TODO: Parse the Value schema and extract properties/required
    -- For now, use an empty schema as placeholder
    schemaToInputDef _ = InputSchemaDefinitionObject
      { properties = []
      , required = []
      }

-- | Execute tool by name
callTool :: McpConfig -> Text -> [(Text, Text)] -> IO (Either Error Content)
callTool cfg name args = case findTool name cfg.mcTools of
  Nothing -> pure $ Left $ UnknownTool name
  Just (McpTool _ _ _ runner) -> do
    -- Convert [(Text, Text)] to JSON Value for parsing
    -- For simplicity, treat each arg as a text value
    let argsObject = toJSON $ foldl (\acc (k, v) -> acc <> [(k, toJSON v)]) [] args
    case fromJSON argsObject of
      Error e -> pure $ Left $ InvalidParams $ T.pack e
      Success input -> do
        result <- runner input
        pure $ Right $ ContentText $ T.pack (show $ toJSON result)

-- | Find tool by name
findTool :: Text -> [McpTool] -> Maybe McpTool
findTool name tools = find (\(McpTool n _ _ _) -> n == name) tools

-- | Helper to create an McpTool from LLMNode
--
-- Usage:
-- @
-- scoutTool <- makeMcpTool (Proxy @ScoutInput) "scout" "Search codebase" $ \\input -> do
--   result <- executeLLMNode scoutNode input
--   pure result
-- @
makeMcpTool
  :: forall i o. (FromJSON i, ToJSON o, HasJSONSchema i)
  => Proxy i
  -> Text
  -> Text
  -> (i -> IO o)
  -> McpTool
makeMcpTool _ name desc runner = McpTool
  { mtName = name
  , mtDescription = desc
  , mtInputSchema = schemaToValue (jsonSchema @i)
  , mtRunner = runner
  }
