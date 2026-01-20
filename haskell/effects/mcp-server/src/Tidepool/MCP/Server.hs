{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | MCP server harness using vendored mcp-server library
--
-- Exposes Tidepool LLMNode as MCP tools over stdio transport.
-- Uses vendored mcp-server with protocol version patch for Claude Code compatibility.
module Tidepool.MCP.Server
  ( -- * Running Servers
    runMcpServer

    -- * Tool Definition Helpers
  , makeMcpTool

    -- * Re-exports
  , module Tidepool.MCP.Types
  ) where

import Data.Aeson (FromJSON, ToJSON, Value(..), Result(..), fromJSON, toJSON, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Text (encodeToTextBuilder)
import qualified Data.Aeson.Key as Key
import Data.Foldable (toList)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Data.List (find)
import Data.Proxy (Proxy(..))
import qualified Data.Scientific as Sci
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import MCP.Server (runMcpServerStdio)
import MCP.Server.Types
  ( McpServerInfo(..)
  , McpServerHandlers(..)
  , ToolDefinition(..)
  , InputSchemaDefinition(..)
  , InputSchemaDefinitionProperty(..)
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
mcpToolToToolDef :: McpTool -> ToolDefinition
mcpToolToToolDef (McpTool name desc schema _) =
  ToolDefinition
    { toolDefinitionName = name
    , toolDefinitionDescription = desc
    , toolDefinitionInputSchema = schemaToInputDef schema
    , toolDefinitionTitle = Nothing
    }
  where
    -- Extract properties and required fields from the JSON Schema Value
    schemaToInputDef (Object obj) =
      let props = case KM.lookup "properties" obj of
            Just (Object p) -> [(Key.toText k, valToProperty v) | (k, v) <- KM.toList p]
            _ -> []
          req = case KM.lookup "required" obj of
            Just (Array a) -> [r | String r <- toList a]
            _ -> []
      in InputSchemaDefinitionObject
          { properties = props
          , required = req
          }
    schemaToInputDef _ = InputSchemaDefinitionObject
      { properties = []
      , required = []
      }

    -- Helper to convert Value to InputSchemaDefinitionProperty
    valToProperty (Object o) =
      let t = case KM.lookup "type" o of
                Just (String s) -> s
                _ -> "string"
          d = case KM.lookup "description" o of
                Just (String s) -> s
                _ -> ""
      in InputSchemaDefinitionProperty { propertyType = t, propertyDescription = d }
    valToProperty _ = InputSchemaDefinitionProperty { propertyType = "string", propertyDescription = "" }

-- | Execute tool by name
callTool :: McpConfig -> Text -> [(Text, Text)] -> IO (Either Error Content)
callTool cfg name args = case findTool name cfg.mcTools of
  Nothing -> pure $ Left $ UnknownTool name
  Just (McpTool _ _ _ runner) -> do
    -- Convert [(Text, Text)] to JSON object
    -- Note: mcp-server library flattens all values to Text.
    -- We need to try parsing them back to appropriate types (e.g. Numbers)
    -- for tools that expect them.
    let argsObject = Object $ KM.fromList [Key.fromText k .= parseArg v | (k, v) <- args]
    case fromJSON argsObject of
      Error e -> pure $ Left $ InvalidParams $ T.pack e
      Success input -> do
        result <- runner input
        -- Return proper JSON encoding, not Haskell show representation
        pure $ Right $ ContentText $ TL.toStrict $ TLB.toLazyText $ encodeToTextBuilder $ toJSON result
  where
    -- Parse arguments from mcp-server's Text format back to appropriate JSON types
    -- MCP protocol flattens all parameters to Text; we need to recover type information
    -- for tools that expect numeric/boolean parameters.
    --
    -- Supports:
    -- - Booleans: "true", "false"
    -- - Integers: "42", "-5"
    -- - Decimals: "3.14", "-0.5"
    -- - Scientific notation: "1.5e10", "2E-3" (MCP spec allows this)
    --
    -- Falls back to String for:
    -- - Malformed numbers: "--5", "1.2.3", "abc"
    -- - Empty strings
    -- - Arbitrary text
    parseArg :: Text -> Value
    parseArg v = case v of
      "true"  -> Bool True
      "false" -> Bool False
      _ -> case TR.signed TR.rational v of
        Right (num, rest) | T.null rest -> Number (Sci.fromFloatDigits (num :: Double))
        _ -> String v

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
