{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Export MCP tools from Role DSL definitions.
module ExoMonad.Control.Export
  ( exportMCPTools,
  )
where

import Control.Monad (forM_)
import Data.Text qualified as T
import ExoMonad.Control.Logging (Logger, logDebug, logInfo)
import ExoMonad.Control.Protocol (ToolDefinition (..))
import ExoMonad.Control.Role.Registry (RoleSchema (..), roleSchemaFor)
import ExoMonad.Graph.MCPReify (MCPToolInfo (..))
import ExoMonad.Role (Role (..))

-- | Export all MCP tools for a specific role using Role DSL.
--
-- This traverses the Role -> Server -> Tool hierarchy to extract
-- schema information.
exportMCPTools :: Logger -> Role -> IO [ToolDefinition]
exportMCPTools logger role = do
  logInfo logger $ "[MCP Discovery] Discovering tools for role: " <> T.pack (show role)

  let schema = roleSchemaFor role
      tools = schema.tools

  logInfo logger $ "[MCP Discovery] Found " <> T.pack (show (length tools)) <> " tools"

  -- Log tool names for debugging
  forM_ tools $ \info ->
    logDebug logger $ "[MCP Discovery]   " <> info.mtdName

  pure $ map toToolDef tools

-- | Convert MCPToolInfo -> ToolDefinition.
toToolDef :: MCPToolInfo -> ToolDefinition
toToolDef info =
  ToolDefinition
    { name = info.mtdName,
      description = info.mtdDescription,
      inputSchema = info.mtdInputSchema
    }
