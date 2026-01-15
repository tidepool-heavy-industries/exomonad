{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

-- | Core types for MCP server
--
-- Provides types for wrapping LLMNode definitions as MCP-exposed tools.
module Tidepool.MCP.Types
  ( -- * MCP Tool Wrapper
    McpTool(..)
  , McpConfig(..)
  ) where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Text (Text)

-- | Wraps an LLM node for MCP exposure
--
-- Existentially quantifies over input/output types, allowing heterogeneous
-- tool collections while maintaining type safety at each invocation site.
data McpTool = forall i o. (FromJSON i, ToJSON o) => McpTool
  { mtName :: Text
    -- ^ Tool name (used for routing)
  , mtDescription :: Text
    -- ^ Tool description (for LLM understanding)
  , mtInputSchema :: Value
    -- ^ JSON Schema for input validation
  , mtRunner :: i -> IO o
    -- ^ Execute the LLM node and return result
  }

-- | MCP server configuration
data McpConfig = McpConfig
  { mcName :: Text
    -- ^ Server name
  , mcVersion :: Text
    -- ^ Server version
  , mcTools :: [McpTool]
    -- ^ Available tools
  }
