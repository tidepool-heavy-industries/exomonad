{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}

-- | Type-safe conversion from 'ToolDef' to wire formats.
--
-- This module provides typeclasses to convert typed tool definitions
-- into the wire formats expected by different LLM APIs:
--
-- * 'ToAnthropicTool' - Convert to Anthropic Messages API format
-- * 'ToCfTool' - Convert to CloudFlare AI / OpenAI format
--
-- Both typeclasses have default implementations that work for any
-- 'ToolDef' instance, using the typed schemas from 'HasJSONSchema'.
--
-- = Usage
--
-- @
-- -- Define a tool with ToolDef
-- data SearchTool = SearchTool
--
-- instance ToolDef SearchTool where
--   type ToolInput SearchTool = SearchInput
--   type ToolOutput SearchTool = SearchOutput
--   type ToolEffects SearchTool = '[]
--   toolName _ = "search"
--   toolDescription _ = "Search the knowledge base"
--   toolExecute _ input = ...
--
-- -- Get empty instance for free (uses default implementation)
-- instance ToAnthropicTool SearchTool
-- instance ToCfTool SearchTool
--
-- -- Convert to wire format
-- anthropicTool :: AnthropicTool
-- anthropicTool = toAnthropicTool SearchTool
--
-- cfTool :: CfTool
-- cfTool = toCfTool SearchTool
-- @
module Tidepool.Tool.Convert
  ( -- * Conversion Typeclasses
    ToAnthropicTool(..)
  , ToCfTool(..)

    -- * Bulk Conversion
  , toolDefsToAnthropic
  , toolDefsToCf
  ) where

import Tidepool.Schema (HasJSONSchema(..))
import Tidepool.Graph.Tool (ToolDef(..))
import Tidepool.Tool.Wire
  ( AnthropicTool(..)
  , CfTool(..)
  , schemaToAnthropicTool
  , schemaToCfTool
  )


-- ════════════════════════════════════════════════════════════════════════════
-- ANTHROPIC CONVERSION
-- ════════════════════════════════════════════════════════════════════════════

-- | Convert a 'ToolDef' to Anthropic wire format.
--
-- The default implementation uses 'toolName', 'toolDescription', and
-- the 'HasJSONSchema' instance for the tool's input type.
--
-- @
-- instance ToAnthropicTool MyTool  -- uses default
--
-- -- Or provide custom implementation:
-- instance ToAnthropicTool MyTool where
--   toAnthropicTool tool = AnthropicTool
--     { atName = "custom_name"
--     , atDescription = "Custom description"
--     , atInputSchema = customSchema
--     }
-- @
class ToolDef t => ToAnthropicTool t where
  toAnthropicTool :: t -> AnthropicTool

  default toAnthropicTool :: HasJSONSchema (ToolInput t) => t -> AnthropicTool
  toAnthropicTool tool = schemaToAnthropicTool
    (toolName tool)
    (toolDescription tool)
    (jsonSchema @(ToolInput t))


-- ════════════════════════════════════════════════════════════════════════════
-- CLOUDFLARE AI / OPENAI CONVERSION
-- ════════════════════════════════════════════════════════════════════════════

-- | Convert a 'ToolDef' to CloudFlare AI / OpenAI wire format.
--
-- The default implementation uses 'toolName', 'toolDescription', and
-- the 'HasJSONSchema' instance for the tool's input type.
--
-- @
-- instance ToCfTool MyTool  -- uses default
--
-- -- Or provide custom implementation:
-- instance ToCfTool MyTool where
--   toCfTool tool = CfTool
--     { ctName = "custom_name"
--     , ctDescription = "Custom description"
--     , ctParameters = customSchema
--     }
-- @
class ToolDef t => ToCfTool t where
  toCfTool :: t -> CfTool

  default toCfTool :: HasJSONSchema (ToolInput t) => t -> CfTool
  toCfTool tool = schemaToCfTool
    (toolName tool)
    (toolDescription tool)
    (jsonSchema @(ToolInput t))


-- ════════════════════════════════════════════════════════════════════════════
-- BULK CONVERSION
-- ════════════════════════════════════════════════════════════════════════════

-- | Convert a collection of tools to Anthropic format.
--
-- @
-- tools :: [MyTool]
-- anthropicTools :: [AnthropicTool]
-- anthropicTools = toolDefsToAnthropic tools
-- @
toolDefsToAnthropic :: (Functor f, ToAnthropicTool t) => f t -> f AnthropicTool
toolDefsToAnthropic = fmap toAnthropicTool

-- | Convert a collection of tools to CloudFlare AI format.
--
-- @
-- tools :: [MyTool]
-- cfTools :: [CfTool]
-- cfTools = toolDefsToCf tools
-- @
toolDefsToCf :: (Functor f, ToCfTool t) => f t -> f CfTool
toolDefsToCf = fmap toCfTool
