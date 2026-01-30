{-# LANGUAGE UndecidableInstances #-}

-- | Tool schema derivation and runtime dispatch for LLM calls.
--
-- This module provides the machinery to:
--
-- 1. Define tool records with handler functions
-- 2. Derive tool schemas for the LLM
-- 3. Dispatch tool calls by name to the appropriate handler at runtime
--
-- = Design
--
-- Tools are defined as records where each field is a handler function.
-- The 'ToolRecord' typeclass provides schema extraction and dispatch.
--
-- = Usage
--
-- @
-- -- Define tool input types (must have FromJSON, HasJSONSchema)
-- data SearchArgs = SearchArgs { query :: Text }
--   deriving (Generic, FromJSON, ToJSON, HasJSONSchema)
--
-- data SearchResult = SearchResult { results :: [Text] }
--   deriving (Generic, FromJSON, ToJSON)
--
-- -- Define tool record
-- data AnalysisTools es = AnalysisTools
--   { search :: SearchArgs -> Eff es SearchResult
--   , readSection :: ReadArgs -> Eff es SectionContent
--   }
--
-- -- Create instance (manual for now, TH later)
-- instance ToolRecord AnalysisTools where
--   toolSchemas _ =
--     [ ToolSchema "search" "Search the document" (schemaToValue $ jsonSchema \@SearchArgs)
--     , ToolSchema "read_section" "Read a section" (schemaToValue $ jsonSchema \@ReadArgs)
--     ]
--   dispatchTool tools name input = case name of
--     "search" -> dispatchHandler tools.search "search" input
--     "read_section" -> dispatchHandler tools.readSection "read_section" input
--     _ -> pure $ Left $ ToolNotFound name
-- @
module ExoMonad.LLM.Tools
  ( -- * Tool Schema Types
    ToolSchema(..)
  , toolSchemaToAnthropicTool

    -- * Tool Record Typeclass
  , ToolRecord(..)

    -- * Tool Dispatch Helpers
  , dispatchHandler
  , ToolDispatchError(..)
  ) where

import Control.Monad.Freer (Eff)
import Data.Aeson (Value, FromJSON, ToJSON, fromJSON, toJSON)
import qualified Data.Aeson as Aeson
import Data.Kind (Type)
import Data.Proxy (Proxy)
import Data.Text (Text)
import qualified Data.Text as T

import ExoMonad.Tool.Wire (AnthropicTool(..))


-- ════════════════════════════════════════════════════════════════════════════
-- TOOL SCHEMA TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Schema for a single tool.
--
-- This is the information needed to tell the LLM about a tool.
data ToolSchema = ToolSchema
  { tsName        :: Text
    -- ^ Tool name (snake_case, used in tool_use blocks)
  , tsDescription :: Text
    -- ^ Human-readable description (shown to LLM)
  , tsInputSchema :: Value
    -- ^ JSON Schema for tool input
  }
  deriving stock (Show, Eq)

-- | Convert a ToolSchema to Anthropic wire format.
toolSchemaToAnthropicTool :: ToolSchema -> AnthropicTool
toolSchemaToAnthropicTool ts = AnthropicTool
  { atName = ts.tsName
  , atDescription = ts.tsDescription
  , atInputSchema = ts.tsInputSchema
  }


-- ════════════════════════════════════════════════════════════════════════════
-- TOOL DISPATCH ERROR
-- ════════════════════════════════════════════════════════════════════════════

-- | Errors that can occur during tool dispatch.
data ToolDispatchError
  = ToolNotFound Text
    -- ^ Tool name not found in the record
  | ToolInputParseError Text Text
    -- ^ Tool input failed to parse: (tool name, error message)
  deriving stock (Show, Eq)


-- ════════════════════════════════════════════════════════════════════════════
-- TOOL RECORD TYPECLASS
-- ════════════════════════════════════════════════════════════════════════════

-- | Typeclass for tool record types.
--
-- Implementations provide:
--
-- 1. 'toolSchemas' - List of schemas for all tools (derived from types)
-- 2. 'dispatchTool' - Route tool calls to handlers by name
--
-- @
-- data MyTools es = MyTools
--   { search :: SearchArgs -> Eff es SearchResult
--   , lookup :: LookupArgs -> Eff es LookupResult
--   }
--
-- instance ToolRecord MyTools where
--   toolSchemas _ =
--     [ ToolSchema "search" "Search for items" (schemaToValue $ jsonSchema \@SearchArgs)
--     , ToolSchema "lookup" "Look up by ID" (schemaToValue $ jsonSchema \@LookupArgs)
--     ]
--   dispatchTool tools name input = case name of
--     "search" -> dispatchHandler tools.search "search" input
--     "lookup" -> dispatchHandler tools.lookup "lookup" input
--     _ -> pure $ Left $ ToolNotFound name
-- @
class ToolRecord (tools :: [Type -> Type] -> Type) where
  -- | Extract tool schemas from the record type.
  --
  -- This uses the proxy pattern since schemas are derived from types,
  -- not from values.
  toolSchemas :: Proxy tools -> [ToolSchema]

  -- | Dispatch a tool call to the appropriate handler.
  --
  -- Given a tool record, tool name, and JSON input, calls the
  -- matching handler and returns the JSON result.
  dispatchTool
    :: tools es
    -> Text
    -> Value
    -> Eff es (Either ToolDispatchError Value)


-- ════════════════════════════════════════════════════════════════════════════
-- DISPATCH HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Dispatch to a single handler function.
--
-- Parses the JSON input, calls the handler, and encodes the result.
--
-- @
-- dispatchTool tools name input = case name of
--   "search" -> dispatchHandler tools.search "search" input
--   _ -> pure $ Left $ ToolNotFound name
-- @
dispatchHandler
  :: (FromJSON args, ToJSON result, Applicative m)
  => (args -> m result)
  -> Text
  -> Value
  -> m (Either ToolDispatchError Value)
dispatchHandler handler toolName input =
  case fromJSON input of
    Aeson.Success args ->
      Right . toJSON <$> handler args
    Aeson.Error err ->
      pure $ Left $ ToolInputParseError toolName (T.pack err)
