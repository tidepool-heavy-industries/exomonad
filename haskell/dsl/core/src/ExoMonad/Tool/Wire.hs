{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Wire format types for LLM tool definitions.
--
-- This module provides serialization-ready types for different LLM APIs:
--
-- * 'AnthropicTool' - Anthropic Messages API format (@input_schema@)
-- * 'CfTool' - CloudFlare AI / OpenAI format (@parameters@, wrapped in @function@)
--
-- These are "wire types" - simple records for JSON serialization.
--
-- = Format Differences
--
-- __Anthropic__:
--
-- @
-- { "name": "search",
--   "description": "Search the web",
--   "input_schema": { "type": "object", "properties": {...} }
-- }
-- @
--
-- __OpenAI / CloudFlare AI__:
--
-- @
-- { "type": "function",
--   "function": {
--     "name": "search",
--     "description": "Search the web",
--     "parameters": { "type": "object", "properties": {...} }
--   }
-- }
-- @
module ExoMonad.Tool.Wire
  ( -- * Anthropic Format
    AnthropicTool (..),
    anthropicToolToJSON,

    -- * OpenAI / CloudFlare AI Format
    CfTool (..),
    CfObjectSchema (..),
    CfProperty (..),
    cfToolToValue,

    -- * Conversion from JSONSchema
    schemaToAnthropicTool,
    schemaToCfTool,
    schemaToCfObject,
    schemaToCfProperty,
  )
where

import Data.Aeson (ToJSON (..), Value, object, (.=))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import ExoMonad.Schema (JSONSchema (..), SchemaType (..), schemaToValue)

-- ════════════════════════════════════════════════════════════════════════════
-- ANTHROPIC FORMAT
-- ════════════════════════════════════════════════════════════════════════════

-- | Anthropic tool definition.
--
-- Anthropic uses @input_schema@ (not @parameters@) for the JSON Schema.
-- This type ensures tools are serialized correctly for the Anthropic API.
--
-- @
-- AnthropicTool
--   { atName = "search"
--   , atDescription = "Search the knowledge base"
--   , atInputSchema = object
--       [ "type" .= "object"
--       , "properties" .= object [("query", object [("type", "string")])]
--       , "required" .= ["query"]
--       ]
--   }
-- @
--
-- Serializes to:
--
-- @
-- { "name": "search",
--   "description": "Search the knowledge base",
--   "input_schema": { "type": "object", ... }
-- }
-- @
data AnthropicTool = AnthropicTool
  { -- | Tool name (used in tool_use blocks)
    atName :: !Text,
    -- | Human-readable description (shown to LLM)
    atDescription :: !Text,
    -- | JSON Schema for tool input
    atInputSchema :: !Value
  }
  deriving stock (Eq, Show)

instance ToJSON AnthropicTool where
  toJSON t =
    object
      [ "name" .= t.atName,
        "description" .= t.atDescription,
        "input_schema" .= t.atInputSchema
      ]

-- | Convert an Anthropic tool to a JSON Value.
--
-- Equivalent to 'toJSON' but explicit for clarity.
anthropicToolToJSON :: AnthropicTool -> Value
anthropicToolToJSON = toJSON

-- ════════════════════════════════════════════════════════════════════════════
-- OPENAI / CLOUDFLARE AI FORMAT
-- ════════════════════════════════════════════════════════════════════════════

-- | CF AI / OpenAI tool definition.
--
-- CloudFlare Workers AI uses OpenAI-compatible tool format:
-- @{type: "function", function: {name, description, parameters}}@.
--
-- @
-- CfTool
--   { ctName = "ask_user"
--   , ctDescription = "Ask the user a question"
--   , ctParameters = CfObjectSchema
--       { cosProperties = Map.fromList
--           [ ("question", CfString "The question to ask")
--           ]
--       , cosRequired = ["question"]
--       }
--   }
-- @
data CfTool = CfTool
  { ctName :: !Text,
    ctDescription :: !Text,
    ctParameters :: !CfObjectSchema
  }
  deriving (Eq, Show)

-- | Object schema with properties and required fields.
data CfObjectSchema = CfObjectSchema
  { cosProperties :: !(Map Text CfProperty),
    cosRequired :: ![Text]
  }
  deriving (Eq, Show)

-- | Property types supported by CF AI.
data CfProperty
  = -- | String property with description
    CfString !Text
  | -- | Simple string type (no description) - for array items
    CfStringType
  | -- | Integer property with description
    CfInteger !Text
  | -- | Number property with description
    CfNumber !Text
  | -- | Boolean property with description
    CfBoolean !Text
  | -- | Array property with description and item type
    CfArray !Text !CfProperty
  | -- | Nested object
    CfObject !CfObjectSchema
  deriving (Eq, Show)

-- JSON Serialization

instance ToJSON CfTool where
  -- OpenAI-compatible format used by CF AI (llama-3.3, llama-4-scout, etc.)
  toJSON CfTool {..} =
    object
      [ "type" .= ("function" :: Text),
        "function"
          .= object
            [ "name" .= ctName,
              "description" .= ctDescription,
              "parameters" .= ctParameters
            ]
      ]

instance ToJSON CfObjectSchema where
  toJSON CfObjectSchema {..} =
    object
      [ "type" .= ("object" :: Text),
        "properties" .= Map.map toJSON cosProperties,
        "required" .= cosRequired
      ]

instance ToJSON CfProperty where
  toJSON (CfString desc) =
    object
      [ "type" .= ("string" :: Text),
        "description" .= desc
      ]
  toJSON CfStringType =
    object
      [ "type" .= ("string" :: Text)
      ]
  toJSON (CfInteger desc) =
    object
      [ "type" .= ("integer" :: Text),
        "description" .= desc
      ]
  toJSON (CfNumber desc) =
    object
      [ "type" .= ("number" :: Text),
        "description" .= desc
      ]
  toJSON (CfBoolean desc) =
    object
      [ "type" .= ("boolean" :: Text),
        "description" .= desc
      ]
  toJSON (CfArray desc items) =
    object
      [ "type" .= ("array" :: Text),
        "description" .= desc,
        "items" .= items
      ]
  toJSON (CfObject schema) = toJSON schema

-- | Convert tool to aeson Value (for use with llmCall).
cfToolToValue :: CfTool -> Value
cfToolToValue = toJSON

-- ════════════════════════════════════════════════════════════════════════════
-- CONVERSION FROM JSONSCHEMA
-- ════════════════════════════════════════════════════════════════════════════

-- | Create an Anthropic tool from name, description, and typed schema.
--
-- Wraps non-object schemas in a "value" property to ensure valid JSON Schema object.
schemaToAnthropicTool :: Text -> Text -> JSONSchema -> AnthropicTool
schemaToAnthropicTool name desc schema =
  let finalSchema = case schema.schemaType of
        TObject -> schemaToValue schema
        -- Non-object schema: wrap in object with "value" property to satisfy Anthropic requirements
        _ ->
          object
            [ "type" .= ("object" :: Text),
              "properties"
                .= object
                  [ "value" .= schemaToValue schema
                  ],
              "required" .= (["value"] :: [Text]),
              -- Anthropic generally prefers additionalProperties: false
              "additionalProperties" .= False
            ]
   in AnthropicTool
        { atName = name,
          atDescription = desc,
          atInputSchema = finalSchema
        }

-- | Create a CF AI tool from name, description, and typed schema.
schemaToCfTool :: Text -> Text -> JSONSchema -> CfTool
schemaToCfTool name desc schema =
  CfTool
    { ctName = name,
      ctDescription = desc,
      ctParameters = schemaToCfObject schema
    }

-- | Convert a JSONSchema to CfObjectSchema.
--
-- Note: This assumes the schema is an object schema. For non-object schemas,
-- it wraps them in a single-property object.
schemaToCfObject :: JSONSchema -> CfObjectSchema
schemaToCfObject schema = case schema.schemaType of
  TObject ->
    CfObjectSchema
      { cosProperties = Map.map schemaToCfProperty schema.schemaProperties,
        cosRequired = schema.schemaRequired
      }
  -- Non-object schema: wrap in object with "value" property
  _ ->
    CfObjectSchema
      { cosProperties = Map.singleton "value" (schemaToCfProperty schema),
        cosRequired = ["value"]
      }

-- | Convert a JSONSchema to a CfProperty.
schemaToCfProperty :: JSONSchema -> CfProperty
schemaToCfProperty schema = case schema.schemaType of
  TString -> CfString (descOrEmpty schema)
  TInteger -> CfInteger (descOrEmpty schema)
  TNumber -> CfNumber (descOrEmpty schema)
  TBoolean -> CfBoolean (descOrEmpty schema)
  TArray -> case schema.schemaItems of
    Just items -> CfArray (descOrEmpty schema) (schemaToCfProperty items)
    Nothing -> CfArray (descOrEmpty schema) CfStringType -- default to string items
  TObject -> CfObject (schemaToCfObject schema)
  TNull -> CfString (descOrEmpty schema) -- treat null as string
  where
    descOrEmpty s = fromMaybe "" s.schemaDescription
