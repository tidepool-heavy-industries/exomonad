{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | CF AI tool schema types.
--
-- Cloudflare Workers AI uses a flat tool format (unlike OpenAI's nested
-- @{type: "function", function: {...}}@ wrapper). These types ensure
-- tools are serialized in the correct format.
--
-- Example:
--
-- @
-- askUserTool :: CfTool
-- askUserTool = CfTool
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
--
-- Serializes to:
--
-- @
-- {
--   "name": "ask_user",
--   "description": "Ask the user a question",
--   "parameters": {
--     "type": "object",
--     "properties": {
--       "question": { "type": "string", "description": "The question to ask" }
--     },
--     "required": ["question"]
--   }
-- }
-- @
module Tidepool.Wasm.CfTool
  ( -- * Tool Schema
    CfTool(..)
  , CfObjectSchema(..)
  , CfProperty(..)
  , cfToolToValue
  ) where

import Data.Aeson (ToJSON(..), Value, object, (.=))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)


-- | CF AI tool definition (flat format).
data CfTool = CfTool
  { ctName        :: !Text
  , ctDescription :: !Text
  , ctParameters  :: !CfObjectSchema
  }
  deriving (Eq, Show)

-- | Object schema with properties and required fields.
data CfObjectSchema = CfObjectSchema
  { cosProperties :: !(Map Text CfProperty)
  , cosRequired   :: ![Text]
  }
  deriving (Eq, Show)

-- | Property types supported by CF AI.
data CfProperty
  = CfString !Text
    -- ^ String property with description
  | CfStringType
    -- ^ Simple string type (no description) - for array items
  | CfArray !Text !CfProperty
    -- ^ Array property with description and item type
  | CfObject !CfObjectSchema
    -- ^ Nested object
  deriving (Eq, Show)


-- ════════════════════════════════════════════════════════════════════════════
-- JSON SERIALIZATION
-- ════════════════════════════════════════════════════════════════════════════

instance ToJSON CfTool where
  -- Use OpenAI-compatible format: {type: "function", function: {...}}
  -- Some CF AI models (like llama-4-scout) require this format
  toJSON CfTool{..} = object
    [ "type" .= ("function" :: Text)
    , "function" .= object
        [ "name" .= ctName
        , "description" .= ctDescription
        , "parameters" .= ctParameters
        ]
    ]

instance ToJSON CfObjectSchema where
  toJSON CfObjectSchema{..} = object
    [ "type" .= ("object" :: Text)
    , "properties" .= Map.map toJSON cosProperties
    , "required" .= cosRequired
    ]

instance ToJSON CfProperty where
  toJSON (CfString desc) = object
    [ "type" .= ("string" :: Text)
    , "description" .= desc
    ]
  toJSON CfStringType = object
    [ "type" .= ("string" :: Text)
    ]
  toJSON (CfArray desc items) = object
    [ "type" .= ("array" :: Text)
    , "description" .= desc
    , "items" .= items
    ]
  toJSON (CfObject schema) = toJSON schema


-- | Convert tool to aeson Value (for use with llmCall).
cfToolToValue :: CfTool -> Value
cfToolToValue = toJSON
