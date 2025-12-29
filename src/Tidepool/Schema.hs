-- | JSON Schema combinators for structured output
module Tidepool.Schema
  ( -- * Schema Type
    JSONSchema(..)
  , SchemaType(..)

    -- * Schema Combinators
  , objectSchema
  , arraySchema
  , enumSchema
  , oneOfSchema
  , nullableSchema
  , describeField
  , emptySchema

    -- * Conversion
  , schemaToValue
  ) where

import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Aeson (Value(..), object, (.=))

-- | A JSON Schema
data JSONSchema = JSONSchema
  { schemaType :: SchemaType
  , schemaDescription :: Maybe Text
  , schemaProperties :: Map Text JSONSchema
  , schemaRequired :: [Text]
  , schemaItems :: Maybe JSONSchema
  , schemaEnum :: Maybe [Text]
  , schemaOneOf :: Maybe [JSONSchema]
  }
  deriving (Show, Eq)

data SchemaType
  = TString
  | TNumber
  | TInteger
  | TBoolean
  | TObject
  | TArray
  | TNull
  deriving (Show, Eq)

-- | Empty schema of given type
emptySchema :: SchemaType -> JSONSchema
emptySchema t = JSONSchema t Nothing Map.empty [] Nothing Nothing Nothing

-- | Object schema combinator
objectSchema :: [(Text, JSONSchema)] -> [Text] -> JSONSchema
objectSchema props required = (emptySchema TObject)
  { schemaProperties = Map.fromList props
  , schemaRequired = required
  }

-- | Array schema combinator
arraySchema :: JSONSchema -> JSONSchema
arraySchema items = (emptySchema TArray) { schemaItems = Just items }

-- | Enum schema combinator
enumSchema :: [Text] -> JSONSchema
enumSchema variants = (emptySchema TString) { schemaEnum = Just variants }

-- | OneOf schema combinator (for sum types)
oneOfSchema :: [JSONSchema] -> JSONSchema
oneOfSchema variants = (emptySchema TObject) { schemaOneOf = Just variants }

-- | Nullable schema combinator (allows null or the given type)
nullableSchema :: JSONSchema -> JSONSchema
nullableSchema inner = oneOfSchema [inner, emptySchema TNull]

-- | Add description to schema
describeField :: Text -> Text -> JSONSchema -> JSONSchema
describeField _fieldName desc schema = schema { schemaDescription = Just desc }

-- | Convert schema to Aeson Value (JSON Schema draft-07 format)
-- Note: Anthropic API requires additionalProperties: false on all object types
schemaToValue :: JSONSchema -> Value
schemaToValue (JSONSchema typ desc props req items enum_ oneOf_) = object $ catMaybes
  [ Just $ "type" .= typeToText typ
  , ("description" .=) <$> desc
  , if Map.null props
    then Nothing
    else Just $ "properties" .= fmap schemaToValue props
  , if null req
    then Nothing
    else Just $ "required" .= req
  -- Anthropic requires additionalProperties: false for all object types
  , if typ == TObject
    then Just $ "additionalProperties" .= False
    else Nothing
  , ("items" .=) . schemaToValue <$> items
  , ("enum" .=) <$> enum_
  , ("oneOf" .=) . fmap schemaToValue <$> oneOf_
  ]

-- | Convert schema type to JSON Schema type string
typeToText :: SchemaType -> Text
typeToText = \case
  TString  -> "string"
  TNumber  -> "number"
  TInteger -> "integer"
  TBoolean -> "boolean"
  TObject  -> "object"
  TArray   -> "array"
  TNull    -> "null"
