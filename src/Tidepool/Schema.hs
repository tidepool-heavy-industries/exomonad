{-# LANGUAGE AllowAmbiguousTypes #-}
-- | JSON Schema derivation for structured output
module Tidepool.Schema
  ( -- * Schema Type
    JSONSchema(..)
  , SchemaType(..)
  
    -- * Derivation
  , deriveSchema
  , GSchema(..)
  
    -- * Schema Combinators
  , objectSchema
  , arraySchema
  , enumSchema
  , oneOfSchema
  , describeField
  , emptySchema
  
    -- * Conversion
  , schemaToValue
  ) where

import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import GHC.Generics
import Data.Aeson (Value(..), ToJSON(..), object, (.=))

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

-- | Typeclass for Generic schema derivation
class GSchema f where
  gSchema :: JSONSchema

-- | Derive schema from Generic instance
deriveSchema :: forall a. (Generic a, GSchema (Rep a)) => JSONSchema
deriveSchema = error "TODO: deriveSchema - use GHC.Generics to build JSONSchema from type structure"

-- Generic instances
instance GSchema V1 where
  gSchema = error "TODO: GSchema V1"

instance GSchema U1 where
  gSchema = error "TODO: GSchema U1"

instance (GSchema f, GSchema g) => GSchema (f :+: g) where
  gSchema = error "TODO: GSchema sum"

instance (GSchema f, GSchema g) => GSchema (f :*: g) where
  gSchema = error "TODO: GSchema product"

instance GSchema f => GSchema (M1 i c f) where
  gSchema = error "TODO: GSchema M1"

instance GSchema (K1 i c) where
  gSchema = error "TODO: GSchema K1"
