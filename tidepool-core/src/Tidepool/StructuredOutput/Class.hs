{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}

-- | The StructuredOutput typeclass for LLM structured output.
--
-- Provides a unified interface for:
-- - JSON Schema generation (for LLM constraints)
-- - JSON encoding (for serialization)
-- - JSON parsing (with detailed diagnostics)
--
-- All three are derived from a single Generic traversal, so application
-- types need only @deriving Generic@.
--
-- @
-- data TypeDefinitions = TypeDefinitions
--   { tdTypeName :: Text
--   , tdDataType :: Text
--   }
--   deriving stock (Generic)
--
-- instance StructuredOutput TypeDefinitions
-- -- That's it! Schema, encode, and parse are all derived.
-- @
module Tidepool.StructuredOutput.Class
  ( -- * The Typeclass
    StructuredOutput(..)

    -- * Generic Machinery
  , GStructuredOutput(..)

    -- * Options
  , StructuredOptions(..)
  , SumEncoding(..)
  , defaultOptions
  ) where

import Data.Aeson (Value)
import Data.Kind (Type)
import Data.Text (Text)
import GHC.Generics (Generic, Rep, from, to)

import Tidepool.Schema (JSONSchema)
import Tidepool.StructuredOutput.Error (ParseDiagnostic)
import Tidepool.StructuredOutput.Prefix (stripFieldPrefix)


-- | Evidence that a type can be used as LLM structured output.
--
-- Provides schema generation, encoding, and parsing from a single
-- Generic representation. Types need only derive Generic.
--
-- == Minimal Instance
--
-- For types with @Generic@ instances, you can use an empty instance:
--
-- @
-- data MyType = MyType { mtField :: Text }
--   deriving stock (Generic)
--
-- instance StructuredOutput MyType
-- @
--
-- All methods have default implementations via 'GStructuredOutput'.
--
-- == JSON Field Names
--
-- By default, field names have their common lowercase prefix stripped:
--
-- @
-- data Example = Example { exName :: Text, exAge :: Int }
-- -- JSON keys: "name", "age" (prefix "ex" stripped)
-- @
--
-- == Custom Options
--
-- Override 'structuredOptions' to customize behavior:
--
-- @
-- instance StructuredOutput MyType where
--   structuredOptions = defaultOptions { soOmitNothingFields = False }
-- @
class StructuredOutput a where
  -- | JSON Schema for this type.
  --
  -- Used to constrain LLM structured output.
  structuredSchema :: JSONSchema

  -- | Encode a value to JSON.
  encodeStructured :: a -> Value

  -- | Parse a JSON value with detailed diagnostics.
  --
  -- Returns 'Left' with path information on failure.
  parseStructured :: Value -> Either ParseDiagnostic a

  -- | Options controlling encoding/decoding behavior.
  --
  -- Override to customize field naming, sum type encoding, etc.
  structuredOptions :: StructuredOptions
  structuredOptions = defaultOptions

  -- Default implementations via Generic
  -- These are filled in by Generic.hs which imports this module
  default structuredSchema
    :: (Generic a, GStructuredOutput (Rep a))
    => JSONSchema
  structuredSchema = gStructuredSchema @(Rep a) (structuredOptions @a)

  default encodeStructured
    :: (Generic a, GStructuredOutput (Rep a))
    => a -> Value
  encodeStructured x = gEncodeStructured (structuredOptions @a) (from x)

  default parseStructured
    :: (Generic a, GStructuredOutput (Rep a))
    => Value -> Either ParseDiagnostic a
  parseStructured v = to <$> gParseStructured (structuredOptions @a) [] v


-- | Generic class for structured output derivation.
--
-- This is the workhorse that walks the Generic representation.
-- Instances are defined in "Tidepool.StructuredOutput.Generic".
class GStructuredOutput (f :: Type -> Type) where
  gStructuredSchema :: StructuredOptions -> JSONSchema
  gEncodeStructured :: StructuredOptions -> f p -> Value
  gParseStructured :: StructuredOptions -> [Text] -> Value -> Either ParseDiagnostic (f p)


-- ════════════════════════════════════════════════════════════════════════════
-- OPTIONS
-- ════════════════════════════════════════════════════════════════════════════

-- | Options for controlling encoding/decoding behavior.
data StructuredOptions = StructuredOptions
  { soFieldLabelModifier :: String -> String
    -- ^ Transform record field names to JSON keys.
    -- Default: 'stripFieldPrefix' (removes common lowercase prefix).
  , soConstructorTagModifier :: String -> String
    -- ^ Transform constructor names for sum type tags.
    -- Default: 'id' (no change).
  , soOmitNothingFields :: Bool
    -- ^ Omit fields with 'Nothing' values from output.
    -- Default: 'True'.
  , soSumEncoding :: SumEncoding
    -- ^ How to encode sum types.
    -- Default: @'TaggedObject' "tag" "contents"@.
  }

-- | How to encode sum types in JSON.
data SumEncoding
  = TaggedObject String String
    -- ^ @TaggedObject tagField contentsField@
    -- Encodes as @{"tag": "Constructor", "contents": ...}@
  | ObjectWithSingleField
    -- ^ Encodes as @{"Constructor": ...}@
  | TwoElemArray
    -- ^ Encodes as @["Constructor", ...]@
  deriving (Show, Eq)

-- | Default options.
--
-- - Field labels: Strip common lowercase prefix ('stripFieldPrefix')
-- - Constructor tags: No modification
-- - Omit Nothing: True
-- - Sum encoding: Tagged object with "tag" and "contents"
defaultOptions :: StructuredOptions
defaultOptions = StructuredOptions
  { soFieldLabelModifier = stripFieldPrefix
  , soConstructorTagModifier = id
  , soOmitNothingFields = True
  , soSumEncoding = TaggedObject "tag" "contents"
  }
