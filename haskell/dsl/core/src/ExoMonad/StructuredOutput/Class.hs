{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Core types and typeclass for LLM structured output.
--
-- This module contains the fundamental definitions for JSON Schema
-- generation and the 'StructuredOutput' typeclass.
module ExoMonad.StructuredOutput.Class
  ( -- * The Typeclass
    StructuredOutput (..),
    HasJSONSchema (..),

    -- * JSON Schema Types
    JSONSchema (..),
    SchemaType (..),

    -- * Generic Machinery
    GStructuredOutput (..),

    -- * Options
    StructuredOptions (..),
    SumEncoding (..),
    defaultOptions,

    -- * Error Types
    ParseDiagnostic (..),
    formatDiagnostic,

    -- * Validation
    ValidStructuredOutput,
    ValidInContext,
    SchemaContext (..),
    HasSumRep,
    IsNullarySum,

    -- * Wrappers
    StringEnum (..),
    ExoMonadDefault (..),
  )
where

import Data.Aeson (Value)
import Data.Text qualified as T
import Data.Type.Bool (type (&&))
import GHC.Generics (C, D, K1, M1, R, Rep, S, U1, V1, from, to, (:*:), (:+:))
import GHC.TypeLits (ErrorMessage (..), Symbol, TypeError)

-- ════════════════════════════════════════════════════════════════════════════
-- JSON SCHEMA TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | A JSON Schema representation.
data JSONSchema = JSONSchema
  { schemaType :: SchemaType,
    schemaDescription :: Maybe Text,
    schemaProperties :: Map Text JSONSchema,
    schemaRequired :: [Text],
    schemaItems :: Maybe JSONSchema,
    schemaMinItems :: Maybe Int,
    schemaEnum :: Maybe [Text],
    schemaOneOf :: Maybe [JSONSchema]
  }
  deriving (Show, Eq)

-- | JSON Schema primitive types.
data SchemaType
  = TString
  | TNumber
  | TInteger
  | TBoolean
  | TObject
  | TArray
  | TNull
  deriving (Show, Eq)

-- ════════════════════════════════════════════════════════════════════════════
-- ERROR TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Detailed parse diagnostic with field path.
data ParseDiagnostic = ParseDiagnostic
  { pdPath :: [Text],
    pdExpected :: Text,
    pdActual :: Text,
    pdMessage :: Text
  }
  deriving (Show, Eq)

-- | Format diagnostic for display.
formatDiagnostic :: ParseDiagnostic -> Text
formatDiagnostic pd =
  T.unlines
    [ "Parse error at: " <> formatPath pd.pdPath,
      "Expected: " <> pd.pdExpected,
      "Got: " <> pd.pdActual,
      pd.pdMessage
    ]
  where
    formatPath [] = "(root)"
    formatPath ps = T.intercalate "." ps

-- ════════════════════════════════════════════════════════════════════════════
-- WRAPPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Wrapper for enum types that should serialize as plain string enums in JSON Schema.
newtype StringEnum a = StringEnum {unStringEnum :: a}
  deriving (Show, Eq, Generic)

-- | Newtype wrapper for 'DerivingVia' to provide standard ExoMonad instances.
newtype ExoMonadDefault a = ExoMonadDefault {unExoMonadDefault :: a}
  deriving stock (Show, Eq, Generic)

-- ════════════════════════════════════════════════════════════════════════════
-- VALIDATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Context in which a schema is being used.
data SchemaContext
  = ToolInputCtx
  | ToolOutputCtx
  | StructuredOutputCtx

-- | Validate that a type's schema is valid in the given context.
type ValidInContext :: SchemaContext -> Type -> Constraint
type family ValidInContext ctx t where
  ValidInContext 'ToolInputCtx t = ()
  ValidInContext 'ToolOutputCtx t = ()
  ValidInContext 'StructuredOutputCtx t = ValidStructuredOutputImpl t

-- | Constraint alias for structured output validation.
type ValidStructuredOutput :: Type -> Constraint
type ValidStructuredOutput t = ValidInContext 'StructuredOutputCtx t

-- | Implementation of structured output validation.
type family ValidStructuredOutputImpl (t :: Type) :: Constraint where
  -- Blessed types (skip validation)
  ValidStructuredOutputImpl Text = ()
  ValidStructuredOutputImpl String = ()
  ValidStructuredOutputImpl Int = ()
  ValidStructuredOutputImpl Integer = ()
  ValidStructuredOutputImpl Double = ()
  ValidStructuredOutputImpl Bool = ()
  ValidStructuredOutputImpl Value = ()
  ValidStructuredOutputImpl () = ()
  -- Standard containers
  ValidStructuredOutputImpl [a] = ()
  ValidStructuredOutputImpl (Maybe a) = ()
  ValidStructuredOutputImpl (NonEmpty a) = ()
  ValidStructuredOutputImpl (Set a) = ()
  ValidStructuredOutputImpl (StringEnum a) = ()
  ValidStructuredOutputImpl (ExoMonadDefault a) = ValidStructuredOutputImpl a
  -- Tuples
  ValidStructuredOutputImpl (a, b) = ()
  ValidStructuredOutputImpl (a, b, c) = ()
  -- Default: Validate structure
  ValidStructuredOutputImpl t =
    ( CheckNotNonNullarySum (HasSumRep t) (IsNullarySum t) t,
      CheckFieldsValid (Rep t)
    )

-- | Reject sum types with data.
type family CheckNotNonNullarySum (hasSum :: Bool) (isNullary :: Bool) (t :: Type) :: Constraint where
  CheckNotNonNullarySum 'False _ _ = ()
  CheckNotNonNullarySum 'True 'True _ = ()
  CheckNotNonNullarySum 'True 'False t =
    TypeError
      ( 'Text "═══════════════════════════════════════════════════════════"
          ':$$: 'Text "  Schema error for structured output type: "
          ':<>: 'ShowType t
          ':$$: 'Text "═══════════════════════════════════════════════════════"
          ':$$: 'Text ""
          ':$$: 'Text "Anthropic's structured output does not support 'oneOf' schemas."
          ':$$: 'Text "This type uses a sum type with data, which generates oneOf."
          ':$$: 'Text ""
          ':$$: 'Text "Fix options:"
          ':$$: 'Text "  1. Use a tagged record: data MyChoice = MyChoice { tag :: Tag, ... }"
          ':$$: 'Text "  2. Use separate fields: data Output = Output { optionA :: Maybe A, ... }"
          ':$$: 'Text "  3. Convert to enum: remove data from all variants"
          ':$$: 'Text ""
          ':$$: 'Text "Note: Nullary enums (no data) are OK and auto-generate string enums."
      )

-- | Recursively validate field types.
type family CheckFieldsValid (rep :: Type -> Type) :: Constraint where
  CheckFieldsValid (l :*: r) = (CheckFieldsValid l, CheckFieldsValid r)
  CheckFieldsValid (K1 R t) = ValidStructuredOutputImpl t
  CheckFieldsValid (M1 i meta f) = CheckFieldsValid f
  CheckFieldsValid U1 = ()
  CheckFieldsValid V1 = ()
  CheckFieldsValid (l :+: r) = ()

-- | Check if a type is a sum type.
type family HasSumRep (t :: Type) :: Bool where
  HasSumRep (Maybe a) = 'False
  HasSumRep t = IsSumRep (Rep t)

type family IsSumRep (rep :: Type -> Type) :: Bool where
  IsSumRep (l :+: r) = 'True
  IsSumRep (M1 D meta f) = IsSumRep f
  IsSumRep (M1 C meta f) = 'False
  IsSumRep (M1 S meta f) = IsSumRep f
  IsSumRep (l :*: r) = 'False
  IsSumRep (K1 i c) = 'False
  IsSumRep U1 = 'False
  IsSumRep V1 = 'False

-- | Check if a sum type is nullary (enum).
type family IsNullarySum (t :: Type) :: Bool where
  IsNullarySum t = IsNullarySumRep (Rep t)

type family IsNullarySumRep (rep :: Type -> Type) :: Bool where
  IsNullarySumRep (l :+: r) = IsNullarySumRep l && IsNullarySumRep r
  IsNullarySumRep (M1 i meta f) = IsNullarySumRep f
  IsNullarySumRep U1 = 'True
  IsNullarySumRep (l :*: r) = 'False
  IsNullarySumRep (K1 i c) = 'False
  IsNullarySumRep _ = 'False

-- ════════════════════════════════════════════════════════════════════════════
-- THE TYPECLASS
-- ════════════════════════════════════════════════════════════════════════════

-- | Typeclass for types that have a JSON Schema representation.
class HasJSONSchema a where
  jsonSchema :: JSONSchema

-- | Evidence that a type can be used as LLM structured output.
class StructuredOutput a where
  -- | JSON Schema for this type.
  structuredSchema :: JSONSchema

  -- | Encode a value to JSON.
  encodeStructured :: a -> Value

  -- | Parse a JSON value with detailed diagnostics.
  parseStructured :: Value -> Either ParseDiagnostic a

  -- | Options controlling encoding/decoding behavior.
  structuredOptions :: StructuredOptions
  structuredOptions = defaultOptions

  -- Default implementations via Generic
  default structuredSchema ::
    (GStructuredOutput (Rep a)) =>
    JSONSchema
  structuredSchema = gStructuredSchema @(Rep a) (structuredOptions @a)

  default encodeStructured ::
    (Generic a, GStructuredOutput (Rep a)) =>
    a -> Value
  encodeStructured x = gEncodeStructured (structuredOptions @a) (from x)

  default parseStructured ::
    (Generic a, GStructuredOutput (Rep a)) =>
    Value -> Either ParseDiagnostic a
  parseStructured v = to <$> gParseStructured (structuredOptions @a) [] v

-- | Generic class for structured output derivation.
class GStructuredOutput (f :: Type -> Type) where
  gStructuredSchema :: StructuredOptions -> JSONSchema
  gEncodeStructured :: StructuredOptions -> f p -> Value
  gParseStructured :: StructuredOptions -> [Text] -> Value -> Either ParseDiagnostic (f p)

-- ════════════════════════════════════════════════════════════════════════════
-- OPTIONS
-- ════════════════════════════════════════════════════════════════════════════

-- | Options for controlling encoding/decoding behavior.
data StructuredOptions = StructuredOptions
  { -- | Transform record field names to JSON keys.
    soFieldLabelModifier :: String -> String,
    -- | Transform constructor names for sum type tags.
    soConstructorTagModifier :: String -> String,
    -- | Omit fields with 'Nothing' values from output.
    soOmitNothingFields :: Bool,
    -- | How to encode sum types.
    soSumEncoding :: SumEncoding
  }

-- | How to encode sum types in JSON.
data SumEncoding
  = TaggedObject String String
  | ObjectWithSingleField
  | TwoElemArray
  deriving (Show, Eq)

-- | Default options.
defaultOptions :: StructuredOptions
defaultOptions =
  StructuredOptions
    { soFieldLabelModifier = id,
      soConstructorTagModifier = id,
      soOmitNothingFields = True,
      soSumEncoding = TaggedObject "tag" "contents"
    }
