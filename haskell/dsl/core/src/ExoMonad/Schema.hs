{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | JSON Schema combinators for structured output
module ExoMonad.Schema
  ( -- * Schema Type
    JSONSchema (..),
    SchemaType (..),

    -- * HasJSONSchema Typeclass
    HasJSONSchema (..),

    -- * Schema Combinators
    objectSchema,
    arraySchema,
    enumSchema,
    oneOfSchema,
    describeField,
    emptySchema,

    -- * Schema Marker Traits
    UsesOneOf,
    UsesEnum,
    IsMarkedOneOf,
    HasUsesOneOf,
    HasUsesEnum,

    -- * Contextual Validation
    SchemaContext (..),
    ValidInContext,
    ValidStructuredOutput,

    -- * TH Derivation
    deriveJSONSchema,
    deriveHasJSONSchema,
    deriveUsesOneOf,
    deriveUsesEnum,
    deriveMCPType,
    deriveMCPTypeWith,
    deriveMCPEnum,
    FieldMapping (..),
    FieldMappingPartial,
    (~>),
    (?),
    (??),
    omit,
    MCPOptions (..),
    defaultMCPOptions,

    -- * Conversion
    schemaToValue,

    -- * Re-exports from Class
    ExoMonadDefault (..),
    StringEnum (..),
    ParseDiagnostic (..),
    formatDiagnostic,
  )
where

import Prelude hiding ((??))

import Data.Aeson (Value (..), object, (.=))
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Schema.TH
  ( FieldMapping (..),
    FieldMappingPartial,
    MCPOptions (..),
    defaultMCPOptions,
    deriveMCPEnum,
    deriveMCPType,
    deriveMCPTypeWith,
    omit,
    (?),
    (??),
    (~>),
  )
-- Import wrappers and validation from Class to avoid circularity issues
-- and make them available for WASM.
import ExoMonad.StructuredOutput.Class
  ( ExoMonadDefault (..),
    HasJSONSchema (..),
    JSONSchema (..),
    ParseDiagnostic (..),
    SchemaContext (..),
    SchemaType (..),
    StringEnum (..),
    ValidInContext,
    ValidStructuredOutput,
    formatDiagnostic,
  )
import Language.Haskell.TH hiding (Type)
import Language.Haskell.TH.Syntax qualified as TH

-- | Empty schema of given type
emptySchema :: SchemaType -> JSONSchema
emptySchema t = JSONSchema t Nothing Map.empty [] Nothing Nothing Nothing Nothing

-- | Object schema combinator
objectSchema :: [(Text, JSONSchema)] -> [Text] -> JSONSchema
objectSchema props required =
  (emptySchema TObject)
    { schemaProperties = Map.fromList props,
      schemaRequired = required
    }

-- | Array schema combinator
arraySchema :: JSONSchema -> JSONSchema
arraySchema items = (emptySchema TArray) {schemaItems = Just items}

-- | Enum schema combinator
enumSchema :: [Text] -> JSONSchema
enumSchema variants = (emptySchema TString) {schemaEnum = Just variants}

-- | OneOf schema combinator (for sum types)
oneOfSchema :: [JSONSchema] -> JSONSchema
oneOfSchema variants = (emptySchema TObject) {schemaOneOf = Just variants}

-- | Add description to schema
describeField :: Text -> JSONSchema -> JSONSchema
describeField desc schema = schema {schemaDescription = Just desc}

-- | Convert schema to Aeson Value (JSON Schema draft-07 format)
-- Note: Anthropic API requires additionalProperties: false on all object types
schemaToValue :: JSONSchema -> Value
schemaToValue (JSONSchema typ desc props req items minItems_ enum_ oneOf_) =
  object $
    catMaybes
      [ Just $ "type" .= typeToText typ,
        ("description" .=) <$> desc,
        if Map.null props
          then Nothing
          else Just $ "properties" .= fmap schemaToValue props,
        if null req
          then Nothing
          else Just $ "required" .= req,
        -- Anthropic requires additionalProperties: false for all object types
        if typ == TObject
          then Just $ "additionalProperties" .= False
          else Nothing,
        ("items" .=) . schemaToValue <$> items,
        ("minItems" .=) <$> minItems_,
        ("enum" .=) <$> enum_,
        ("oneOf" .=) . fmap schemaToValue <$> oneOf_
      ]

-- | Convert schema type to JSON Schema type string
typeToText :: SchemaType -> Text
typeToText = \case
  TString -> "string"
  TNumber -> "number"
  TInteger -> "integer"
  TBoolean -> "boolean"
  TObject -> "object"
  TArray -> "array"
  TNull -> "null"

-- ══════════════════════════════════════════════════════════════
-- TEMPLATE HASKELL DERIVATION
-- ══════════════════════════════════════════════════════════════

-- | Derive a JSONSchema from a record type, using Haddock comments as descriptions.
deriveJSONSchema :: Name -> Q Exp
deriveJSONSchema typeName = do
  info <- reify typeName
  case info of
    -- Single-constructor record
    TyConI (DataD _ _ _ _ [RecC conName fields] _) -> deriveFromFields conName fields
    -- Newtype with record syntax
    TyConI (NewtypeD _ _ _ _ (RecC conName fields) _) -> deriveFromFields conName fields
    -- Enum: all nullary constructors
    TyConI (DataD _ _ _ _ cons _) | all isNullaryCon cons -> deriveEnum cons
    -- Unsupported
    _ -> fail $ "deriveJSONSchema: " ++ show typeName ++ " must be a record type or enum (all nullary constructors)"
  where
    deriveFromFields conName fields = do
      -- Derive schemas for each field, requiring Haddock docs
      fieldSchemas <-
        sequence
          [ deriveFieldSchema typeName conName idx field
          | (idx, field) <- zip [0 ..] fields
          ]
      let fieldInfo = [(nameBase name, typ) | (name, _, typ) <- fields]
          propsExpr =
            listE
              [ tupE [litE (stringL fn), pure fs]
              | ((fn, _), fs) <- zip fieldInfo fieldSchemas
              ]
          -- Only require non-Maybe fields
          reqExpr =
            listE
              [ litE (stringL fn)
              | (fn, typ) <- fieldInfo,
                not (isMaybeType typ)
              ]
      [|objectSchema (map (\(n, s) -> (T.pack n, s)) $propsExpr) (map T.pack $reqExpr)|]

    deriveEnum cons = do
      let names = [nameBase n | NormalC n [] <- cons]
      [|enumSchema $(listE [litE (stringL n) | n <- names])|]

    isNullaryCon (NormalC _ []) = True
    isNullaryCon _ = False

    isMaybeType (AppT (ConT name) _) = nameBase name == "Maybe"
    isMaybeType _ = False

-- | Derive schema for a single field, requiring Haddock documentation.
deriveFieldSchema :: Name -> Name -> Int -> VarBangType -> Q Exp
deriveFieldSchema _typeName _conName _fieldIdx (fieldName, _, fieldType) = do
  mDoc <- getDoc (DeclDoc fieldName)
  desc <- case mDoc of
    Just doc -> pure (T.pack doc)
    Nothing -> pure ""
  baseSchema <- typeToSchemaExp fieldType
  [|describeField desc $(pure baseSchema)|]

-- | Convert a Haskell type to a JSONSchema expression
typeToSchemaExp :: TH.Type -> Q Exp
typeToSchemaExp typ = case typ of
  -- Text, String, FilePath -> TString
  ConT name
    | nameBase name `elem` ["Text", "String", "FilePath"] ->
        [|emptySchema TString|]
  -- Int, Integer -> TInteger
  ConT name
    | nameBase name `elem` ["Int", "Integer"] ->
        [|emptySchema TInteger|]
  -- Double, Float -> TNumber
  ConT name
    | nameBase name `elem` ["Double", "Float"] ->
        [|emptySchema TNumber|]
  -- Bool -> TBoolean
  ConT name | nameBase name == "Bool" -> [|emptySchema TBoolean|]
  -- [a] -> array of a
  AppT ListT elemType -> do
    elemSchema <- typeToSchemaExp elemType
    [|arraySchema $(pure elemSchema)|]
  -- Maybe a -> same as a (optional handled by not being in required)
  AppT (ConT name) innerType
    | nameBase name == "Maybe" ->
        typeToSchemaExp innerType
  -- Named type with HasJSONSchema instance -> use its schema
  ConT name -> do
    [|jsonSchema @($(conT name))|]
  -- Fallback: treat as string (for complex types without HasJSONSchema)
  _ -> [|emptySchema TString|]

-- ════════════════════════════════════════════════════════════════════════════
-- HAS JSON SCHEMA TYPECLASS
-- ════════════════════════════════════════════════════════════════════════════

-- | Derive a 'HasJSONSchema' instance for a record type.
deriveHasJSONSchema :: Name -> Q [Dec]
deriveHasJSONSchema typeName = do
  schemaExp <- deriveJSONSchema typeName
  [d|
    instance HasJSONSchema $(conT typeName) where
      jsonSchema = $(pure schemaExp)
    |]

-- ══════════════════════════════════════════════════════════════
-- SCHEMA MARKER TRAITS
-- ══════════════════════════════════════════════════════════════

-- | Marker class for types whose JSON schema uses oneOf.
class UsesOneOf a

-- | Open type family for types explicitly marked as using oneOf.
type family IsMarkedOneOf (a :: Type) :: Bool

-- | Marker class for types whose JSON schema uses enum.
class UsesEnum a

type family HasUsesOneOf a where
  HasUsesOneOf a = IsMarkedOneOf a

type family HasUsesEnum a :: Bool where
  HasUsesEnum a = 'False

-- ══════════════════════════════════════════════════════════════
-- TH MARKER DERIVATION
-- ══════════════════════════════════════════════════════════════

-- | Derive a 'UsesOneOf' instance for a type.
deriveUsesOneOf :: Name -> Q [Dec]
deriveUsesOneOf typeName = do
  markerInst <- [d|instance UsesOneOf $(conT typeName)|]
  let markedInst =
        TySynInstD
          ( TySynEqn
              Nothing
              (AppT (ConT ''IsMarkedOneOf) (ConT typeName))
              (PromotedT 'True)
          )
  pure (markerInst ++ [markedInst])

-- | Derive a 'UsesEnum' instance for a type.
deriveUsesEnum :: Name -> Q [Dec]
deriveUsesEnum typeName = do
  [d|instance UsesEnum $(conT typeName)|]
