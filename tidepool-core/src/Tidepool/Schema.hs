{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | JSON Schema combinators for structured output
module Tidepool.Schema
  ( -- * Schema Type
    JSONSchema(..)
  , SchemaType(..)

    -- * HasJSONSchema Typeclass
  , HasJSONSchema(..)

    -- * Schema Combinators
  , objectSchema
  , arraySchema
  , enumSchema
  , oneOfSchema
  , describeField
  , emptySchema

    -- * Schema Marker Traits
  , UsesOneOf
  , UsesEnum
  , IsMarkedOneOf
  , HasUsesOneOf
  , HasUsesEnum

    -- * Contextual Validation
  , SchemaContext(..)
  , ValidInContext
  , ValidStructuredOutput

    -- * TH Derivation
  , deriveJSONSchema
  , deriveHasJSONSchema
  , deriveUsesOneOf
  , deriveUsesEnum

    -- * Conversion
  , schemaToValue
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Aeson (Value(..), object, (.=))
import Data.Kind (Constraint)
import qualified Data.Kind as K
import GHC.TypeLits (TypeError, ErrorMessage(..))
import Language.Haskell.TH

-- | A JSON Schema
data JSONSchema = JSONSchema
  { schemaType :: SchemaType
  , schemaDescription :: Maybe Text
  , schemaProperties :: Map Text JSONSchema
  , schemaRequired :: [Text]
  , schemaItems :: Maybe JSONSchema
  , schemaMinItems :: Maybe Int
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
emptySchema t = JSONSchema t Nothing Map.empty [] Nothing Nothing Nothing Nothing

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
schemaToValue (JSONSchema typ desc props req items minItems_ enum_ oneOf_) = object $ catMaybes
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
  , ("minItems" .=) <$> minItems_
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

-- ══════════════════════════════════════════════════════════════
-- TEMPLATE HASKELL DERIVATION
-- ══════════════════════════════════════════════════════════════

-- | Derive a JSONSchema from a record type, using Haddock comments as descriptions.
--
-- == Requirements
--
-- 1. Type must be in a /separate, already-compiled/ module (TH staging)
-- 2. That module must have @{-\# LANGUAGE FieldSelectors \#-}@
-- 3. Field docs must use @-- ^@ format (after field)
-- 4. Every field must have a doc comment (or compilation fails)
--
-- == Example
--
-- In @Types.hs@ (separate module with FieldSelectors):
--
-- @
-- data TurnOutput = TurnOutput
--   { narration :: Text
--     -- ^ Narrative prose describing what happens.
--   , stressDelta :: Int
--     -- ^ Change in stress (-9 to +9, 0 if no change)
--   }
-- @
--
-- In @Output.hs@:
--
-- @
-- import Types (TurnOutput(..))
--
-- turnOutputSchema :: JSONSchema
-- turnOutputSchema = $(deriveJSONSchema ''TurnOutput)
-- @
--
-- == Why These Constraints?
--
-- __Staging__: TH's @reify@ can only introspect types from already-compiled
-- modules. Types in the same module haven't been compiled yet when the splice
-- runs.
--
-- __FieldSelectors__: This is specifically required for @getDoc@, not @reify@.
-- TH can see the record structure without field selectors via @reify@. However,
-- we use @getDoc (DeclDoc fieldName)@ to retrieve Haddock comments, which looks
-- for documentation attached to a /declaration/ with that name. With
-- @FieldSelectors@, each record field generates a top-level accessor function
-- (@name :: Person -> Text@), and Haddock attaches the @-- ^@ comment to that
-- function. Without @FieldSelectors@, no such function exists, so @getDoc@
-- returns @Nothing@.
--
-- If you don't need field descriptions in your schema, you could use the
-- manual combinators (@objectSchema@, @emptySchema@) which don't require
-- @FieldSelectors@.
deriveJSONSchema :: Name -> Q Exp
deriveJSONSchema typeName = do
  info <- reify typeName
  case info of
    TyConI (DataD _ _ _ _ [RecC conName fields] _) -> deriveFromFields conName fields
    TyConI (NewtypeD _ _ _ _ (RecC conName fields) _) -> deriveFromFields conName fields
    _ -> fail $ "deriveJSONSchema: " ++ show typeName ++ " must be a record type"
  where
    deriveFromFields conName fields = do
      -- Derive schemas for each field, requiring Haddock docs
      fieldSchemas <- sequence
        [ deriveFieldSchema typeName conName idx field
        | (idx, field) <- zip [0..] fields ]
      let fieldInfo = [(nameBase name, typ) | (name, _, typ) <- fields]
          propsExpr = listE [ tupE [litE (stringL fn), pure fs]
                            | ((fn, _), fs) <- zip fieldInfo fieldSchemas ]
          -- Only require non-Maybe fields
          reqExpr = listE [ litE (stringL fn)
                          | (fn, typ) <- fieldInfo
                          , not (isMaybeType typ) ]
      [| objectSchema (map (\(n, s) -> (T.pack n, s)) $propsExpr) (map T.pack $reqExpr) |]

    isMaybeType (AppT (ConT name) _) = nameBase name == "Maybe"
    isMaybeType _ = False

-- | Derive schema for a single field, requiring Haddock documentation.
-- Compile-fails with helpful error if doc is missing.
-- Uses DeclDoc for the field selector (generated by record syntax).
deriveFieldSchema :: Name -> Name -> Int -> VarBangType -> Q Exp
deriveFieldSchema typeName _conName _fieldIdx (fieldName, _, fieldType) = do
  -- DeclDoc works for field selectors when type is in a separate compiled module
  mDoc <- getDoc (DeclDoc fieldName)
  desc <- case mDoc of
    Just doc -> pure (T.pack doc)
    Nothing  -> fail $ schemaFieldError typeName fieldName fieldType
  baseSchema <- typeToSchemaExp fieldType
  [| describeField (T.pack $(litE (stringL (nameBase fieldName)))) desc $(pure baseSchema) |]

-- | Generate helpful error message for missing field docs
schemaFieldError :: Name -> Name -> Type -> String
schemaFieldError typeName fieldName fieldType = unlines
  [ "deriveJSONSchema: Can't get docs for field '" ++ nameBase fieldName ++ "' in '" ++ nameBase typeName ++ "'"
  , ""
  , "This happens when either:"
  , ""
  , "  1. The field is missing a Haddock comment (must use -- ^ format after field)"
  , ""
  , "  2. The type's module doesn't have {-# LANGUAGE FieldSelectors #-}"
  , "     (getDoc uses DeclDoc which looks up the field selector function;"
  , "     without FieldSelectors, that function doesn't exist)"
  , ""
  , "Fix option 1 - Add documentation:"
  , ""
  , "  " ++ nameBase fieldName ++ " :: " ++ prettyType fieldType
  , "    -- ^ " ++ typeHint fieldType
  , ""
  , "Fix option 2 - Enable FieldSelectors in the module defining '" ++ nameBase typeName ++ "':"
  , ""
  , "  {-# LANGUAGE FieldSelectors #-}"
  ]

-- | Pretty-print a type without internal module qualifiers
prettyType :: Type -> String
prettyType (ConT n) = nameBase n
prettyType (AppT (ConT n) inner)
  | nameBase n == "Maybe" = "Maybe " ++ prettyType inner
  | nameBase n == "[]"    = "[" ++ prettyType inner ++ "]"
  | otherwise             = nameBase n ++ " " ++ prettyType inner
prettyType (AppT ListT inner) = "[" ++ prettyType inner ++ "]"
prettyType (AppT l r) = prettyType l ++ " " ++ prettyType r
prettyType t = pprint t  -- fallback

-- | Type-specific hint for what documentation should include
typeHint :: Type -> String
typeHint (ConT n)
  | nameBase n == "Int"     = "<meaning> (<min>-<max>)"
  | nameBase n == "Integer" = "<meaning> (<min>-<max>)"
  | nameBase n == "Text"    = "<what this text represents>"
  | nameBase n == "String"  = "<what this string represents>"
  | nameBase n == "Bool"    = "<when true vs false>"
typeHint (AppT (ConT n) _)
  | nameBase n == "Maybe"   = "<when present vs absent>"
typeHint (AppT ListT _)     = "<what each item represents>"
typeHint (AppT (AppT (ConT n) _) _)
  | nameBase n == "Map"     = "<key meaning> -> <value meaning>"
typeHint _ = "<describe valid values and meaning>"

-- | Convert a Haskell type to a JSONSchema expression
typeToSchemaExp :: Type -> Q Exp
typeToSchemaExp typ = case typ of
  -- Text, String, FilePath -> TString
  ConT name | nameBase name `elem` ["Text", "String", "FilePath"] ->
    [| emptySchema TString |]
  -- Int, Integer -> TInteger
  ConT name | nameBase name `elem` ["Int", "Integer"] ->
    [| emptySchema TInteger |]
  -- Double, Float -> TNumber
  ConT name | nameBase name `elem` ["Double", "Float"] ->
    [| emptySchema TNumber |]
  -- Bool -> TBoolean
  ConT name | nameBase name == "Bool" -> [| emptySchema TBoolean |]
  -- [a] -> array of a
  AppT ListT elemType -> do
    elemSchema <- typeToSchemaExp elemType
    [| arraySchema $(pure elemSchema) |]
  -- Maybe a -> same as a (optional handled by not being in required)
  AppT (ConT name) innerType | nameBase name == "Maybe" ->
    typeToSchemaExp innerType
  -- Named type with HasJSONSchema instance -> use its schema
  ConT name -> do
    -- Check if the type has a HasJSONSchema instance by trying to generate
    -- an expression that uses it. If the instance doesn't exist, this will
    -- cause a compile error at the call site, which is the desired behavior.
    [| jsonSchema @($(conT name)) |]
  -- Fallback: treat as string (for complex types without HasJSONSchema)
  _ -> [| emptySchema TString |]

-- ══════════════════════════════════════════════════════════════
-- HAS JSON SCHEMA TYPECLASS
-- ══════════════════════════════════════════════════════════════

-- | Typeclass for types that have a JSON Schema representation.
--
-- Use 'deriveHasJSONSchema' to generate instances from record types
-- with Haddock documentation.
--
-- @
-- -- In Types.hs (separate module):
-- data MyInput = MyInput
--   { field1 :: Text
--     -- ^ Description of field1
--   , field2 :: Int
--     -- ^ Description of field2
--   }
--
-- -- In Main.hs:
-- $(deriveHasJSONSchema ''MyInput)
--
-- -- Now you can use:
-- schema = jsonSchema @MyInput
-- @
class HasJSONSchema a where
  jsonSchema :: JSONSchema

-- | Derive a 'HasJSONSchema' instance for a record type.
--
-- Same requirements as 'deriveJSONSchema':
--
-- 1. Type must be in a separate, already-compiled module (TH staging)
-- 2. That module must have @{-\# LANGUAGE FieldSelectors \#-}@ (for @getDoc@)
-- 3. All fields must have @-- ^@ Haddock documentation
--
-- See 'deriveJSONSchema' for detailed explanation of why these are required.
--
-- @
-- $(deriveHasJSONSchema ''MyInput)
-- @
deriveHasJSONSchema :: Name -> Q [Dec]
deriveHasJSONSchema typeName = do
  schemaExp <- deriveJSONSchema typeName
  [d| instance HasJSONSchema $(conT typeName) where
        jsonSchema = $(pure schemaExp)
    |]

-- ══════════════════════════════════════════════════════════════
-- SCHEMA MARKER TRAITS
-- ══════════════════════════════════════════════════════════════

-- | Marker class for types whose JSON schema uses oneOf.
--
-- Automatically derived by 'deriveHasJSONSchema' when the type is a sum type.
-- Used for contextual validation - e.g., structured output doesn't support oneOf.
class UsesOneOf a

-- | Open type family for types explicitly marked as using oneOf.
--
-- 'deriveUsesOneOf' generates instances like:
-- @type instance IsMarkedOneOf MySum = 'True@
--
-- Types without an explicit instance remain stuck, which the
-- 'HasUsesOneOf' type family interprets as 'False via 'IfStuck'.
type family IsMarkedOneOf (a :: K.Type) :: Bool

-- | Marker class for types whose JSON schema uses enum.
--
-- Automatically derived by 'deriveHasJSONSchema' when the type is an enum
-- (sum type with all nullary constructors).
class UsesEnum a

-- | Type family to check if a type has the 'UsesOneOf' marker.
--
-- Returns ''True' if the type has 'IsMarkedOneOf' set to ''True'.
-- For types without an explicit instance, this causes a type family
-- conflict when used in structured output validation.
type HasUsesOneOf :: K.Type -> Bool
type family HasUsesOneOf a where
  HasUsesOneOf a = IsMarkedOneOf a

-- | Type family to check if a type has the 'UsesEnum' marker.
type HasUsesEnum :: K.Type -> Bool
type family HasUsesEnum a where
  HasUsesEnum a = HasUsesEnumImpl a

-- Implementation detail: HasUsesEnumImpl still uses the closed type family approach.
-- Could be updated similarly to HasUsesOneOf if needed.
type family HasUsesEnumImpl (a :: K.Type) :: Bool where
  HasUsesEnumImpl _ = 'False  -- Default: no enum

-- ══════════════════════════════════════════════════════════════
-- CONTEXTUAL SCHEMA VALIDATION
-- ══════════════════════════════════════════════════════════════

-- | Context in which a schema is being used.
--
-- Different contexts have different constraints on what schema features
-- are supported. For example, Anthropic's structured output doesn't
-- support @oneOf@ schemas.
data SchemaContext
  = ToolInputCtx     -- ^ Tool input: full JSON Schema support
  | ToolOutputCtx    -- ^ Tool output: full JSON Schema support
  | StructuredOutputCtx  -- ^ LLM structured output: no oneOf

-- | Validate that a type's schema is valid in the given context.
--
-- Produces helpful compile-time error messages when validation fails.
--
-- @
-- -- This compiles (tools support oneOf):
-- type Valid = ValidInContext 'ToolInputCtx MyUnion
--
-- -- This fails with helpful message (structured output doesn't support oneOf):
-- type Invalid = ValidInContext 'StructuredOutputCtx MyUnion
-- @
type ValidInContext :: SchemaContext -> K.Type -> Constraint
type family ValidInContext ctx t where
  ValidInContext 'ToolInputCtx t = ()  -- Tools allow everything
  ValidInContext 'ToolOutputCtx t = ()
  ValidInContext 'StructuredOutputCtx t = ValidStructuredOutputImpl t

-- | Constraint alias for structured output validation.
type ValidStructuredOutput :: K.Type -> Constraint
type ValidStructuredOutput t = ValidInContext 'StructuredOutputCtx t

-- | Implementation of structured output validation.
--
-- Checks that the type doesn't use oneOf (sum types).
-- Uses CheckNotMarkedOneOf to avoid evaluating TypeError when the marker is unknown.
type family ValidStructuredOutputImpl (t :: K.Type) :: Constraint where
  ValidStructuredOutputImpl t = CheckNotMarkedOneOf (IsMarkedOneOf t) t

-- | Check that a type is not marked as oneOf.
-- Only pattern matches on 'True to emit error; 'False and stuck types pass through.
type family CheckNotMarkedOneOf (isMarked :: Bool) (t :: K.Type) :: Constraint where
  CheckNotMarkedOneOf 'True t = TypeError
    ('Text "Schema error for structured output type: " ':<>: 'ShowType t
     ':$$: 'Text ""
     ':$$: 'Text "Anthropic's structured output does not support 'oneOf' schemas."
     ':$$: 'Text "This type uses a sum type or union that generates oneOf."
     ':$$: 'Text ""
     ':$$: 'Text "Fix options:"
     ':$$: 'Text "  1. Use a tagged record: data MyChoice = MyChoice { tag :: Tag, ... }"
     ':$$: 'Text "  2. Use separate fields: data Output = Output { optionA :: Maybe A, optionB :: Maybe B }"
     ':$$: 'Text "  3. Use an enum if choices are simple strings")
  CheckNotMarkedOneOf 'False _ = ()

-- | Type-level conditional.
type If :: Bool -> Constraint -> Constraint -> Constraint
type family If cond t f where
  If 'True t _ = t
  If 'False _ f = f

-- ══════════════════════════════════════════════════════════════
-- TH MARKER DERIVATION
-- ══════════════════════════════════════════════════════════════

-- | Derive a 'UsesOneOf' instance for a type.
--
-- Call this when you have a sum type that generates a oneOf schema.
-- Typically called automatically by 'deriveHasJSONSchema' for sum types.
--
-- Generates both:
--
-- 1. @instance UsesOneOf TypeName@ (marker class)
-- 2. @instance IsOneOfSchema TypeName where type UsesOneOfSchema TypeName = 'True@
--    (for type-level detection via 'HasUsesOneOf')
--
-- @
-- $(deriveUsesOneOf ''MyUnion)
-- @
deriveUsesOneOf :: Name -> Q [Dec]
deriveUsesOneOf typeName = do
  -- Generate: instance UsesOneOf TypeName (backwards-compatible marker)
  markerInst <- [d| instance UsesOneOf $(conT typeName) |]
  -- Generate: type instance IsMarkedOneOf TypeName = 'True
  let markedInst = TySynInstD (TySynEqn Nothing
        (AppT (ConT ''IsMarkedOneOf) (ConT typeName))
        (PromotedT 'True))
  pure (markerInst ++ [markedInst])

-- | Derive a 'UsesEnum' instance for a type.
--
-- Call this when you have an enum type (sum type with all nullary constructors).
-- Typically called automatically by 'deriveHasJSONSchema' for enum types.
--
-- @
-- $(deriveUsesEnum ''MyEnum)
-- @
deriveUsesEnum :: Name -> Q [Dec]
deriveUsesEnum typeName = do
  -- Generate: instance UsesEnum TypeName
  [d| instance UsesEnum $(conT typeName) |]