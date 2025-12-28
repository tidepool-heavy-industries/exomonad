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
  , describeField
  , emptySchema

    -- * TH Derivation
  , deriveJSONSchema

    -- * Conversion
  , schemaToValue
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Aeson (Value(..), object, (.=))
import Language.Haskell.TH

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
-- - @getDoc@ needs compiled interface files to read docs
-- - @NoFieldSelectors@ (project default) suppresses field selectors
-- - Without selectors, @getDoc (DeclDoc fieldName)@ fails
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
  [ "Field '" ++ nameBase fieldName ++ "' in '" ++ nameBase typeName ++ "' needs docs for schema."
  , ""
  , "Add:  " ++ nameBase fieldName ++ " :: " ++ prettyType fieldType
  , "      -- ^ " ++ typeHint fieldType
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
  -- Text, String -> TString
  ConT name | nameBase name == "Text"   -> [| emptySchema TString |]
            | nameBase name == "String" -> [| emptySchema TString |]
  -- Int, Integer -> TInteger
  ConT name | nameBase name == "Int"     -> [| emptySchema TInteger |]
            | nameBase name == "Integer" -> [| emptySchema TInteger |]
  -- Double, Float -> TNumber
  ConT name | nameBase name == "Double" -> [| emptySchema TNumber |]
            | nameBase name == "Float"  -> [| emptySchema TNumber |]
  -- Bool -> TBoolean
  ConT name | nameBase name == "Bool" -> [| emptySchema TBoolean |]
  -- [a] -> array of a
  AppT ListT elemType -> do
    elemSchema <- typeToSchemaExp elemType
    [| arraySchema $(pure elemSchema) |]
  -- Maybe a -> same as a (optional handled by not being in required)
  AppT (ConT name) innerType | nameBase name == "Maybe" ->
    typeToSchemaExp innerType
  -- Fallback: treat as string
  _ -> [| emptySchema TString |]
