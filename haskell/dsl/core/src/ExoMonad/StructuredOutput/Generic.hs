{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Generic derivation machinery for 'StructuredOutput'.
--
-- This module provides 'GStructuredOutput' instances that walk the
-- GHC.Generics representation to derive schema, encoding, and parsing.
--
-- Users don't typically need to import this module directly - the
-- 'StructuredOutput' class uses these instances via DefaultSignatures.
module ExoMonad.StructuredOutput.Generic
  ( -- * Re-export the class
    GStructuredOutput (..),

    -- * Product Field Handling
    GStructuredProduct (..),

    -- * Sum Type Handling
    GStructuredSum (..),
  )
where

import Lens.Micro ((%~), _1)
import Lens.Micro.GHC (each)
import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Bifunctor (first)
import Data.Text qualified as T
import ExoMonad.Schema (JSONSchema (..), SchemaType (..), emptySchema, objectSchema)
import ExoMonad.StructuredOutput.Class (GStructuredOutput (..), StructuredOptions (..), StructuredOutput (..), SumEncoding (..))
import ExoMonad.StructuredOutput.Error (ParseDiagnostic (..), expectedObject, missingField, typeMismatch)
import ExoMonad.StructuredOutput.Prefix (detectPrefix, makeStripPrefix)
import GHC.Generics

-- ════════════════════════════════════════════════════════════════════════════
-- DATATYPE WRAPPER (M1 D)
-- ════════════════════════════════════════════════════════════════════════════

-- | Pass through datatype metadata.
instance (GStructuredOutput f) => GStructuredOutput (M1 D d f) where
  gStructuredSchema = gStructuredSchema @f
  gEncodeStructured opts (M1 x) = gEncodeStructured opts x
  gParseStructured opts path v = M1 <$> gParseStructured @f opts path v

-- ════════════════════════════════════════════════════════════════════════════
-- CONSTRUCTOR WRAPPER (M1 C) - Single Constructor (Product Type)
-- ════════════════════════════════════════════════════════════════════════════

-- | Handle single-constructor types (records).
-- Detects common prefix across all fields and strips it.
instance (GStructuredProduct f) => GStructuredOutput (M1 C c f) where
  gStructuredSchema opts =
    let -- Collect all raw field names and detect common prefix
        rawNames = gProductRawFieldNames @f
        commonPfx = detectPrefix rawNames
        -- Create modifier that strips this specific prefix
        prefixModifier = makeStripPrefix commonPfx
        opts' = opts {soFieldLabelModifier = prefixModifier}
        fields = gProductSchema @f opts'
        required = gProductRequired @f opts'
     in objectSchema (fields & each . _1 %~ T.pack) (required & each %~ T.pack)

  gEncodeStructured opts (M1 x) =
    let rawNames = gProductRawFieldNames @f
        commonPfx = detectPrefix rawNames
        prefixModifier = makeStripPrefix commonPfx
        opts' = opts {soFieldLabelModifier = prefixModifier}
     in Object $ KeyMap.fromList $ map (first Key.fromString) $ gProductEncode opts' x

  gParseStructured opts path (Object obj) =
    let rawNames = gProductRawFieldNames @f
        commonPfx = detectPrefix rawNames
        prefixModifier = makeStripPrefix commonPfx
        opts' = opts {soFieldLabelModifier = prefixModifier}
     in M1 <$> gProductParse opts' path obj
  gParseStructured _ path v =
    Left $ expectedObject path v

-- ════════════════════════════════════════════════════════════════════════════
-- PRODUCT FIELD HANDLING
-- ════════════════════════════════════════════════════════════════════════════

-- | Class for handling product (record) fields.
class GStructuredProduct (f :: Type -> Type) where
  -- | Get all raw field names (before any modification).
  -- Used to detect common prefixes across the record.
  gProductRawFieldNames :: [String]

  -- | Generate schema entries for all fields.
  gProductSchema :: StructuredOptions -> [(String, JSONSchema)]

  -- | List of required field names (non-Maybe fields).
  gProductRequired :: StructuredOptions -> [String]

  -- | Encode all fields to key-value pairs.
  gProductEncode :: StructuredOptions -> f p -> [(String, Value)]

  -- | Parse all fields from an object.
  gProductParse :: StructuredOptions -> [Text] -> KeyMap.KeyMap Value -> Either ParseDiagnostic (f p)

-- | Handle Maybe fields - NOT required, handles missing/null specially.
--
-- This OVERLAPPING instance pattern-matches on Maybe to detect optional fields
-- at the type level. Maybe fields are:
-- - Never in the required list
-- - Encoded as null (or omitted if soOmitNothingFields)
-- - Parsed as Nothing when missing or null
instance
  {-# OVERLAPPING #-}
  (Selector s, StructuredOutput a) =>
  GStructuredProduct (M1 S s (K1 i (Maybe a)))
  where
  gProductRawFieldNames =
    [selName (undefined :: M1 S s (K1 i (Maybe a)) p)]

  gProductSchema opts =
    let rawName = selName (undefined :: M1 S s (K1 i (Maybe a)) p)
        jsonName = opts.soFieldLabelModifier rawName
     in -- Schema is the inner type - Maybe just makes it optional
        [(jsonName, structuredSchema @a)]

  -- Maybe fields are NEVER required
  gProductRequired _ = []

  gProductEncode opts (M1 (K1 mx)) =
    let rawName = selName (undefined :: M1 S s (K1 i (Maybe a)) p)
        jsonName = opts.soFieldLabelModifier rawName
     in case mx of
          Nothing | opts.soOmitNothingFields -> [] -- Omit field entirely
          Nothing -> [(jsonName, Null)] -- Explicit null
          Just x -> [(jsonName, encodeStructured x)]

  gProductParse opts path obj =
    let rawName = selName (undefined :: M1 S s (K1 i (Maybe a)) p)
        jsonName = opts.soFieldLabelModifier rawName
        jsonKey = Key.fromString jsonName
        fieldPath = path ++ [T.pack jsonName]
     in case KeyMap.lookup jsonKey obj of
          Nothing -> Right (M1 (K1 Nothing)) -- Missing = Nothing
          Just Null -> Right (M1 (K1 Nothing)) -- Explicit null = Nothing
          Just v -> M1 . K1 . Just <$> first (prependPath fieldPath) (parseStructured @a v)

-- | Handle a single record field with selector (non-Maybe fields).
instance (Selector s, StructuredOutput a) => GStructuredProduct (M1 S s (K1 i a)) where
  gProductRawFieldNames =
    [selName (undefined :: M1 S s (K1 i a) p)]

  gProductSchema opts =
    let rawName = selName (undefined :: M1 S s (K1 i a) p)
        jsonName = opts.soFieldLabelModifier rawName
     in [(jsonName, structuredSchema @a)]

  gProductRequired opts =
    -- Check if the field type is Maybe by looking at its schema
    -- A proper implementation would use a type family, but for now
    -- we check the schema type
    let rawName = selName (undefined :: M1 S s (K1 i a) p)
        jsonName = opts.soFieldLabelModifier rawName
        _schema = structuredSchema @a
     in -- All fields are required for now (Maybe handling comes in Instances)
        [jsonName]

  gProductEncode opts (M1 (K1 x)) =
    let rawName = selName (undefined :: M1 S s (K1 i a) p)
        jsonName = opts.soFieldLabelModifier rawName
     in [(jsonName, encodeStructured x)]

  gProductParse opts path obj =
    let rawName = selName (undefined :: M1 S s (K1 i a) p)
        jsonName = opts.soFieldLabelModifier rawName
        jsonKey = Key.fromString jsonName
        fieldPath = path ++ [T.pack jsonName]
     in case KeyMap.lookup jsonKey obj of
          Just v -> M1 . K1 <$> first (prependPath fieldPath) (parseStructured @a v)
          Nothing -> Left $ missingField fieldPath

-- | Handle product of two field groups.
instance (GStructuredProduct l, GStructuredProduct r) => GStructuredProduct (l :*: r) where
  gProductRawFieldNames =
    gProductRawFieldNames @l ++ gProductRawFieldNames @r

  gProductSchema opts =
    gProductSchema @l opts ++ gProductSchema @r opts

  gProductRequired opts =
    gProductRequired @l opts ++ gProductRequired @r opts

  gProductEncode opts (l :*: r) =
    gProductEncode opts l ++ gProductEncode opts r

  gProductParse opts path obj = do
    l <- gProductParse @l opts path obj
    r <- gProductParse @r opts path obj
    pure (l :*: r)

-- | Handle empty product (no fields).
instance GStructuredProduct U1 where
  gProductRawFieldNames = []
  gProductSchema _ = []
  gProductRequired _ = []
  gProductEncode _ U1 = []
  gProductParse _ _ _ = Right U1

-- ════════════════════════════════════════════════════════════════════════════
-- HELPER: Update path in diagnostic
-- ════════════════════════════════════════════════════════════════════════════

-- | Prepend path to a diagnostic's existing path.
prependPath :: [Text] -> ParseDiagnostic -> ParseDiagnostic
prependPath prefix (ParseDiagnostic p e a m) = ParseDiagnostic (prefix ++ p) e a m

-- ════════════════════════════════════════════════════════════════════════════
-- SUM TYPES (Multiple Constructors)
-- ════════════════════════════════════════════════════════════════════════════

-- | Class for handling sum type constructors.
class GStructuredSum (f :: Type -> Type) where
  -- | Get all constructor variants for schema generation.
  gSumVariants :: StructuredOptions -> [(String, JSONSchema)]

  -- | Encode a value, returning (constructor name, encoded payload).
  gSumEncode :: StructuredOptions -> f p -> (String, Value)

  -- | Try to parse a value given a constructor name.
  gSumParse :: StructuredOptions -> [Text] -> String -> Value -> Maybe (Either ParseDiagnostic (f p))

-- | Sum type choice: try left branch, then right branch.
instance (GStructuredSum l, GStructuredSum r) => GStructuredSum (l :+: r) where
  gSumVariants opts = gSumVariants @l opts ++ gSumVariants @r opts

  gSumEncode opts (L1 x) = gSumEncode opts x
  gSumEncode opts (R1 x) = gSumEncode opts x

  gSumParse opts path tag v =
    case gSumParse @l opts path tag v of
      Just result -> Just (L1 <$> result)
      Nothing -> case gSumParse @r opts path tag v of
        Just result -> Just (R1 <$> result)
        Nothing -> Nothing

-- | Single constructor in a sum type.
-- Also detects common prefix within each constructor's fields.
instance (Constructor c, GStructuredProduct f) => GStructuredSum (M1 C c f) where
  gSumVariants opts =
    let constructorName = opts.soConstructorTagModifier $ conName' (undefined :: M1 C c f p)
        -- Detect common prefix for this constructor's fields
        rawNames = gProductRawFieldNames @f
        commonPfx = detectPrefix rawNames
        prefixModifier = makeStripPrefix commonPfx
        opts' = opts {soFieldLabelModifier = prefixModifier}
        fields = gProductSchema @f opts'
        required = gProductRequired @f opts'
        schema = objectSchema (fields & each . _1 %~ T.pack) (required & each %~ T.pack)
     in [(constructorName, schema)]

  gSumEncode opts (M1 x) =
    let constructorName = opts.soConstructorTagModifier $ conName' (undefined :: M1 C c f p)
        rawNames = gProductRawFieldNames @f
        commonPfx = detectPrefix rawNames
        prefixModifier = makeStripPrefix commonPfx
        opts' = opts {soFieldLabelModifier = prefixModifier}
        fields = gProductEncode opts' x
     in (constructorName, Object $ KeyMap.fromList $ map (first Key.fromString) fields)

  gSumParse opts path tag v
    | tag == opts.soConstructorTagModifier (conName' (undefined :: M1 C c f p)) =
        let rawNames = gProductRawFieldNames @f
            commonPfx = detectPrefix rawNames
            prefixModifier = makeStripPrefix commonPfx
            opts' = opts {soFieldLabelModifier = prefixModifier}
         in case v of
              Object obj -> Just (M1 <$> gProductParse @f opts' path obj)
              _ -> Just (Left $ expectedObject path v)
    | otherwise = Nothing

-- Helper to extract constructor name
conName' :: forall c f p. (Constructor c) => M1 C c f p -> String
conName' _ = conName (undefined :: M1 C c f p)

-- ════════════════════════════════════════════════════════════════════════════
-- NULLARY SUM TYPE DETECTION AND ENUM GENERATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Check if all variants have empty contents (nullary constructors).
--
-- For example, @data Priority = Low | Medium | High@ has all nullary constructors.
allNullary :: [(String, JSONSchema)] -> Bool
allNullary = all (\(_, schema) -> isEmptyObject schema)

-- | Check if a schema represents an empty object {}.
--
-- An empty object has type TObject with no properties and no required fields.
isEmptyObject :: JSONSchema -> Bool
isEmptyObject (JSONSchema {schemaType = typ, schemaProperties = props, schemaRequired = req}) =
  typ == TObject
    && null props
    && null req

-- | Generate a string enum schema.
--
-- For nullary sum types like @data Priority = Low | Medium | High@,
-- generates @{"type": "string", "enum": ["Low", "Medium", "High"]}@
-- instead of wasteful oneOf with tag+contents encoding.
stringEnumSchema :: [String] -> JSONSchema
stringEnumSchema constructors =
  (emptySchema TString) {schemaEnum = Just (map T.pack constructors)}

-- | GStructuredOutput instance for sum types (multiple constructors).
--
-- This uses TaggedObject encoding: {"tag": "Constructor", "contents": {...}}
--
-- Special case: If ALL constructors are nullary (no data), generates a
-- string enum instead of oneOf for efficiency and Anthropic compatibility.
instance (GStructuredSum (l :+: r)) => GStructuredOutput (l :+: r) where
  gStructuredSchema opts =
    let variants = gSumVariants @(l :+: r) opts
     in case opts.soSumEncoding of
          TaggedObject tagField contentsField ->
            -- Check if ALL variants are nullary (empty contents)
            if allNullary variants
              then stringEnumSchema (map fst variants)
              else
                let variantSchemas = map (makeVariantSchema tagField contentsField) variants
                 in (emptySchema TObject) {schemaOneOf = Just variantSchemas}
          ObjectWithSingleField ->
            error "StructuredOutput: ObjectWithSingleField sum encoding not yet supported"
          TwoElemArray ->
            error "StructuredOutput: TwoElemArray sum encoding not yet supported"
    where
      makeVariantSchema tagField contentsField (constructorName, contentsSchema) =
        -- Tag field uses enum to constrain to this constructor
        let tagSchema = (emptySchema TString) {schemaEnum = Just [T.pack constructorName]}
         in objectSchema
              [ (T.pack tagField, tagSchema),
                (T.pack contentsField, contentsSchema)
              ]
              [T.pack tagField, T.pack contentsField]

  gEncodeStructured opts x =
    let (constructorName, contents) = gSumEncode opts x
        variants = gSumVariants @(l :+: r) opts
     in case opts.soSumEncoding of
          TaggedObject tagField contentsField ->
            -- If ALL variants are nullary, encode as plain string
            if allNullary variants
              then String $ T.pack constructorName
              else
                Object $
                  KeyMap.fromList
                    [ (Key.fromString tagField, String $ T.pack constructorName),
                      (Key.fromString contentsField, contents)
                    ]
          ObjectWithSingleField ->
            error "StructuredOutput: ObjectWithSingleField sum encoding not yet supported"
          TwoElemArray ->
            error "StructuredOutput: TwoElemArray sum encoding not yet supported"

  gParseStructured opts path v@(Object obj) =
    let variants = gSumVariants @(l :+: r) opts
     in case opts.soSumEncoding of
          TaggedObject tagField contentsField ->
            -- If ALL variants are nullary, we expect string, not object
            if allNullary variants
              then Left $ typeMismatch path "string (for nullary enum)" v
              else
                let tagKey = Key.fromString tagField
                    contentsKey = Key.fromString contentsField
                 in case (KeyMap.lookup tagKey obj, KeyMap.lookup contentsKey obj) of
                      (Just (String tag), Just contents) ->
                        case gSumParse @(l :+: r) opts path (T.unpack tag) contents of
                          Just result -> result
                          Nothing -> Left $ typeMismatch path "one of known constructors" (String tag)
                      (Just _, _) -> Left $ typeMismatch path "string tag" (Object obj)
                      (Nothing, _) -> Left $ missingField (path ++ [T.pack tagField])
          ObjectWithSingleField ->
            error "StructuredOutput: ObjectWithSingleField sum encoding not yet supported"
          TwoElemArray ->
            error "StructuredOutput: TwoElemArray sum encoding not yet supported"
  gParseStructured opts path v@(String str) =
    let variants = gSumVariants @(l :+: r) opts
     in case opts.soSumEncoding of
          TaggedObject _ _ ->
            -- If ALL variants are nullary, parse as string enum
            if allNullary variants
              then case gSumParse @(l :+: r) opts path (T.unpack str) (Object KeyMap.empty) of
                Just result -> result
                Nothing -> Left $ typeMismatch path "one of known enum values" v
              else Left $ expectedObject path v
          _ -> Left $ expectedObject path v
  gParseStructured _ path v = Left $ expectedObject path v
