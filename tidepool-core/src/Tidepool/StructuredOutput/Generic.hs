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
module Tidepool.StructuredOutput.Generic
  ( -- * Re-export the class
    GStructuredOutput(..)

    -- * Product Field Handling
  , GStructuredProduct(..)

    -- * Sum Type Handling
  , GStructuredSum(..)
  ) where

import Data.Aeson (Value(..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics

import Tidepool.Schema (JSONSchema(..), SchemaType(..), emptySchema, objectSchema)
import Tidepool.StructuredOutput.Class (GStructuredOutput(..), StructuredOutput(..), StructuredOptions(..), SumEncoding(..))
import Tidepool.StructuredOutput.Error (ParseDiagnostic(..), expectedObject, missingField, typeMismatch)


-- ════════════════════════════════════════════════════════════════════════════
-- DATATYPE WRAPPER (M1 D)
-- ════════════════════════════════════════════════════════════════════════════

-- | Pass through datatype metadata.
instance GStructuredOutput f => GStructuredOutput (M1 D d f) where
  gStructuredSchema = gStructuredSchema @f
  gEncodeStructured opts (M1 x) = gEncodeStructured opts x
  gParseStructured opts path v = M1 <$> gParseStructured @f opts path v


-- ════════════════════════════════════════════════════════════════════════════
-- CONSTRUCTOR WRAPPER (M1 C) - Single Constructor (Product Type)
-- ════════════════════════════════════════════════════════════════════════════

-- | Handle single-constructor types (records).
instance GStructuredProduct f => GStructuredOutput (M1 C c f) where
  gStructuredSchema opts =
    let fields = gProductSchema @f opts
        required = gProductRequired @f opts
    in objectSchema (map (\(k, v) -> (T.pack k, v)) fields) (map T.pack required)

  gEncodeStructured opts (M1 x) =
    Object $ KeyMap.fromList $ map (first Key.fromString) $ gProductEncode opts x

  gParseStructured opts path (Object obj) =
    M1 <$> gProductParse opts path obj
  gParseStructured _ path v =
    Left $ expectedObject path v


-- ════════════════════════════════════════════════════════════════════════════
-- PRODUCT FIELD HANDLING
-- ════════════════════════════════════════════════════════════════════════════

-- | Class for handling product (record) fields.
class GStructuredProduct (f :: Type -> Type) where
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
instance {-# OVERLAPPING #-} (Selector s, StructuredOutput a)
    => GStructuredProduct (M1 S s (K1 i (Maybe a))) where
  gProductSchema opts =
    let rawName = selName (undefined :: M1 S s (K1 i (Maybe a)) p)
        jsonName = opts.soFieldLabelModifier rawName
    -- Schema is the inner type - Maybe just makes it optional
    in [(jsonName, structuredSchema @a)]

  -- Maybe fields are NEVER required
  gProductRequired _ = []

  gProductEncode opts (M1 (K1 mx)) =
    let rawName = selName (undefined :: M1 S s (K1 i (Maybe a)) p)
        jsonName = opts.soFieldLabelModifier rawName
    in case mx of
         Nothing | opts.soOmitNothingFields -> []  -- Omit field entirely
         Nothing -> [(jsonName, Null)]             -- Explicit null
         Just x  -> [(jsonName, encodeStructured x)]

  gProductParse opts path obj =
    let rawName = selName (undefined :: M1 S s (K1 i (Maybe a)) p)
        jsonName = opts.soFieldLabelModifier rawName
        jsonKey = Key.fromString jsonName
        fieldPath = path ++ [T.pack jsonName]
    in case KeyMap.lookup jsonKey obj of
         Nothing -> Right (M1 (K1 Nothing))    -- Missing = Nothing
         Just Null -> Right (M1 (K1 Nothing))  -- Explicit null = Nothing
         Just v -> M1 . K1 . Just <$> first (prependPath fieldPath) (parseStructured @a v)


-- | Handle a single record field with selector (non-Maybe fields).
instance (Selector s, StructuredOutput a) => GStructuredProduct (M1 S s (K1 i a)) where
  gProductSchema opts =
    let rawName = selName (undefined :: M1 S s (K1 i a) p)
        jsonName = opts.soFieldLabelModifier rawName
    in [(jsonName, structuredSchema @a)]

  gProductRequired opts
    -- Check if the field type is Maybe by looking at its schema
    -- A proper implementation would use a type family, but for now
    -- we check the schema type
    = let rawName = selName (undefined :: M1 S s (K1 i a) p)
          jsonName = opts.soFieldLabelModifier rawName
          _schema = structuredSchema @a
      -- All fields are required for now (Maybe handling comes in Instances)
      in [jsonName]

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
instance (Constructor c, GStructuredProduct f) => GStructuredSum (M1 C c f) where
  gSumVariants opts =
    let constructorName = opts.soConstructorTagModifier $ conName' (undefined :: M1 C c f p)
        fields = gProductSchema @f opts
        required = gProductRequired @f opts
        schema = objectSchema (map (\(k, v) -> (T.pack k, v)) fields) (map T.pack required)
    in [(constructorName, schema)]

  gSumEncode opts (M1 x) =
    let constructorName = opts.soConstructorTagModifier $ conName' (undefined :: M1 C c f p)
        fields = gProductEncode opts x
    in (constructorName, Object $ KeyMap.fromList $ map (first Key.fromString) fields)

  gSumParse opts path tag v
    | tag == opts.soConstructorTagModifier (conName' (undefined :: M1 C c f p)) =
        case v of
          Object obj -> Just (M1 <$> gProductParse @f opts path obj)
          _ -> Just (Left $ expectedObject path v)
    | otherwise = Nothing

-- Helper to extract constructor name
conName' :: forall c f p. Constructor c => M1 C c f p -> String
conName' _ = conName (undefined :: M1 C c f p)


-- | GStructuredOutput instance for sum types (multiple constructors).
--
-- This uses TaggedObject encoding: {"tag": "Constructor", "contents": {...}}
instance GStructuredSum (l :+: r) => GStructuredOutput (l :+: r) where
  gStructuredSchema opts =
    let variants = gSumVariants @(l :+: r) opts
    in case opts.soSumEncoding of
         TaggedObject tagField contentsField ->
           let variantSchemas = map (makeVariantSchema tagField contentsField) variants
           in (emptySchema TObject) { schemaOneOf = Just variantSchemas }
         ObjectWithSingleField ->
           error "StructuredOutput: ObjectWithSingleField sum encoding not yet supported"
         TwoElemArray ->
           error "StructuredOutput: TwoElemArray sum encoding not yet supported"
    where
      makeVariantSchema tagField contentsField (constructorName, contentsSchema) =
        -- Tag field uses enum to constrain to this constructor
        let tagSchema = (emptySchema TString) { schemaEnum = Just [T.pack constructorName] }
        in objectSchema
             [ (T.pack tagField, tagSchema)
             , (T.pack contentsField, contentsSchema)
             ]
             [T.pack tagField, T.pack contentsField]

  gEncodeStructured opts x =
    let (constructorName, contents) = gSumEncode opts x
    in case opts.soSumEncoding of
         TaggedObject tagField contentsField ->
           Object $ KeyMap.fromList
             [ (Key.fromString tagField, String $ T.pack constructorName)
             , (Key.fromString contentsField, contents)
             ]
         ObjectWithSingleField ->
           error "StructuredOutput: ObjectWithSingleField sum encoding not yet supported"
         TwoElemArray ->
           error "StructuredOutput: TwoElemArray sum encoding not yet supported"

  gParseStructured opts path (Object obj) =
    case opts.soSumEncoding of
      TaggedObject tagField contentsField ->
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
  gParseStructured _ path v = Left $ expectedObject path v
