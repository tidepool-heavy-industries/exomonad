{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Base type instances for 'StructuredOutput'.
--
-- Provides instances for primitive types like 'Text', 'Int', 'Bool',
-- as well as container types like 'Maybe' and lists.
--
-- Also provides 'StringEnum' for LLM-compatible enum schemas.
module ExoMonad.StructuredOutput.Instances
  ( StringEnum (..),
    ExoMonadDefault (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..))
import Data.Text qualified as T
import Data.Vector qualified as V
import ExoMonad.StructuredOutput.Class (ExoMonadDefault (..), GStructuredOutput (..), HasJSONSchema (..), JSONSchema (..), ParseDiagnostic (..), StructuredOutput (..), formatDiagnostic)
import ExoMonad.StructuredOutput.Error (expectedArray, expectedNumber, expectedString, typeMismatch)
import GHC.Generics (Rep, from, to)

-- ════════════════════════════════════════════════════════════════════════════
-- PRIMITIVE TYPES
-- ════════════════════════════════════════════════════════════════════════════

instance StructuredOutput Text where
  structuredSchema = emptySchema TString
  encodeStructured = String
  parseStructured (String s) = Right s
  parseStructured v = Left $ expectedString [] v

instance {-# OVERLAPPING #-} StructuredOutput String where
  structuredSchema = emptySchema TString
  encodeStructured = String . T.pack
  parseStructured (String s) = Right (T.unpack s)
  parseStructured v = Left $ expectedString [] v

instance StructuredOutput Int where
  structuredSchema = emptySchema TInteger
  encodeStructured = Number . fromIntegral
  parseStructured (Number n) =
    case toBoundedInteger n of
      Just i -> Right i
      Nothing -> Left $ typeMismatch [] "integer (in bounds)" (Number n)
  parseStructured v = Left $ expectedNumber [] v

instance StructuredOutput Integer where
  structuredSchema = emptySchema TInteger
  encodeStructured = Number . fromIntegral
  parseStructured (Number n) =
    case toBoundedInteger n of
      Just (i :: Int) -> Right (fromIntegral i)
      Nothing -> Left $ typeMismatch [] "integer" (Number n)
  parseStructured v = Left $ expectedNumber [] v

instance StructuredOutput Double where
  structuredSchema = emptySchema TNumber
  encodeStructured = Number . fromFloatDigits
  parseStructured (Number n) = Right $ realToFrac n
  parseStructured v = Left $ expectedNumber [] v

instance StructuredOutput Bool where
  structuredSchema = emptySchema TBoolean
  encodeStructured = Bool
  parseStructured (Bool b) = Right b
  parseStructured v = Left $ expectedBool [] v

-- ════════════════════════════════════════════════════════════════════════════
-- CONTAINER TYPES
-- ════════════════════════════════════════════════════════════════════════════

instance (StructuredOutput a) => StructuredOutput (Maybe a) where
  -- Schema is same as inner type - optionality handled by 'required' list
  structuredSchema = structuredSchema @a

  encodeStructured Nothing = Null
  encodeStructured (Just x) = encodeStructured x

  parseStructured Null = Right Nothing
  parseStructured v = Just <$> parseStructured v

instance {-# OVERLAPPABLE #-} (StructuredOutput a) => StructuredOutput [a] where
  structuredSchema = arraySchema (structuredSchema @a)

  encodeStructured xs = Array $ V.fromList $ map encodeStructured xs

  parseStructured (Array arr) = do
    let indexed = zip [0 ..] (V.toList arr)
    traverse parseWithIndex indexed
    where
      parseWithIndex :: (Int, Value) -> Either ParseDiagnostic a
      parseWithIndex (i, v) =
        first (addIndex i) (parseStructured @a v)

      addIndex :: Int -> ParseDiagnostic -> ParseDiagnostic
      addIndex i (ParseDiagnostic p e a m) =
        ParseDiagnostic (T.pack ("[" ++ show i ++ "]") : p) e a m
  parseStructured v = Left $ expectedArray [] v

-- | Set as JSON array (elements deduplicated, order not guaranteed).
instance (StructuredOutput a, Ord a) => StructuredOutput (Set a) where
  structuredSchema = arraySchema (structuredSchema @a)
  encodeStructured = Array . V.fromList . map encodeStructured . Set.toList
  parseStructured (Array arr) =
    Set.fromList <$> traverse parseStructured (V.toList arr)
  parseStructured v = Left $ expectedArray [] v

-- | NonEmpty list as JSON array (must have at least one element).
instance (StructuredOutput a) => StructuredOutput (NonEmpty a) where
  structuredSchema = (arraySchema (structuredSchema @a)) {schemaMinItems = Just 1}
  encodeStructured = encodeStructured . NonEmpty.toList
  parseStructured v = do
    list <- parseStructured @[a] v
    case NonEmpty.nonEmpty list of
      Nothing -> Left $ typeMismatch [] "non-empty array" v
      Just ne -> Right ne

-- ════════════════════════════════════════════════════════════════════════════
-- TUPLE TYPES (common arities)
-- ════════════════════════════════════════════════════════════════════════════

instance (StructuredOutput a, StructuredOutput b) => StructuredOutput (a, b) where
  structuredSchema = arraySchema (emptySchema TObject) -- Simplified
  encodeStructured (x, y) = Array $ V.fromList [encodeStructured x, encodeStructured y]
  parseStructured (Array arr)
    | V.length arr == 2 = do
        x <- first (addIndex (0 :: Int)) $ parseStructured @a (arr V.! 0)
        y <- first (addIndex (1 :: Int)) $ parseStructured @b (arr V.! 1)
        pure (x, y)
    | otherwise = Left $ typeMismatch [] "array of 2 elements" (Array arr)
    where
      addIndex :: Int -> ParseDiagnostic -> ParseDiagnostic
      addIndex i (ParseDiagnostic path expected actual msg) =
        ParseDiagnostic (T.pack ("[" ++ show i ++ "]") : path) expected actual msg
  parseStructured v = Left $ expectedArray [] v

-- ════════════════════════════════════════════════════════════════════════════
-- UNIT TYPE
-- ════════════════════════════════════════════════════════════════════════════

-- | Unit type accepts any JSON value (matching Aeson's FromJSON behavior).
-- This is useful for transitions that don't carry payload.
instance StructuredOutput () where
  structuredSchema = emptySchema TNull
  encodeStructured () = Null
  parseStructured _ = Right () -- Accept any JSON, like Aeson

-- | Pass-through instance for raw JSON Value.
-- This enables backward compatibility with code using dynamic schemas.
-- The schema is an empty object (no constraints), and values pass through unchanged.
instance StructuredOutput Value where
  structuredSchema = emptySchema TObject
  encodeStructured = id
  parseStructured = Right

-- ════════════════════════════════════════════════════════════════════════════
-- EXOMONAD DEFAULT WRAPPER
-- ════════════════════════════════════════════════════════════════════════════

instance (Generic a, GStructuredOutput (Rep a)) => StructuredOutput (ExoMonadDefault a) where
  -- Use default generic implementation
  structuredSchema = gStructuredSchema @(Rep a) defaultOptions
  encodeStructured (ExoMonadDefault x) = gEncodeStructured defaultOptions (from x)
  parseStructured v = ExoMonadDefault . to <$> gParseStructured defaultOptions [] v

instance (Generic a, GStructuredOutput (Rep a)) => ToJSON (ExoMonadDefault a) where
  toJSON = encodeStructured

instance (Generic a, GStructuredOutput (Rep a)) => FromJSON (ExoMonadDefault a) where
  parseJSON v = case parseStructured v of
    Right x -> pure x
    Left diag -> fail (T.unpack (formatDiagnostic diag))

instance (Generic a, GStructuredOutput (Rep a)) => HasJSONSchema (ExoMonadDefault a) where
  jsonSchema = structuredSchema @(ExoMonadDefault a)
