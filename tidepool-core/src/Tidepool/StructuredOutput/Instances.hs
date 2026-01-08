{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Base type instances for 'StructuredOutput'.
--
-- Provides instances for primitive types like 'Text', 'Int', 'Bool',
-- as well as container types like 'Maybe' and lists.
module Tidepool.StructuredOutput.Instances () where

import Data.Aeson (Value(..))
import Data.Bifunctor (first)
import Data.Scientific (toBoundedInteger, fromFloatDigits)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import Tidepool.Schema (SchemaType(..), emptySchema, arraySchema)
import Tidepool.StructuredOutput.Class (StructuredOutput(..))
import Tidepool.StructuredOutput.Error
  ( ParseDiagnostic(..)
  , expectedString
  , expectedNumber
  , expectedBool
  , expectedArray
  , typeMismatch
  )


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

instance StructuredOutput a => StructuredOutput (Maybe a) where
  -- Schema is same as inner type - optionality handled by 'required' list
  structuredSchema = structuredSchema @a

  encodeStructured Nothing = Null
  encodeStructured (Just x) = encodeStructured x

  parseStructured Null = Right Nothing
  parseStructured v = Just <$> parseStructured v

instance {-# OVERLAPPABLE #-} StructuredOutput a => StructuredOutput [a] where
  structuredSchema = arraySchema (structuredSchema @a)

  encodeStructured xs = Array $ V.fromList $ map encodeStructured xs

  parseStructured (Array arr) = do
    let indexed = zip [0..] (V.toList arr)
    traverse parseWithIndex indexed
    where
      parseWithIndex :: (Int, Value) -> Either ParseDiagnostic a
      parseWithIndex (i, v) =
        first (addIndex i) (parseStructured @a v)

      addIndex :: Int -> ParseDiagnostic -> ParseDiagnostic
      addIndex i (ParseDiagnostic p e a m) =
        ParseDiagnostic (T.pack ("[" ++ show i ++ "]") : p) e a m

  parseStructured v = Left $ expectedArray [] v


-- ════════════════════════════════════════════════════════════════════════════
-- TUPLE TYPES (common arities)
-- ════════════════════════════════════════════════════════════════════════════

instance (StructuredOutput a, StructuredOutput b) => StructuredOutput (a, b) where
  structuredSchema = arraySchema (emptySchema TObject)  -- Simplified
  encodeStructured (a, b) = Array $ V.fromList [encodeStructured a, encodeStructured b]
  parseStructured (Array arr)
    | V.length arr == 2 = do
        a <- first (addIndex 0) $ parseStructured @a (arr V.! 0)
        b <- first (addIndex 1) $ parseStructured @b (arr V.! 1)
        pure (a, b)
    | otherwise = Left $ typeMismatch [] "array of 2 elements" (Array arr)
    where
      addIndex i (ParseDiagnostic p e a m) =
        ParseDiagnostic (T.pack ("[" ++ show i ++ "]") : p) e a m
  parseStructured v = Left $ expectedArray [] v


-- ════════════════════════════════════════════════════════════════════════════
-- UNIT TYPE
-- ════════════════════════════════════════════════════════════════════════════

-- | Unit type accepts any JSON value (matching Aeson's FromJSON behavior).
-- This is useful for transitions that don't carry payload.
instance StructuredOutput () where
  structuredSchema = emptySchema TNull
  encodeStructured () = Null
  parseStructured _ = Right ()  -- Accept any JSON, like Aeson


-- | Pass-through instance for raw JSON Value.
-- This enables backward compatibility with code using dynamic schemas.
-- The schema is an empty object (no constraints), and values pass through unchanged.
instance StructuredOutput Value where
  structuredSchema = emptySchema TObject
  encodeStructured = id
  parseStructured = Right
