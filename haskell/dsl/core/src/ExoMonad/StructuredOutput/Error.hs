{-# LANGUAGE OverloadedStrings #-}

-- | Parse diagnostics for structured output.
--
-- Provides detailed error information when JSON parsing fails,
-- including the exact field path where the error occurred.
module ExoMonad.StructuredOutput.Error
  ( -- * Parse Diagnostic
    ParseDiagnostic (..),
    formatDiagnostic,

    -- * Smart Constructors
    expectedObject,
    expectedArray,
    expectedString,
    expectedNumber,
    expectedBool,
    missingField,
    typeMismatch,
    customError,

    -- * Value Description
    describeValue,
  )
where

import Data.Aeson (Value (..))
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import Data.Text qualified as T
-- Core types imported from Class.hs to avoid circularity
import ExoMonad.StructuredOutput.Class (ParseDiagnostic (..), formatDiagnostic)

-- ════════════════════════════════════════════════════════════════════════════
-- SMART CONSTRUCTORS
-- ════════════════════════════════════════════════════════════════════════════

-- | Error for when an object was expected.
expectedObject :: [Text] -> Value -> ParseDiagnostic
expectedObject path v =
  ParseDiagnostic
    { pdPath = path,
      pdExpected = "object",
      pdActual = describeValue v,
      pdMessage = "Expected JSON object but got " <> describeValue v
    }

-- | Error for when an array was expected.
expectedArray :: [Text] -> Value -> ParseDiagnostic
expectedArray path v =
  ParseDiagnostic
    { pdPath = path,
      pdExpected = "array",
      pdActual = describeValue v,
      pdMessage = "Expected JSON array but got " <> describeValue v
    }

-- | Error for when a string was expected.
expectedString :: [Text] -> Value -> ParseDiagnostic
expectedString path v =
  ParseDiagnostic
    { pdPath = path,
      pdExpected = "string",
      pdActual = describeValue v,
      pdMessage = "Expected string but got " <> describeValue v
    }

-- | Error for when a number was expected.
expectedNumber :: [Text] -> Value -> ParseDiagnostic
expectedNumber path v =
  ParseDiagnostic
    { pdPath = path,
      pdExpected = "number",
      pdActual = describeValue v,
      pdMessage = "Expected number but got " <> describeValue v
    }

-- | Error for when a boolean was expected.
expectedBool :: [Text] -> Value -> ParseDiagnostic
expectedBool path v =
  ParseDiagnostic
    { pdPath = path,
      pdExpected = "boolean",
      pdActual = describeValue v,
      pdMessage = "Expected boolean but got " <> describeValue v
    }

-- | Error for a missing required field.
missingField :: [Text] -> ParseDiagnostic
missingField path =
  ParseDiagnostic
    { pdPath = path,
      pdExpected = "required field",
      pdActual = "missing",
      pdMessage = "Required field '" <> fieldName <> "' is missing"
    }
  where
    fieldName = case path of
      [] -> "(unknown)"
      ps -> last ps

-- | Generic type mismatch error.
typeMismatch :: [Text] -> Text -> Value -> ParseDiagnostic
typeMismatch path expected v =
  ParseDiagnostic
    { pdPath = path,
      pdExpected = expected,
      pdActual = describeValue v,
      pdMessage = "Type mismatch: expected " <> expected <> " but got " <> describeValue v
    }

-- | Custom error with user-provided message.
customError :: [Text] -> Text -> ParseDiagnostic
customError path msg =
  ParseDiagnostic
    { pdPath = path,
      pdExpected = "",
      pdActual = "",
      pdMessage = msg
    }

-- ════════════════════════════════════════════════════════════════════════════
-- VALUE DESCRIPTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Describe a JSON value for error messages.
--
-- Truncates long strings and summarizes structures.
describeValue :: Value -> Text
describeValue = \case
  Object o -> "object with " <> T.pack (show (length o)) <> " fields"
  Array a -> "array with " <> T.pack (show (length a)) <> " items"
  String s
    | T.length s <= 30 -> "string: \"" <> s <> "\""
    | otherwise -> "string: \"" <> T.take 30 s <> "...\""
  Number n -> case floatingOrInteger n of
    Left (d :: Double) -> "number: " <> T.pack (show d)
    Right (i :: Integer) -> "integer: " <> T.pack (show i)
  Bool b -> "boolean: " <> if b then "true" else "false"
  Null -> "null"
