{-# LANGUAGE OverloadedStrings #-}

-- | Parse diagnostics for structured output.
--
-- Provides detailed error information when JSON parsing fails,
-- including the exact field path where the error occurred.
module Tidepool.StructuredOutput.Error
  ( -- * Parse Diagnostic
    ParseDiagnostic(..)
  , formatDiagnostic

    -- * Smart Constructors
  , expectedObject
  , expectedArray
  , expectedString
  , expectedNumber
  , expectedBool
  , missingField
  , typeMismatch
  , customError

    -- * Value Description
  , describeValue
  ) where

import Data.Aeson (Value(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Scientific (floatingOrInteger)


-- | Detailed parse diagnostic with field path.
--
-- Provides actionable error messages for LLM output parsing failures.
--
-- @
-- ParseDiagnostic
--   { pdPath = ["signatures", "0", "name"]
--   , pdExpected = "string"
--   , pdActual = "number: 42"
--   , pdMessage = "Expected string but got number"
--   }
-- @
data ParseDiagnostic = ParseDiagnostic
  { pdPath :: [Text]
    -- ^ Field path from root to error location.
    -- e.g., ["root", "nested", "field"] or ["items", "0", "name"]
  , pdExpected :: Text
    -- ^ What type/value was expected.
  , pdActual :: Text
    -- ^ What was actually received (summarized).
  , pdMessage :: Text
    -- ^ Human-readable error message.
  }
  deriving (Show, Eq)


-- | Format diagnostic for display.
--
-- @
-- Parse error at: signatures.0.name
-- Expected: string
-- Got: number: 42
-- Expected string but got number
-- @
formatDiagnostic :: ParseDiagnostic -> Text
formatDiagnostic pd = T.unlines
  [ "Parse error at: " <> formatPath pd.pdPath
  , "Expected: " <> pd.pdExpected
  , "Got: " <> pd.pdActual
  , pd.pdMessage
  ]
  where
    formatPath [] = "(root)"
    formatPath ps = T.intercalate "." ps


-- ════════════════════════════════════════════════════════════════════════════
-- SMART CONSTRUCTORS
-- ════════════════════════════════════════════════════════════════════════════

-- | Error for when an object was expected.
expectedObject :: [Text] -> Value -> ParseDiagnostic
expectedObject path v = ParseDiagnostic
  { pdPath = path
  , pdExpected = "object"
  , pdActual = describeValue v
  , pdMessage = "Expected JSON object but got " <> describeValue v
  }

-- | Error for when an array was expected.
expectedArray :: [Text] -> Value -> ParseDiagnostic
expectedArray path v = ParseDiagnostic
  { pdPath = path
  , pdExpected = "array"
  , pdActual = describeValue v
  , pdMessage = "Expected JSON array but got " <> describeValue v
  }

-- | Error for when a string was expected.
expectedString :: [Text] -> Value -> ParseDiagnostic
expectedString path v = ParseDiagnostic
  { pdPath = path
  , pdExpected = "string"
  , pdActual = describeValue v
  , pdMessage = "Expected string but got " <> describeValue v
  }

-- | Error for when a number was expected.
expectedNumber :: [Text] -> Value -> ParseDiagnostic
expectedNumber path v = ParseDiagnostic
  { pdPath = path
  , pdExpected = "number"
  , pdActual = describeValue v
  , pdMessage = "Expected number but got " <> describeValue v
  }

-- | Error for when a boolean was expected.
expectedBool :: [Text] -> Value -> ParseDiagnostic
expectedBool path v = ParseDiagnostic
  { pdPath = path
  , pdExpected = "boolean"
  , pdActual = describeValue v
  , pdMessage = "Expected boolean but got " <> describeValue v
  }

-- | Error for a missing required field.
missingField :: [Text] -> ParseDiagnostic
missingField path = ParseDiagnostic
  { pdPath = path
  , pdExpected = "required field"
  , pdActual = "missing"
  , pdMessage = "Required field '" <> fieldName <> "' is missing"
  }
  where
    fieldName = case path of
      [] -> "(unknown)"
      ps -> last ps

-- | Generic type mismatch error.
typeMismatch :: [Text] -> Text -> Value -> ParseDiagnostic
typeMismatch path expected v = ParseDiagnostic
  { pdPath = path
  , pdExpected = expected
  , pdActual = describeValue v
  , pdMessage = "Type mismatch: expected " <> expected <> " but got " <> describeValue v
  }

-- | Custom error with user-provided message.
customError :: [Text] -> Text -> ParseDiagnostic
customError path msg = ParseDiagnostic
  { pdPath = path
  , pdExpected = ""
  , pdActual = ""
  , pdMessage = msg
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
