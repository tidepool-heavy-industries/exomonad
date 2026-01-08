-- | Field name prefix detection and stripping.
--
-- Automatically transforms Haskell record field names to cleaner JSON keys
-- by detecting and stripping common prefixes.
--
-- @
-- -- Given record with fields: tdTypeName, tdDataType, tdSignatures
-- stripFieldPrefix "tdTypeName"  = "typeName"
-- stripFieldPrefix "tdDataType"  = "dataType"
-- stripFieldPrefix "tdSignatures" = "signatures"
-- @
module Tidepool.StructuredOutput.Prefix
  ( -- * Field Label Modification
    stripFieldPrefix
  , defaultFieldLabel

    -- * Prefix Detection
  , detectPrefix
  , commonPrefix

    -- * Utilities
  , lcFirst
  , ucFirst
  ) where

import Data.Char (isLower, toLower, toUpper)
import Data.List (foldl')


-- | Strip common lowercase prefix from a field name and lowercase the first char.
--
-- This is the default field label modifier for 'StructuredOutput'.
--
-- Algorithm:
-- 1. Find the lowercase prefix (characters before first uppercase)
-- 2. Strip that prefix
-- 3. Lowercase the first character of the remainder
--
-- @
-- stripFieldPrefix "tdTypeName"     = "typeName"
-- stripFieldPrefix "fsDescription"  = "description"
-- stripFieldPrefix "trBuildPassed"  = "buildPassed"
-- stripFieldPrefix "name"           = "name"  (no prefix to strip)
-- stripFieldPrefix "URL"            = "url"   (all caps -> lowercase)
-- @
stripFieldPrefix :: String -> String
stripFieldPrefix fieldName =
  let prefix = takeWhile isLower fieldName
      remainder = drop (length prefix) fieldName
  in case remainder of
       -- If there's a remainder after stripping prefix, use it (lowercased first char)
       (c:cs) | not (null prefix) -> toLower c : cs
       -- No uppercase found after prefix, or no prefix - just lowercase first char
       _ -> lcFirst fieldName


-- | Default field label modifier.
--
-- Same as 'stripFieldPrefix' - exported with a more descriptive name
-- for use in 'StructuredOptions'.
defaultFieldLabel :: String -> String
defaultFieldLabel = stripFieldPrefix


-- | Detect the common lowercase prefix from a list of field names.
--
-- Used during schema generation to detect the shared prefix across all fields.
--
-- @
-- detectPrefix ["tdTypeName", "tdDataType", "tdSignatures"] = "td"
-- detectPrefix ["fsName", "fsSignature", "fsDescription"] = "fs"
-- detectPrefix ["name", "age", "email"] = ""  (no common prefix)
-- detectPrefix ["userName", "userId"] = "user"  (longer prefix)
-- @
detectPrefix :: [String] -> String
detectPrefix [] = ""
detectPrefix [x] = takeWhile isLower x
detectPrefix (x:xs) = foldl' commonPrefix (takeWhile isLower x) (map (takeWhile isLower) xs)


-- | Find the longest common prefix of two strings.
--
-- @
-- commonPrefix "tdTypeName" "tdDataType" = "td"
-- commonPrefix "hello" "help" = "hel"
-- commonPrefix "abc" "xyz" = ""
-- @
commonPrefix :: String -> String -> String
commonPrefix a b = map fst $ takeWhile (uncurry (==)) $ zip a b


-- | Lowercase the first character of a string.
--
-- @
-- lcFirst "TypeName" = "typeName"
-- lcFirst "URL" = "uRL"
-- lcFirst "" = ""
-- @
lcFirst :: String -> String
lcFirst [] = []
lcFirst (c:cs) = toLower c : cs


-- | Uppercase the first character of a string.
--
-- @
-- ucFirst "typeName" = "TypeName"
-- ucFirst "url" = "Url"
-- ucFirst "" = ""
-- @
ucFirst :: String -> String
ucFirst [] = []
ucFirst (c:cs) = toUpper c : cs
