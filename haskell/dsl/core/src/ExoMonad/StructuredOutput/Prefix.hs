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
module ExoMonad.StructuredOutput.Prefix
  ( -- * Field Label Modification
    stripFieldPrefix,
    defaultFieldLabel,
    makeStripPrefix,

    -- * Prefix Detection
    detectPrefix,
    commonPrefix,

    -- * Utilities
    lcFirst,
    ucFirst,
  )
where

import Data.Char (isLower, toLower, toUpper)

-- | Strip a known common prefix from a field name.
--
-- This function is NOT the default - use 'id' by default.
-- Only use this when you KNOW all fields share a common prefix.
--
-- @
-- stripFieldPrefix "tdTypeName"     = "typeName"
-- stripFieldPrefix "fsDescription"  = "description"
-- stripFieldPrefix "name"           = "name"  (no prefix to strip)
-- stripFieldPrefix "URL"            = "url"   (all caps -> lowercase)
-- @
stripFieldPrefix :: String -> String
stripFieldPrefix fieldName =
  let prefix = takeWhile isLower fieldName
      remainder = drop (length prefix) fieldName
   in case remainder of
        (c : cs) | not (null prefix) -> toLower c : cs
        _ -> lcFirst fieldName

-- | Default field label modifier.
--
-- Same as 'stripFieldPrefix' - exported with a more descriptive name
-- for use in 'StructuredOptions'.
defaultFieldLabel :: String -> String
defaultFieldLabel = stripFieldPrefix

-- | Create a field modifier that strips a specific prefix.
--
-- If the prefix is empty, returns 'lcFirst' (just lowercase first char).
-- Otherwise, strips the exact prefix and lowercases the next character.
--
-- @
-- makeStripPrefix "td" "tdTypeName"  = "typeName"
-- makeStripPrefix "td" "tdDataType"  = "dataType"
-- makeStripPrefix "td" "otherField"  = "otherField"  -- no match, unchanged
-- makeStripPrefix ""   "SomeField"   = "someField"   -- empty prefix = lcFirst
-- @
makeStripPrefix :: String -> (String -> String)
makeStripPrefix "" = lcFirst
makeStripPrefix prefix = \fieldName ->
  case stripPrefixExact prefix fieldName of
    Just (c : cs) -> toLower c : cs
    Just [] -> fieldName -- prefix was the whole name, keep as-is
    Nothing -> fieldName -- didn't match, keep as-is
  where
    stripPrefixExact :: String -> String -> Maybe String
    stripPrefixExact [] ys = Just ys
    stripPrefixExact _ [] = Nothing
    stripPrefixExact (x : xs) (y : ys)
      | x == y = stripPrefixExact xs ys
      | otherwise = Nothing

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
detectPrefix (x : xs) = foldl' commonPrefix (takeWhile isLower x) (map (takeWhile isLower) xs)

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
lcFirst (c : cs) = toLower c : cs

-- | Uppercase the first character of a string.
--
-- @
-- ucFirst "typeName" = "TypeName"
-- ucFirst "url" = "Url"
-- ucFirst "" = ""
-- @
ucFirst :: String -> String
ucFirst [] = []
ucFirst (c : cs) = toUpper c : cs
