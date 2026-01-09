{-# LANGUAGE OverloadedStrings #-}
-- | Validate access paths against schemas.
module Text.Ginger.TH.Validate
  ( validatePath
  , validatePaths
  , formatValidationError
  , formatValidationErrors
  , formatValidationErrorsWithSource
  , SchemaRegistry
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Parsec.Pos (SourcePos, sourceName, sourceLine, sourceColumn)

import Text.Ginger.TH.Types

-- | Registry mapping type names to their schemas.
-- Used to resolve RecursiveRef during validation.
type SchemaRegistry = Map Text Schema

-- | Validate multiple access paths against a schema.
validatePaths :: SchemaRegistry -> Schema -> [AccessPath] -> [ValidationError]
validatePaths registry schema = mapMaybe (validatePath registry schema)

-- | Validate a single access path against a schema.
-- Returns Nothing if valid, Just error if invalid.
--
-- For existence checks (paths inside @is defined@), we validate only the prefix
-- (all but the last segment) because the final segment is what we're checking for.
validatePath :: SchemaRegistry -> Schema -> AccessPath -> Maybe ValidationError
validatePath registry schema access@(AccessPath root segments _ narrowed isExistenceCheck) =
  -- Check if the full access path is guarded by an `is defined` check
  let isGuarded = isNarrowedBy access
      -- For existence checks, validate only the prefix (drop last segment)
      segmentsToValidate = if isExistenceCheck && not (null segments)
                           then init segments
                           else segments
  in case lookupRoot registry root schema isGuarded of
       Nothing ->
         -- For existence checks with no segments, the root itself is being checked
         -- So if it doesn't exist, that's OK - that's what we're checking
         if isExistenceCheck && null segments
         then Nothing
         else Just $ FieldNotFound access root
       Just fieldSchema -> validateSegments registry fieldSchema segmentsToValidate access

-- | Look up a root variable in the schema.
-- The isGuarded parameter indicates if the access is guarded by `is defined`.
lookupRoot :: SchemaRegistry -> Text -> Schema -> Bool -> Maybe Schema
lookupRoot registry name schema isGuarded = case resolveSchema registry schema of
  RecordSchema fields -> lookup name fields
  SumSchema constructors -> lookupSumField name constructors isGuarded
  OpaqueSchema _ -> Nothing  -- Can't look up fields in opaque types
  _ -> Nothing  -- Can't look up in List, Scalar, or unresolved RecursiveRef

-- | Resolve a RecursiveRef to its actual schema.
resolveSchema :: SchemaRegistry -> Schema -> Schema
resolveSchema registry (RecursiveRef typeName) =
  case Map.lookup typeName registry of
    Just s -> s
    Nothing -> ScalarSchema  -- Fallback if not found (shouldn't happen)
resolveSchema _ schema = schema

-- | Look up a field in a sum type.
-- Constructors are stored as (constructorName, [(fieldName, fieldSchema)]).
--
-- Valid accesses:
-- 1. Constructor name access (e.g., @status.Blocked@): Always valid.
--    When guarded by @is defined@, returns the constructor's inner schema.
--    This enables the pattern: @{% if status.Blocked is defined %}{{ status.Blocked }}{% endif %}@
-- 2. Field access: Must exist in ALL constructors (unless guarded).
lookupSumField :: Text -> [(Text, [(Text, Schema)])] -> Bool -> Maybe Schema
lookupSumField name constructors isGuarded =
  -- First, check if 'name' is a constructor name
  case lookup name constructors of
    Just fields ->
      -- It's a constructor name access (e.g., status.Blocked)
      -- Return the schema of the constructor's inner value
      Just $ constructorInnerSchema fields
    Nothing ->
      -- Not a constructor name, look for a field in constructors
      lookupFieldInConstructors name constructors isGuarded

-- | Get the schema for a constructor's inner value.
-- For record constructors with multiple fields, returns a RecordSchema.
-- For single-field constructors, returns that field's schema directly.
-- For nullary constructors, returns ScalarSchema (truthy value).
constructorInnerSchema :: [(Text, Schema)] -> Schema
constructorInnerSchema [] = ScalarSchema  -- Nullary constructor
constructorInnerSchema [(_, schema)] = schema  -- Single field: unwrap
constructorInnerSchema fields = RecordSchema fields  -- Multi-field: nested record

-- | Look up a field across all constructors of a sum type.
lookupFieldInConstructors :: Text -> [(Text, [(Text, Schema)])] -> Bool -> Maybe Schema
lookupFieldInConstructors fieldName constructors isGuarded =
  let fieldSets = map snd constructors
      lookups = map (lookup fieldName) fieldSets
      presentIn = mapMaybe id lookups
  in if null presentIn
     then Nothing  -- Field doesn't exist in any constructor
     else if isGuarded
       -- Guarded: OK if exists in at least one constructor with consistent schema
       then if allSame presentIn
            then Just (head presentIn)
            else Nothing  -- Schema mismatch across constructors
       -- Not guarded: must exist in ALL constructors
       else if all isJust lookups && allSame presentIn
            then Just (head presentIn)
            else Nothing
  where
    isJust (Just _) = True
    isJust Nothing = False

    allSame [] = True
    allSame (x:xs) = all (schemaEq x) xs

    -- Simple structural equality for schemas
    schemaEq ScalarSchema ScalarSchema = True
    schemaEq (ListSchema a) (ListSchema b) = schemaEq a b
    schemaEq (RecordSchema a) (RecordSchema b) =
      length a == length b &&
      all (\((n1, s1), (n2, s2)) -> n1 == n2 && schemaEq s1 s2) (zip a b)
    schemaEq (SumSchema a) (SumSchema b) =
      length a == length b  -- Simplified comparison
    schemaEq _ _ = False

-- | Validate path segments against a schema.
validateSegments :: SchemaRegistry -> Schema -> [PathSegment] -> AccessPath -> Maybe ValidationError
validateSegments _ _ [] _ = Nothing  -- Reached end of path, valid

validateSegments registry schema (seg:rest) access =
  -- Check if the full access path is guarded
  let isGuarded = isNarrowedBy access
  -- First resolve any RecursiveRef
  in case (resolveSchema registry schema, seg) of
    -- Record with static key access
    (RecordSchema fields, StaticKey key) ->
      case lookup key fields of
        Nothing -> Just $ FieldNotFound access key
        Just fieldSchema -> validateSegments registry fieldSchema rest access

    -- Record with dynamic key access - NOT ALLOWED
    (RecordSchema _, DynamicKey) ->
      Just $ DynamicAccessNotAllowed access

    -- Sum type with static key access
    (SumSchema constructors, StaticKey key) ->
      case lookupSumField key constructors isGuarded of
        Nothing -> Just $ FieldNotInAllConstructors access key
        Just fieldSchema -> validateSegments registry fieldSchema rest access

    -- Sum type with dynamic key access - NOT ALLOWED
    (SumSchema _, DynamicKey) ->
      Just $ DynamicAccessNotAllowed access

    -- List with any key access (index)
    (ListSchema elemSchema, _) ->
      validateSegments registry elemSchema rest access

    -- Scalar with any key access - NOT ALLOWED
    (ScalarSchema, _) ->
      Just $ AccessOnScalar access

    -- Opaque type with any key access - NOT ALLOWED
    -- This only triggers if the template actually tries to access through it
    (OpaqueSchema reason, _) ->
      Just $ AccessOnOpaqueType access reason

    -- RecursiveRef that couldn't be resolved (shouldn't happen)
    (RecursiveRef _, _) ->
      Just $ UnknownType access "unresolved recursive reference"

-- | Format a single validation error as a string.
formatValidationError :: ValidationError -> String
formatValidationError err = case err of
  FieldNotFound access field ->
    formatLocation (apSourcePos access) ++
    "  Error: Field '" ++ Text.unpack field ++ "' not found\n" ++
    "  In access: " ++ formatAccessPath access ++ "\n"

  FieldNotInAllConstructors access field ->
    formatLocation (apSourcePos access) ++
    "  Error: Field '" ++ Text.unpack field ++
    "' does not exist in all constructors\n" ++
    "  In access: " ++ formatAccessPath access ++ "\n" ++
    "  Hint: For sum types, accessed fields must exist in ALL constructors\n"

  DynamicAccessNotAllowed access ->
    formatLocation (apSourcePos access) ++
    "  Error: Dynamic key access [expr] is not allowed\n" ++
    "  In access: " ++ formatAccessPath access ++ "\n" ++
    "  Hint: Use static field access .field or [\"literal\"] instead\n"

  AccessOnScalar access ->
    formatLocation (apSourcePos access) ++
    "  Error: Cannot access field on scalar type\n" ++
    "  In access: " ++ formatAccessPath access ++ "\n"

  UnknownType access typeName ->
    formatLocation (apSourcePos access) ++
    "  Error: Unknown type '" ++ Text.unpack typeName ++ "'\n" ++
    "  In access: " ++ formatAccessPath access ++ "\n"

  AccessOnOpaqueType access reason ->
    formatLocation (apSourcePos access) ++
    "  Error: Cannot access field on opaque type\n" ++
    "  Reason: " ++ Text.unpack reason ++ "\n" ++
    "  In access: " ++ formatAccessPath access ++ "\n" ++
    "  Hint: This type is not template-friendly. Either avoid accessing it,\n" ++
    "        or refactor it to use record syntax with named fields.\n"

-- | Format multiple validation errors.
formatValidationErrors :: [ValidationError] -> String
formatValidationErrors [] = ""
formatValidationErrors errors =
  "Template type checking failed:\n\n" ++
  concatMap formatValidationError errors

-- | Format validation errors with source context (Rust-style).
formatValidationErrorsWithSource :: String -> [ValidationError] -> String
formatValidationErrorsWithSource _ [] = ""
formatValidationErrorsWithSource src errors =
  "Template type checking failed:\n\n" ++
  concatMap (formatValidationErrorWithSource src) errors

-- | Format a single validation error with source context.
formatValidationErrorWithSource :: String -> ValidationError -> String
formatValidationErrorWithSource src err =
  let pos = getErrorPos err
      sourceLines = lines src
      lineNum = sourceLine pos
      col = sourceColumn pos
      maybeLine = listToMaybe $ drop (lineNum - 1) sourceLines
  in unlines $
    [ "error: " ++ getErrorMessage err
    , "  --> " ++ sourceName pos ++ ":" ++ show lineNum ++ ":" ++ show col
    , "   |"
    ] ++
    maybe [] (\line ->
      [ padLineNum lineNum ++ " | " ++ line
      , "   | " ++ replicate (col - 1) ' ' ++ "^"
      ]) maybeLine ++
    [ "   |"
    , "   = in access: " ++ formatAccessPath (getErrorAccess err)
    ] ++
    getHints err
  where
    padLineNum n = let s = show n in replicate (3 - length s) ' ' ++ s

-- | Get source position from validation error.
getErrorPos :: ValidationError -> SourcePos
getErrorPos (FieldNotFound access _) = apSourcePos access
getErrorPos (FieldNotInAllConstructors access _) = apSourcePos access
getErrorPos (DynamicAccessNotAllowed access) = apSourcePos access
getErrorPos (AccessOnScalar access) = apSourcePos access
getErrorPos (UnknownType access _) = apSourcePos access
getErrorPos (AccessOnOpaqueType access _) = apSourcePos access

-- | Get AccessPath from validation error.
getErrorAccess :: ValidationError -> AccessPath
getErrorAccess (FieldNotFound access _) = access
getErrorAccess (FieldNotInAllConstructors access _) = access
getErrorAccess (DynamicAccessNotAllowed access) = access
getErrorAccess (AccessOnScalar access) = access
getErrorAccess (UnknownType access _) = access
getErrorAccess (AccessOnOpaqueType access _) = access

-- | Get error message without location.
getErrorMessage :: ValidationError -> String
getErrorMessage (FieldNotFound _ field) =
  "field '" ++ Text.unpack field ++ "' not found"
getErrorMessage (FieldNotInAllConstructors _ field) =
  "field '" ++ Text.unpack field ++ "' not in all constructors"
getErrorMessage (DynamicAccessNotAllowed _) =
  "dynamic key access [expr] not allowed"
getErrorMessage (AccessOnScalar _) =
  "cannot access field on scalar type"
getErrorMessage (UnknownType _ typeName) =
  "unknown type '" ++ Text.unpack typeName ++ "'"
getErrorMessage (AccessOnOpaqueType _ _) =
  "cannot access field on opaque type"

-- | Get hint lines for an error.
getHints :: ValidationError -> [String]
getHints (FieldNotInAllConstructors _ _) =
  ["   = hint: for sum types, fields must exist in ALL constructors"]
getHints (DynamicAccessNotAllowed _) =
  ["   = hint: use static access .field or [\"literal\"] instead"]
getHints (AccessOnOpaqueType _ reason) =
  ["   = reason: " ++ Text.unpack reason]
getHints _ = []

-- | Format an access path for error messages.
formatAccessPath :: AccessPath -> String
formatAccessPath (AccessPath root segments _ _ _) =
  Text.unpack root ++ concatMap formatSegment segments
  where
    formatSegment (StaticKey k) = "." ++ Text.unpack k
    formatSegment DynamicKey = "[<expr>]"

-- | Format source location for error messages.
formatLocation :: SourcePos -> String
formatLocation pos =
  sourceName pos ++ ":" ++
  show (sourceLine pos) ++ ":" ++
  show (sourceColumn pos) ++ ":\n"
