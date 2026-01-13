{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Generate Schema from Haskell types via Template Haskell reification.
--
-- This module automatically detects and strips common prefixes from record
-- field names. For example, given:
--
-- @
-- data Spec = Spec
--   { specId :: Text
--   , specDescription :: Text
--   }
-- @
--
-- The generated schema will have field names @id@ and @description@ (not
-- @specId@ and @specDescription@), matching what templates typically expect.
module Text.Ginger.TH.Schema
  ( generateSchema
  , SchemaRegistry
  ) where

import Control.Monad (forM)
import Data.Char (isLower, isUpper, toLower)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.List (foldl', isPrefixOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift(..))

import Text.Ginger.TH.Types (Schema(..))

-- | Registry mapping type names to their schemas.
-- Used to resolve RecursiveRef during validation.
type SchemaRegistry = Map Text Schema

-- | Generate a Schema from a Haskell type name.
-- Returns both the schema and a registry of all type schemas encountered.
-- This is run at compile time via Template Haskell.
generateSchema :: Name -> Q (Schema, SchemaRegistry)
generateSchema typeName = do
  -- Use a memoization map to handle recursive types
  visited <- runIO $ newIORef Set.empty
  memo <- runIO $ newIORef Map.empty
  schema <- generateSchemaWithMemo visited memo typeName
  -- Convert the memo to a registry with Text keys
  memoMap <- runIO $ readIORef memo
  let registry = Map.mapKeys (Text.pack . nameBase) memoMap
  -- Also add the root type to the registry
  let rootName = Text.pack $ nameBase typeName
  let registry' = Map.insert rootName schema registry
  return (schema, registry')

-- | Generate schema with memoization for recursive types.
generateSchemaWithMemo :: IORef (Set Name) -> IORef (Map Name Schema) -> Name -> Q Schema
generateSchemaWithMemo visited memo typeName = do
  -- Check if we're already processing this type (recursion)
  inProgress <- runIO $ readIORef visited
  if typeName `Set.member` inProgress
    then do
      -- We're in a recursive type. Return a reference that can be
      -- resolved during validation by looking up the type name.
      return $ RecursiveRef (Text.pack $ nameBase typeName)
    else do
      -- Check memo
      memoized <- runIO $ readIORef memo
      case Map.lookup typeName memoized of
        Just schema -> return schema
        Nothing -> do
          -- Mark as in-progress
          runIO $ modifyIORef' visited (Set.insert typeName)
          -- Generate the schema
          schema <- generateSchemaForType visited memo typeName
          -- Cache it
          runIO $ modifyIORef' memo (Map.insert typeName schema)
          -- Remove from in-progress
          runIO $ modifyIORef' visited (Set.delete typeName)
          return schema

-- | Generate schema for a specific type.
generateSchemaForType :: IORef (Set Name) -> IORef (Map Name Schema) -> Name -> Q Schema
generateSchemaForType visited memo typeName = do
  info <- reify typeName
  case info of
    TyConI dec -> schemaFromDec visited memo dec
    _ -> fail $ "generateSchema: Expected a type, got: " ++ show typeName

-- | Generate schema from a type declaration.
-- Returns OpaqueSchema for polymorphic or unsupported declarations.
schemaFromDec :: IORef (Set Name) -> IORef (Map Name Schema) -> Dec -> Q Schema
schemaFromDec visited memo dec = case dec of
  -- Single constructor data type
  DataD _ _ tvs _ [con] _ ->
    case checkNoTypeVars' tvs of
      Just reason -> return $ OpaqueSchema reason
      Nothing -> schemaFromCon visited memo con

  -- Multiple constructors (sum type)
  DataD _ _ tvs _ cons _ ->
    case checkNoTypeVars' tvs of
      Just reason -> return $ OpaqueSchema reason
      Nothing -> do
        constructors <- mapM (constructorWithFields visited memo) cons
        return $ SumSchema constructors

  -- Newtype
  NewtypeD _ _ tvs _ con _ ->
    case checkNoTypeVars' tvs of
      Just reason -> return $ OpaqueSchema reason
      Nothing -> schemaFromCon visited memo con

  -- Type synonym
  TySynD _ tvs ty ->
    case checkNoTypeVars' tvs of
      Just reason -> return $ OpaqueSchema reason
      Nothing -> schemaFromType visited memo ty

  _ -> return $ OpaqueSchema $ "unsupported declaration: " <> Text.pack (show dec)

-- | Check that there are no type variables.
-- Returns Nothing if OK, Just reason if polymorphic.
checkNoTypeVars' :: [TyVarBndr a] -> Maybe Text
checkNoTypeVars' [] = Nothing
checkNoTypeVars' tvs = Just $
  "polymorphic type with variables: " <> Text.pack (show (map tvName tvs))
  where
    tvName (PlainTV n _) = n
    tvName (KindedTV n _ _) = n

-- | Generate schema from a constructor.
-- For unsupported constructor types, returns OpaqueSchema instead of failing.
-- This allows types with non-record constructors to exist in the type tree
-- as long as templates don't actually access through them.
--
-- Automatically detects and strips common prefixes from field names.
-- For example, @specId@, @specDescription@ become @id@, @description@.
schemaFromCon :: IORef (Set Name) -> IORef (Map Name Schema) -> Con -> Q Schema
schemaFromCon visited memo con = case con of
  RecC _ fields -> do
    -- Collect all field names and detect common prefix
    let rawNames = map (\(fieldName, _, _) -> nameBase fieldName) fields
        commonPfx = detectPrefix rawNames
        stripPrefix = makeStripPrefix commonPfx
    -- Generate schema with stripped field names
    fieldSchemas <- forM fields $ \(fieldName, _, fieldType) -> do
      schema <- schemaFromType visited memo fieldType
      let strippedName = stripPrefix (nameBase fieldName)
      return (Text.pack strippedName, schema)
    return $ RecordSchema fieldSchemas

  NormalC name _ ->
    return $ OpaqueSchema $ "non-record constructor '" <> Text.pack (nameBase name) <> "'"

  InfixC _ name _ ->
    return $ OpaqueSchema $ "infix constructor '" <> Text.pack (nameBase name) <> "'"

  ForallC _ _ _ ->
    return $ OpaqueSchema "existential type"

  GadtC _ _ _ ->
    return $ OpaqueSchema "GADT"

  RecGadtC _ _ _ ->
    return $ OpaqueSchema "record GADT"

-- | Extract constructor name and fields from a constructor (for sum types).
-- Returns (constructorName, fields) where fields may be empty for non-record constructors.
-- This allows sum types with mixed constructor styles to be used in templates,
-- as long as only record constructors' fields are accessed.
--
-- Automatically detects and strips common prefixes from field names.
constructorWithFields :: IORef (Set Name) -> IORef (Map Name Schema) -> Con -> Q (Text, [(Text, Schema)])
constructorWithFields visited memo con = case con of
  RecC name fields -> do
    -- Collect all field names and detect common prefix
    let rawNames = map (\(fieldName, _, _) -> nameBase fieldName) fields
        commonPfx = detectPrefix rawNames
        stripPrefix = makeStripPrefix commonPfx
    -- Generate schema with stripped field names
    fieldSchemas <- forM fields $ \(fieldName, _, fieldType) -> do
      schema <- schemaFromType visited memo fieldType
      let strippedName = stripPrefix (nameBase fieldName)
      return (Text.pack strippedName, schema)
    return (Text.pack $ nameBase name, fieldSchemas)

  NormalC name _ ->
    -- Non-record constructor, no named fields accessible
    return (Text.pack $ nameBase name, [])

  InfixC _ name _ ->
    -- Infix constructor, no named fields accessible
    return (Text.pack $ nameBase name, [])

  ForallC _ _ inner ->
    -- Unwrap the forall and try the inner constructor
    constructorWithFields visited memo inner

  GadtC names _ _ ->
    -- GADT constructor, no named fields accessible
    -- Use first name if multiple
    let name = case names of
          (n:_) -> n
          [] -> error "GadtC with no names"
    in return (Text.pack $ nameBase name, [])

  RecGadtC names fields _ -> do
    -- Record GADT has named fields
    let name = case names of
          (n:_) -> n
          [] -> error "RecGadtC with no names"
    -- Collect all field names and detect common prefix
    let rawNames = map (\(fieldName, _, _) -> nameBase fieldName) fields
        commonPfx = detectPrefix rawNames
        stripPrefix = makeStripPrefix commonPfx
    -- Generate schema with stripped field names
    fieldSchemas <- forM fields $ \(fieldName, _, fieldType) -> do
      schema <- schemaFromType visited memo fieldType
      let strippedName = stripPrefix (nameBase fieldName)
      return (Text.pack strippedName, schema)
    return (Text.pack $ nameBase name, fieldSchemas)

-- | Generate schema from a Type.
-- Returns OpaqueSchema for unsupported types instead of failing.
-- This allows complex types to exist in the tree as long as templates
-- don't actually access through them.
schemaFromType :: IORef (Set Name) -> IORef (Map Name Schema) -> Type -> Q Schema
schemaFromType visited memo ty = case ty of
  -- List type [a]
  AppT ListT elemType -> do
    elemSchema <- schemaFromType visited memo elemType
    return $ ListSchema elemSchema

  -- Maybe a - treat as the inner type (accessing Maybe field might be null)
  AppT (ConT maybeName) innerType
    | nameBase maybeName == "Maybe" -> do
        schemaFromType visited memo innerType

  -- Vector a
  AppT (ConT vectorName) elemType
    | nameBase vectorName == "Vector" -> do
        elemSchema <- schemaFromType visited memo elemType
        return $ ListSchema elemSchema

  -- Either a b - opaque (could support in future)
  AppT (AppT (ConT eitherName) _) _
    | nameBase eitherName == "Either" ->
        return $ OpaqueSchema "Either type"

  -- Tuple types - opaque
  TupleT _ ->
    return $ OpaqueSchema "tuple type"
  AppT (TupleT _) _ ->
    return $ OpaqueSchema "tuple type"

  -- Known scalar types
  ConT name
    | isScalarType name -> return ScalarSchema

  -- Other named types - recurse
  ConT name -> generateSchemaWithMemo visited memo name

  -- Type variable - opaque (polymorphic field)
  VarT name ->
    return $ OpaqueSchema $ "type variable '" <> Text.pack (nameBase name) <> "'"

  -- Type application we don't recognize - opaque
  AppT _ _ ->
    return $ OpaqueSchema $ "unsupported type application: " <> Text.pack (show ty)

  -- Other type forms - opaque
  _ ->
    return $ OpaqueSchema $ "unsupported type: " <> Text.pack (show ty)

-- | Check if a type name is a known scalar type.
isScalarType :: Name -> Bool
isScalarType name = nameBase name `elem` scalarTypeNames

-- | List of scalar type names we recognize.
scalarTypeNames :: [String]
scalarTypeNames =
  [ "Text"
  , "String"
  , "Int"
  , "Integer"
  , "Double"
  , "Float"
  , "Bool"
  , "Scientific"
  , "Day"
  , "TimeOfDay"
  , "LocalTime"
  , "ZonedTime"
  , "UTCTime"
  , "Char"
  , "ByteString"
  ]

-- ════════════════════════════════════════════════════════════════════════════
-- PREFIX STRIPPING (for template-friendly field names)
-- ════════════════════════════════════════════════════════════════════════════

-- | Detect the common lowercase prefix from a list of field names.
-- Convention: type FooBar has fields prefixed with "fb" (lowercase acronym).
--
-- @
-- detectPrefix ["sId", "sDescription"] = "s"
-- detectPrefix ["irCommitHash", "irIterations"] = "ir"
-- detectPrefix ["name", "age"] = ""  (no common lowercase prefix)
-- @
detectPrefix :: [String] -> String
detectPrefix [] = ""
detectPrefix ss = takeWhile isLower $ foldl1 commonPrefix ss
  where
    commonPrefix (a:as) (b:bs) | a == b = a : commonPrefix as bs
    commonPrefix _ _ = []

-- | Create a field modifier that strips a specific prefix.
--
-- @
-- makeStripPrefix "spec" "specId" = "id"
-- makeStripPrefix "spec" "specDescription" = "description"
-- makeStripPrefix "spec" "other" = "other"  (no match, unchanged)
-- @
makeStripPrefix :: String -> (String -> String)
makeStripPrefix "" = lcFirst
makeStripPrefix prefix = \fieldName ->
  case stripPrefixExact prefix fieldName of
    Just (c:cs) -> toLower c : cs
    Just []     -> fieldName  -- prefix was the whole name, keep as-is
    Nothing     -> fieldName  -- didn't match, keep as-is
  where
    stripPrefixExact :: String -> String -> Maybe String
    stripPrefixExact [] ys = Just ys
    stripPrefixExact _ [] = Nothing
    stripPrefixExact (x:xs) (y:ys)
      | x == y    = stripPrefixExact xs ys
      | otherwise = Nothing

-- | Lowercase the first character of a string.
lcFirst :: String -> String
lcFirst [] = []
lcFirst (c:cs) = toLower c : cs