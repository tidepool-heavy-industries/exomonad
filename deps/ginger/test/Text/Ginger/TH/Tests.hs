{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Tests for the compile-time type-checked template system.
module Text.Ginger.TH.Tests
  ( thTests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Control.Monad (forM_)
import Data.List (sort, isSuffixOf, isInfixOf)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Text.Parsec.Pos (newPos, SourcePos)

import Text.Ginger.AST
import Text.Ginger.Parse (parseGinger', mkParserOptions, ParserOptions(..))
import Text.Ginger.TH (jinja, typedTemplateFile, TypedTemplate(..), TemplateDependency(..), DepRelation(..), DepLocation(..), TemplateContextInfo(..), flattenDeps, absolutePaths, relativePaths)
import Text.Ginger.TH.Types
import Text.Ginger.TH.Builtins (isBuiltin, builtinNames)
import Text.Ginger.TH.Extract (extractFromTemplate, extractVariableAccesses)
import Text.Ginger.TH.Schema (generateSchema, SchemaRegistry)
import Text.Ginger.TH.Validate (validatePath, validatePaths, formatValidationError)
import Text.Ginger.GVal (GVal, ToGVal(..), asText, asBoolean, asLookup, isNull)
import Text.Ginger.GVal.Generic (genericToGVal)

-- Import test types from separate module (required for TH to see them)
import Text.Ginger.TH.TestTypes

-- | All TH-related tests
thTests :: TestTree
thTests = testGroup "Template Haskell Type Checking"
  [ builtinTests
  , extractionTests
  , validationTests
  , schemaGenerationTests
  , narrowingTests
  , endToEndTests
  , includeValidationTests
  , genericToGValTests
  , quasiQuoterTests
  , dependencyTests
  , propertyTests
  ]

--------------------------------------------------------------------------------
-- Builtin Tests
--------------------------------------------------------------------------------

builtinTests :: TestTree
builtinTests = testGroup "Builtins"
  [ testCase "common functions are builtins" $ do
      assertBool "length is builtin" (isBuiltin "length")
      assertBool "upper is builtin" (isBuiltin "upper")
      assertBool "filter is builtin" (isBuiltin "filter")
      assertBool "map is builtin" (isBuiltin "map")

  , testCase "loop is not a builtin (it's context-dependent)" $ do
      assertBool "loop is not a global builtin" (not $ isBuiltin "loop")

  , testCase "user variables are not builtins" $ do
      assertBool "userName is not builtin" (not $ isBuiltin "userName")
      assertBool "items is not builtin" (not $ isBuiltin "items")

  , testCase "boolean literals are builtins" $ do
      assertBool "true is builtin" (isBuiltin "true")
      assertBool "false is builtin" (isBuiltin "false")
      assertBool "null is builtin" (isBuiltin "null")
  ]

--------------------------------------------------------------------------------
-- Extraction Tests
--------------------------------------------------------------------------------

extractionTests :: TestTree
extractionTests = testGroup "Variable Extraction"
  [ testCase "simple variable" $ do
      paths <- parseAndExtract "{{ name }}"
      assertEqual "should extract one path" 1 (length paths)
      assertEqual "root should be 'name'" "name" (apRoot $ head paths)
      assertEqual "path should be empty" [] (apPath $ head paths)

  , testCase "nested field access" $ do
      paths <- parseAndExtract "{{ user.profile.name }}"
      assertEqual "should extract one path" 1 (length paths)
      assertEqual "root should be 'user'" "user" (apRoot $ head paths)
      assertEqual "path should have two segments"
        [StaticKey "profile", StaticKey "name"]
        (apPath $ head paths)

  , testCase "multiple variables" $ do
      paths <- parseAndExtract "{{ name }} - {{ email }}"
      assertEqual "should extract two paths" 2 (length paths)
      let roots = Set.fromList $ map apRoot paths
      assertBool "should have 'name'" (Set.member "name" roots)
      assertBool "should have 'email'" (Set.member "email" roots)

  , testCase "builtin functions are excluded" $ do
      paths <- parseAndExtract "{{ items | length }}"
      -- 'items' should be extracted, 'length' should not (it's a builtin)
      let userPaths = filter (not . isBuiltin . apRoot) paths
      assertEqual "should extract one user path" 1 (length userPaths)
      assertEqual "root should be 'items'" "items" (apRoot $ head userPaths)

  , testCase "for loop binds variables" $ do
      paths <- parseAndExtract "{% for item in items %}{{ item.name }}{% endfor %}"
      -- 'items' is free, 'item' is bound by the for loop
      let userPaths = filter (not . isBuiltin . apRoot) paths
      assertEqual "should extract one path (items)" 1 (length userPaths)
      assertEqual "root should be 'items'" "items" (apRoot $ head userPaths)

  , testCase "for loop binds loop variable" $ do
      paths <- parseAndExtract "{% for x in items %}{{ loop.index }}{% endfor %}"
      -- 'items' is free, 'loop' is bound by the for loop
      let userPaths = filter (not . isBuiltin . apRoot) paths
      assertEqual "should extract one path (items)" 1 (length userPaths)

  , testCase "set binds variable in subsequent statements" $ do
      -- 'value' is free, 'x' is bound by set and available in subsequent statements
      paths <- parseAndExtract "{% set x = value %}{{ x }}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      -- Only 'value' should be extracted, 'x' is bound
      assertEqual "should extract only 'value'" 1 (length userPaths)
      assertEqual "should extract 'value'" "value" (apRoot $ head userPaths)

  , testCase "set binds variable for nested access" $ do
      -- Verify set binding works for nested access too
      paths <- parseAndExtract "{% set user = data %}{{ user.name }}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      -- Only 'data' should be extracted, 'user.name' is bound
      assertEqual "should extract only 'data'" 1 (length userPaths)
      assertEqual "should extract 'data'" "data" (apRoot $ head userPaths)

  , testCase "set variable used before definition is free" $ do
      -- If a variable is used before set, it's still free at that point
      paths <- parseAndExtract "{{ x }}{% set x = 1 %}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      -- 'x' used before set should be extracted
      assertEqual "should extract 'x'" 1 (length userPaths)
      assertEqual "should be 'x'" "x" (apRoot $ head userPaths)

  , testCase "lambda binds parameters" $ do
      paths <- parseAndExtract "{{ items | map((x) -> x.name) }}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      -- 'items' is free, 'x' is bound by lambda
      assertEqual "should extract one user path" 1 (length userPaths)
      assertEqual "root should be 'items'" "items" (apRoot $ head userPaths)

  , testCase "dynamic key access is marked" $ do
      paths <- parseAndExtract "{{ data[key] }}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      -- Should have 'data' with DynamicKey, and 'key' as separate access
      let dataPaths = filter (\p -> apRoot p == "data") userPaths
      assertEqual "should have data path" 1 (length dataPaths)
      assertEqual "data path should have DynamicKey"
        [DynamicKey]
        (apPath $ head dataPaths)

  , testCase "bracket with string literal is static" $ do
      paths <- parseAndExtract "{{ data[\"field\"] }}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      assertEqual "should extract one path" 1 (length userPaths)
      assertEqual "path should have StaticKey"
        [StaticKey "field"]
        (apPath $ head userPaths)

  , testCase "macro parameters are bound" $ do
      paths <- parseAndExtract "{% macro greet(name) %}Hello {{ name }}{% endmacro %}"
      -- 'name' is bound by the macro definition
      let userPaths = filter (not . isBuiltin . apRoot) paths
      assertEqual "should extract no user paths" 0 (length userPaths)

  , testCase "catch binds exception variable" $ do
      paths <- parseAndExtract "{% try %}{{ risky }}{% catch * as e %}{{ e }}{% endtry %}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      -- 'risky' is free, 'e' is bound by catch
      assertEqual "should extract one user path" 1 (length userPaths)
      assertEqual "root should be 'risky'" "risky" (apRoot $ head userPaths)
  ]

--------------------------------------------------------------------------------
-- Validation Tests
--------------------------------------------------------------------------------

-- | Empty registry for simple tests
emptyRegistry :: SchemaRegistry
emptyRegistry = Map.empty

validationTests :: TestTree
validationTests = testGroup "Path Validation"
  [ testCase "valid field on record" $ do
      let schema = RecordSchema [("name", ScalarSchema), ("email", ScalarSchema)]
      let path = mkAccessPath "name" [] dummyPos
      assertEqual "should be valid" Nothing (validatePath emptyRegistry schema path)

  , testCase "valid nested field" $ do
      let profileSchema = RecordSchema [("bio", ScalarSchema)]
      let schema = RecordSchema [("profile", profileSchema)]
      let path = mkAccessPath "profile" [StaticKey "bio"] dummyPos
      assertEqual "should be valid" Nothing (validatePath emptyRegistry schema path)

  , testCase "invalid field on record" $ do
      let schema = RecordSchema [("name", ScalarSchema)]
      let path = mkAccessPath "email" [] dummyPos
      case validatePath emptyRegistry schema path of
        Nothing -> assertFailure "should be invalid"
        Just (FieldNotFound _ field) -> assertEqual "field name" "email" field
        Just other -> assertFailure $ "wrong error type: " ++ show other

  , testCase "invalid nested field" $ do
      let schema = RecordSchema [("name", ScalarSchema)]
      let path = mkAccessPath "name" [StaticKey "foo"] dummyPos
      case validatePath emptyRegistry schema path of
        Nothing -> assertFailure "should be invalid (can't access field on scalar)"
        Just (AccessOnScalar _) -> return ()
        Just other -> assertFailure $ "wrong error type: " ++ show other

  , testCase "dynamic key on record is error" $ do
      -- Access user[<dynamic>] where user is a record
      let schema = RecordSchema [("user", RecordSchema [("name", ScalarSchema)])]
      let path = mkAccessPath "user" [DynamicKey] dummyPos
      case validatePath emptyRegistry schema path of
        Nothing -> assertFailure "should be invalid"
        Just (DynamicAccessNotAllowed _) -> return ()
        Just other -> assertFailure $ "wrong error type: " ++ show other

  , testCase "sum type - field in all constructors" $ do
      let schema = SumSchema
            [ ("ConA", [("tag", ScalarSchema), ("name", ScalarSchema)])
            , ("ConB", [("tag", ScalarSchema), ("value", ScalarSchema)])
            ]
      let path = mkAccessPath "tag" [] dummyPos
      assertEqual "should be valid" Nothing (validatePath emptyRegistry schema path)

  , testCase "sum type - constructor name access always valid" $ do
      -- Accessing constructor name (e.g., status.ConA) is always valid
      let schema = SumSchema
            [ ("ConA", [("name", ScalarSchema)])
            , ("ConB", [("value", ScalarSchema)])
            ]
      let path = mkAccessPath "ConA" [] dummyPos
      assertEqual "constructor access should be valid" Nothing (validatePath emptyRegistry schema path)

  , testCase "sum type - field not in all constructors (root)" $ do
      -- At root level, missing field in some constructors returns FieldNotFound
      let schema = SumSchema
            [ ("ConA", [("tag", ScalarSchema), ("name", ScalarSchema)])
            , ("ConB", [("tag", ScalarSchema), ("value", ScalarSchema)])
            ]
      let path = mkAccessPath "name" [] dummyPos
      case validatePath emptyRegistry schema path of
        Nothing -> assertFailure "should be invalid"
        Just (FieldNotFound _ field) ->
          assertEqual "field name" "name" field
        Just other -> assertFailure $ "wrong error type: " ++ show other

  , testCase "sum type - field not in all constructors (nested)" $ do
      -- Nested access returns FieldNotInAllConstructors
      let innerSum = SumSchema
            [ ("ConX", [("x", ScalarSchema)])
            , ("ConY", [("y", ScalarSchema)])
            ]
      let schema = RecordSchema [("content", innerSum)]
      let path = mkAccessPath "content" [StaticKey "x"] dummyPos
      case validatePath emptyRegistry schema path of
        Nothing -> assertFailure "should be invalid"
        Just (FieldNotInAllConstructors _ field) ->
          assertEqual "field name" "x" field
        Just other -> assertFailure $ "wrong error type: " ++ show other

  , testCase "list index access is valid" $ do
      let schema = RecordSchema [("items", ListSchema ScalarSchema)]
      let path = mkAccessPath "items" [StaticKey "0"] dummyPos
      assertEqual "should be valid" Nothing (validatePath emptyRegistry schema path)

  , testCase "list with nested record access" $ do
      let itemSchema = RecordSchema [("name", ScalarSchema)]
      let schema = RecordSchema [("items", ListSchema itemSchema)]
      let path = mkAccessPath "items" [StaticKey "0", StaticKey "name"] dummyPos
      assertEqual "should be valid" Nothing (validatePath emptyRegistry schema path)

  , testCase "recursive type reference is resolved" $ do
      -- Simulate a recursive type: Tree = Node { value :: Int, children :: [Tree] }
      -- The RecursiveRef "Tree" should resolve to the Tree schema
      let treeSchema = RecordSchema
            [ ("value", ScalarSchema)
            , ("children", ListSchema (RecursiveRef "Tree"))
            ]
      let registry = Map.singleton "Tree" treeSchema
      -- Access tree.children.0.value should be valid
      let path = mkAccessPath "children" [StaticKey "0", StaticKey "value"] dummyPos
      assertEqual "should be valid" Nothing (validatePath registry treeSchema path)

  , testCase "recursive type nested access works" $ do
      -- Access tree.children.0.children.0.value (two levels deep)
      let treeSchema = RecordSchema
            [ ("value", ScalarSchema)
            , ("children", ListSchema (RecursiveRef "Tree"))
            ]
      let registry = Map.singleton "Tree" treeSchema
      let path = mkAccessPath "children" [StaticKey "0", StaticKey "children", StaticKey "0", StaticKey "value"] dummyPos
      assertEqual "should be valid" Nothing (validatePath registry treeSchema path)

  , testCase "error formatting includes location" $ do
      let schema = RecordSchema [("name", ScalarSchema)]
      let path = AccessPath "email" [] (newPos "test.html" 5 10) Set.empty False
      case validatePath emptyRegistry schema path of
        Nothing -> assertFailure "should be invalid"
        Just err -> do
          let formatted = formatValidationError err
          assertBool "should contain filename" ("test.html" `isInfixOf` formatted)
          assertBool "should contain line number" ("5" `isInfixOf` formatted)
  ]
  where
    isInfixOf needle haystack = needle `elem` words haystack ||
      any (needle `isPrefixOf`) (tails haystack)
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
    tails [] = [[]]
    tails s@(_:xs) = s : tails xs

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Parse a template string and extract variable accesses.
parseAndExtract :: String -> IO [AccessPath]
parseAndExtract src = do
  let opts = (mkParserOptions nullResolver) { poSourceName = Just "test" }
  result <- parseGinger' opts src
  case result of
    Left err -> error $ "Parse error: " ++ show err
    Right tpl -> return $ extractFromTemplate tpl

-- | Null resolver for testing (no includes).
nullResolver :: Monad m => String -> m (Maybe String)
nullResolver _ = return Nothing

-- | Parse and extract from a file with include support.
-- Uses a lookup table for includes (like simulation tests).
parseAndExtractWithIncludes :: FilePath -> String -> [(FilePath, String)] -> IO [AccessPath]
parseAndExtractWithIncludes sourcePath src includeLookup = do
  let resolver path = return $ lookup path includeLookup
  let opts = (mkParserOptions resolver) { poSourceName = Just sourcePath }
  result <- parseGinger' opts src
  case result of
    Left err -> error $ "Parse error: " ++ show err
    Right tpl -> return $ extractFromTemplate tpl

-- | Dummy source position for testing.
dummyPos :: SourcePos
dummyPos = newPos "test" 1 1

-- | Create an AccessPath with no narrowing context (for tests).
mkAccessPath :: Text -> [PathSegment] -> SourcePos -> AccessPath
mkAccessPath root segs pos = AccessPath root segs pos Set.empty False

--------------------------------------------------------------------------------
-- Schema Generation Tests (using compile-time TH)
--------------------------------------------------------------------------------

-- Generate schemas at compile time
simpleRecordSchema :: (Schema, SchemaRegistry)
simpleRecordSchema = $( do
  (s, r) <- generateSchema ''SimpleRecord
  [| (s, r) |]
 )

nestedRecordSchema :: (Schema, SchemaRegistry)
nestedRecordSchema = $( do
  (s, r) <- generateSchema ''NestedRecord
  [| (s, r) |]
 )

contentTypeSchema :: (Schema, SchemaRegistry)
contentTypeSchema = $( do
  (s, r) <- generateSchema ''ContentType
  [| (s, r) |]
 )

animalSchema :: (Schema, SchemaRegistry)
animalSchema = $( do
  (s, r) <- generateSchema ''Animal
  [| (s, r) |]
 )

treeSchema :: (Schema, SchemaRegistry)
treeSchema = $( do
  (s, r) <- generateSchema ''Tree
  [| (s, r) |]
 )

withListSchema :: (Schema, SchemaRegistry)
withListSchema = $( do
  (s, r) <- generateSchema ''WithList
  [| (s, r) |]
 )

withMaybeSchema :: (Schema, SchemaRegistry)
withMaybeSchema = $( do
  (s, r) <- generateSchema ''WithMaybe
  [| (s, r) |]
 )

withVectorSchema :: (Schema, SchemaRegistry)
withVectorSchema = $( do
  (s, r) <- generateSchema ''WithVector
  [| (s, r) |]
 )

userIdSchema :: (Schema, SchemaRegistry)
userIdSchema = $( do
  (s, r) <- generateSchema ''UserId
  [| (s, r) |]
 )

withTypeSynonymSchema :: (Schema, SchemaRegistry)
withTypeSynonymSchema = $( do
  (s, r) <- generateSchema ''WithTypeSynonym
  [| (s, r) |]
 )

-- Schemas for opaque type tests
withOpaqueFieldSchema :: (Schema, SchemaRegistry)
withOpaqueFieldSchema = $( do
  (s, r) <- generateSchema ''WithOpaqueField
  [| (s, r) |]
 )

complexWithOpaqueSchema :: (Schema, SchemaRegistry)
complexWithOpaqueSchema = $( do
  (s, r) <- generateSchema ''ComplexWithOpaque
  [| (s, r) |]
 )

mixedContentSchema :: (Schema, SchemaRegistry)
mixedContentSchema = $( do
  (s, r) <- generateSchema ''MixedContent
  [| (s, r) |]
 )

schemaGenerationTests :: TestTree
schemaGenerationTests = testGroup "Schema Generation"
  [ testCase "simple record generates RecordSchema" $ do
      let (schema, _) = simpleRecordSchema
      case schema of
        RecordSchema fields -> do
          assertEqual "should have 2 fields" 2 (length fields)
          assertBool "should have srName" $ any ((== "srName") . fst) fields
          assertBool "should have srAge" $ any ((== "srAge") . fst) fields
        _ -> assertFailure $ "expected RecordSchema, got: " ++ show schema

  , testCase "nested record generates nested schema" $ do
      let (schema, _) = nestedRecordSchema
      case schema of
        RecordSchema fields -> do
          assertEqual "should have 2 fields" 2 (length fields)
          case lookup "nrUser" fields of
            Just (RecordSchema innerFields) ->
              assertEqual "inner should have 2 fields" 2 (length innerFields)
            Just other -> assertFailure $ "expected RecordSchema for nrUser, got: " ++ show other
            Nothing -> assertFailure "missing nrUser field"
        _ -> assertFailure $ "expected RecordSchema, got: " ++ show schema

  , testCase "sum type generates SumSchema" $ do
      let (schema, _) = contentTypeSchema
      case schema of
        SumSchema constructors -> do
          assertEqual "should have 2 constructors" 2 (length constructors)
          -- Check constructor names exist
          let conNames = map fst constructors
          assertBool "should have TextContent" $ "TextContent" `elem` conNames
          assertBool "should have ImageContent" $ "ImageContent" `elem` conNames
          -- TextContent has 1 field, ImageContent has 2
          let fieldCounts = map (length . snd) constructors
          assertBool "field counts should be [1,2] or [2,1]" $
            sort fieldCounts == [1,2]
        _ -> assertFailure $ "expected SumSchema, got: " ++ show schema

  , testCase "sum type with shared field" $ do
      let (schema, registry) = animalSchema
      case schema of
        SumSchema constructors -> do
          assertEqual "should have 2 constructors" 2 (length constructors)
          -- Check constructor names
          let conNames = map fst constructors
          assertBool "should have Dog" $ "Dog" `elem` conNames
          assertBool "should have Cat" $ "Cat" `elem` conNames
          -- animalName should be in both constructors
          let hasAnimalName = all (any ((== "animalName") . fst) . snd) constructors
          assertBool "animalName should be in all constructors" hasAnimalName
          -- Validate that animalName access is valid
          let path = mkAccessPath "animalName" [] dummyPos
          assertEqual "animalName should be accessible" Nothing $
            validatePath registry schema path
        _ -> assertFailure $ "expected SumSchema, got: " ++ show schema

  , testCase "recursive type generates RecursiveRef" $ do
      let (schema, registry) = treeSchema
      case schema of
        RecordSchema fields -> do
          assertBool "should have treeChildren" $ any ((== "treeChildren") . fst) fields
          case lookup "treeChildren" fields of
            Just (ListSchema (RecursiveRef refName)) ->
              assertEqual "should reference Tree" "Tree" refName
            Just other -> assertFailure $ "expected ListSchema with RecursiveRef, got: " ++ show other
            Nothing -> assertFailure "missing treeChildren field"
          -- Validate deep access works
          let path = mkAccessPath "treeChildren" [StaticKey "0", StaticKey "treeValue"] dummyPos
          assertEqual "nested access should work" Nothing $
            validatePath registry schema path
        _ -> assertFailure $ "expected RecordSchema, got: " ++ show schema

  , testCase "list field generates ListSchema" $ do
      let (schema, _) = withListSchema
      case schema of
        RecordSchema fields ->
          case lookup "wlItems" fields of
            Just (ListSchema ScalarSchema) -> return ()
            Just other -> assertFailure $ "expected ListSchema ScalarSchema, got: " ++ show other
            Nothing -> assertFailure "missing wlItems field"
        _ -> assertFailure $ "expected RecordSchema, got: " ++ show schema

  , testCase "Maybe field treated as inner type" $ do
      let (schema, _) = withMaybeSchema
      case schema of
        RecordSchema fields -> do
          case lookup "wmOptional" fields of
            Just ScalarSchema -> return ()  -- Maybe Text -> Text -> ScalarSchema
            Just other -> assertFailure $ "expected ScalarSchema for Maybe, got: " ++ show other
            Nothing -> assertFailure "missing wmOptional field"
        _ -> assertFailure $ "expected RecordSchema, got: " ++ show schema

  , testCase "Vector field generates ListSchema" $ do
      let (schema, _) = withVectorSchema
      case schema of
        RecordSchema fields ->
          case lookup "wvItems" fields of
            Just (ListSchema ScalarSchema) -> return ()
            Just other -> assertFailure $ "expected ListSchema ScalarSchema, got: " ++ show other
            Nothing -> assertFailure "missing wvItems field"
        _ -> assertFailure $ "expected RecordSchema, got: " ++ show schema

  , testCase "newtype generates schema for wrapped type" $ do
      let (schema, _) = userIdSchema
      case schema of
        RecordSchema fields -> do
          assertEqual "should have 1 field" 1 (length fields)
          assertBool "should have unUserId" $ any ((== "unUserId") . fst) fields
        _ -> assertFailure $ "expected RecordSchema, got: " ++ show schema

  , testCase "type synonym is transparent" $ do
      let (schema, _) = withTypeSynonymSchema
      case schema of
        RecordSchema fields -> do
          case lookup "wtsEmail" fields of
            Just ScalarSchema -> return ()  -- Email = Text -> ScalarSchema
            Just other -> assertFailure $ "expected ScalarSchema for type synonym, got: " ++ show other
            Nothing -> assertFailure "missing wtsEmail field"
        _ -> assertFailure $ "expected RecordSchema, got: " ++ show schema

  -- Opaque type tests: types that schema generation doesn't fully understand
  -- should become OpaqueSchema rather than causing compile failure

  , testCase "record with opaque field generates schema (not accessed)" $ do
      -- WithOpaqueField has a Status field which is a non-record sum type
      -- Schema generation should succeed, and accessing wofName should work
      let (schema, registry) = withOpaqueFieldSchema
      case schema of
        RecordSchema fields -> do
          assertBool "should have wofName" $ any ((== "wofName") . fst) fields
          assertBool "should have wofStatus" $ any ((== "wofStatus") . fst) fields
          -- wofStatus should be opaque (SumSchema with empty field lists per constructor)
          case lookup "wofStatus" fields of
            Just (SumSchema constructors) -> do
              -- Non-record sum types have constructors with no named fields
              assertBool "opaque sum type has empty field sets" $
                all (null . snd) constructors
              -- But constructor names are present
              let conNames = map fst constructors
              assertBool "should have Active" $ "Active" `elem` conNames
              assertBool "should have Inactive" $ "Inactive" `elem` conNames
              assertBool "should have Pending" $ "Pending" `elem` conNames
            Just other -> assertFailure $ "expected SumSchema for Status, got: " ++ show other
            Nothing -> assertFailure "missing wofStatus field"
          -- Accessing wofName should succeed
          let namePath = mkAccessPath "wofName" [] dummyPos
          assertEqual "wofName access should be valid" Nothing $
            validatePath registry schema namePath
        _ -> assertFailure $ "expected RecordSchema, got: " ++ show schema

  , testCase "accessing opaque field fails at validation" $ do
      -- Trying to access status.something should fail
      let (schema, registry) = withOpaqueFieldSchema
      let statusPath = mkAccessPath "wofStatus" [StaticKey "payload"] dummyPos
      case validatePath registry schema statusPath of
        Just (FieldNotInAllConstructors _ _) -> return ()  -- Expected: field not in all constructors
        Just err -> return ()  -- Any validation error is acceptable
        Nothing -> assertFailure "should fail when accessing into opaque type"

  , testCase "non-record single constructor becomes opaque" $ do
      -- ComplexWithOpaque has Point fields which are positional
      let (schema, registry) = complexWithOpaqueSchema
      case schema of
        RecordSchema fields -> do
          -- cwoTitle should work
          let titlePath = mkAccessPath "cwoTitle" [] dummyPos
          assertEqual "cwoTitle should be valid" Nothing $
            validatePath registry schema titlePath
          -- cwoPoint should be opaque
          case lookup "cwoPoint" fields of
            Just (OpaqueSchema reason) ->
              assertBool "should mention non-record" $ "non-record" `Text.isInfixOf` reason
            Just other -> assertFailure $ "expected OpaqueSchema for Point, got: " ++ show other
            Nothing -> assertFailure "missing cwoPoint field"
        _ -> assertFailure $ "expected RecordSchema, got: " ++ show schema

  , testCase "mixed sum type (record + nullary) works" $ do
      -- MixedContent has TextBlock/ImageBlock with records, Divider without
      let (schema, registry) = mixedContentSchema
      case schema of
        SumSchema constructors -> do
          assertEqual "should have 3 constructors" 3 (length constructors)
          -- Check constructor names
          let conNames = map fst constructors
          assertBool "should have TextBlock" $ "TextBlock" `elem` conNames
          assertBool "should have ImageBlock" $ "ImageBlock" `elem` conNames
          assertBool "should have Divider" $ "Divider" `elem` conNames
          -- TextBlock and ImageBlock have mcText/mcUrl, Divider has nothing
          let fieldCounts = map (length . snd) constructors
          assertBool "should have 0, 1, and 1 fields" $
            sort fieldCounts == [0, 1, 1]
        _ -> assertFailure $ "expected SumSchema, got: " ++ show schema
  ]

--------------------------------------------------------------------------------
-- Narrowing Tests (is defined guards)
--------------------------------------------------------------------------------

narrowingTests :: TestTree
narrowingTests = testGroup "Narrowing (is defined guards)"
  [ testCase "guarded sum type field access" $ do
      -- {% if ctBody is defined %}{{ ctBody }}{% endif %}
      -- Should work - ctBody only in TextContent but access is guarded
      let (schema, registry) = contentTypeSchema
      paths <- parseAndExtract "{% if ctBody is defined %}{{ ctBody }}{% endif %}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      -- ctBody is extracted from condition (existence check) and body
      let ctBodyPaths = filter (\p -> apRoot p == "ctBody") userPaths
      -- Now we extract 2 paths: one for the `is defined` check, one for the body access
      assertEqual "should have 2 ctBody paths (condition + body)" 2 (length ctBodyPaths)
      -- The body access (non-existence-check) should be narrowed
      let bodyPaths = filter (not . apIsExistenceCheck) ctBodyPaths
      assertEqual "should have 1 body path" 1 (length bodyPaths)
      let narrowed = apNarrowed (head bodyPaths)
      assertBool "ctBody body access should be narrowed" (not $ Set.null narrowed)
      -- Validation should succeed because the body access is guarded
      let errors = validatePaths registry schema userPaths
      assertEqual "should have no errors (guarded access)" [] errors

  , testCase "unguarded sum type field rejected" $ do
      -- {{ ctBody }} alone (unguarded)
      let (schema, registry) = contentTypeSchema
      paths <- parseAndExtract "{{ ctBody }}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      let errors = validatePaths registry schema userPaths
      assertEqual "should have 1 error" 1 (length errors)

  , testCase "is undefined narrows in else branch" $ do
      -- {% if x is undefined %}...{% else %}{{ x }}{% endif %}
      -- x in else branch should be narrowed
      paths <- parseAndExtract "{% if x is undefined %}no{% else %}{{ x }}{% endif %}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      let xPaths = filter (\p -> apRoot p == "x") userPaths
      -- Now we extract 2 paths: one for `is undefined` check, one for else branch
      assertEqual "should have 2 x paths (condition + else branch)" 2 (length xPaths)
      -- The else branch access (non-existence-check) should be narrowed
      let bodyPaths = filter (not . apIsExistenceCheck) xPaths
      assertEqual "should have 1 body path" 1 (length bodyPaths)
      let narrowed = apNarrowed (head bodyPaths)
      assertBool "x in else branch should be narrowed" (not $ Set.null narrowed)

  , testCase "and narrows both sides" $ do
      -- {% if a is defined and b is defined %}{{ a }}{{ b }}{% endif %}
      paths <- parseAndExtract "{% if a is defined and b is defined %}{{ a }}{{ b }}{% endif %}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      -- Now we extract 4 paths: 2 for the condition checks, 2 for the body
      assertEqual "should have 4 paths (2 condition + 2 body)" 4 (length userPaths)
      -- Filter for non-existence-check paths (body only)
      let bodyPaths = filter (not . apIsExistenceCheck) userPaths
      assertEqual "should have 2 body paths" 2 (length bodyPaths)
      -- Both a and b in the body should be narrowed
      let aPaths = filter (\p -> apRoot p == "a") bodyPaths
      let bPaths = filter (\p -> apRoot p == "b") bodyPaths
      assertBool "a should be narrowed" (all (not . Set.null . apNarrowed) aPaths)
      assertBool "b should be narrowed" (all (not . Set.null . apNarrowed) bPaths)

  , testCase "or narrows neither" $ do
      -- {% if a is defined or b is defined %}{{ a }}{% endif %}
      -- a in body should NOT be narrowed (could be undefined)
      paths <- parseAndExtract "{% if a is defined or b is defined %}{{ a }}{% endif %}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      -- Now we extract 3 paths: 2 for condition, 1 for body
      assertEqual "should have 3 paths (2 condition + 1 body)" 3 (length userPaths)
      -- Filter for body access
      let bodyPaths = filter (not . apIsExistenceCheck) userPaths
      assertEqual "should have 1 body path" 1 (length bodyPaths)
      -- The body 'a' should not be narrowed (or is conservative)
      let narrowed = apNarrowed (head bodyPaths)
      assertBool "a should not be narrowed in or condition" (Set.null narrowed)

  , testCase "nested if accumulates narrowing" $ do
      -- {% if x is defined %}{% if y is defined %}{{ x }}{{ y }}{% endif %}{% endif %}
      paths <- parseAndExtract "{% if x is defined %}{% if y is defined %}{{ x }}{{ y }}{% endif %}{% endif %}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      -- Now we extract 4 paths: 2 for conditions, 2 for body
      assertEqual "should have 4 paths (2 condition + 2 body)" 4 (length userPaths)
      -- Filter for body accesses
      let bodyPaths = filter (not . apIsExistenceCheck) userPaths
      assertEqual "should have 2 body paths" 2 (length bodyPaths)
      -- x and y in innermost block should both be narrowed
      let xPaths = filter (\p -> apRoot p == "x") bodyPaths
      let yPaths = filter (\p -> apRoot p == "y") bodyPaths
      assertBool "x should be narrowed in nested block" (all (not . Set.null . apNarrowed) xPaths)
      assertBool "y should be narrowed in nested block" (all (not . Set.null . apNarrowed) yPaths)

  , testCase "ternary expression narrows" $ do
      -- {{ x.field is defined ? x.field : "default" }}
      paths <- parseAndExtract "{{ x.field is defined ? x.field : \"default\" }}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      -- Now we extract 2 paths: 1 for condition, 1 for true branch
      assertEqual "should have 2 paths (condition + true branch)" 2 (length userPaths)
      -- Filter for true branch access
      let bodyPaths = filter (not . apIsExistenceCheck) userPaths
      assertEqual "should have 1 body path" 1 (length bodyPaths)
      -- The true branch x.field should be narrowed
      let narrowed = apNarrowed (head bodyPaths)
      assertBool "x.field in ternary true branch should be narrowed" (not $ Set.null narrowed)

  , testCase "not inverts narrowing" $ do
      -- {% if not (x is undefined) %}{{ x }}{% endif %}
      -- same as x is defined
      paths <- parseAndExtract "{% if not (x is undefined) %}{{ x }}{% endif %}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      -- Filter for body access
      let bodyPaths = filter (not . apIsExistenceCheck) userPaths
      assertBool "should have at least one body path" (not $ null bodyPaths)
      -- At least one x should be narrowed (the one in the body)
      let hasNarrowed = any (not . Set.null . apNarrowed) bodyPaths
      assertBool "x should be narrowed (not undefined = defined)" hasNarrowed

  , testCase "prefix narrowing: parent guard covers child" $ do
      -- {% if user.profile is defined %}{{ user.profile.name }}{% endif %}
      -- user.profile.name should be narrowed because user.profile is guarded
      paths <- parseAndExtract "{% if user.profile is defined %}{{ user.profile.name }}{% endif %}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      -- Now we extract 2 paths: 1 for condition, 1 for body
      assertEqual "should have 2 paths (condition + body)" 2 (length userPaths)
      -- Filter for body access
      let bodyPaths = filter (not . apIsExistenceCheck) userPaths
      assertEqual "should have 1 body path" 1 (length bodyPaths)
      let path = head bodyPaths
      -- The path should be narrowed (prefix-based)
      assertBool "user.profile.name should be narrowed via prefix" (isNarrowedBy path)

  , testCase "prefix narrowing: exact match still works" $ do
      -- {% if x.y is defined %}{{ x.y }}{% endif %}
      -- Exact match case
      paths <- parseAndExtract "{% if x.y is defined %}{{ x.y }}{% endif %}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      -- Now we extract 2 paths: 1 for condition, 1 for body
      assertEqual "should have 2 paths (condition + body)" 2 (length userPaths)
      let bodyPaths = filter (not . apIsExistenceCheck) userPaths
      assertEqual "should have 1 body path" 1 (length bodyPaths)
      assertBool "x.y should be narrowed (exact match)" (isNarrowedBy $ head bodyPaths)

  , testCase "prefix narrowing: sibling not narrowed" $ do
      -- {% if user.profile is defined %}{{ user.other }}{% endif %}
      -- user.other should NOT be narrowed (different path)
      paths <- parseAndExtract "{% if user.profile is defined %}{{ user.other }}{% endif %}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      -- Now we extract 2 paths: 1 for condition (user.profile), 1 for body (user.other)
      assertEqual "should have 2 paths (condition + body)" 2 (length userPaths)
      -- Filter for body access
      let bodyPaths = filter (not . apIsExistenceCheck) userPaths
      assertEqual "should have 1 body path" 1 (length bodyPaths)
      -- user.other is not narrowed because user.profile doesn't prefix it
      assertBool "user.other should NOT be narrowed" (not $ isNarrowedBy $ head bodyPaths)

  , testCase "prefix narrowing: root only narrows all children" $ do
      -- {% if user is defined %}{{ user.profile.name }}{% endif %}
      -- If the root is guarded, all accesses through it are safe
      paths <- parseAndExtract "{% if user is defined %}{{ user.profile.name }}{% endif %}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      -- Now we extract 2 paths: 1 for condition, 1 for body
      assertEqual "should have 2 paths (condition + body)" 2 (length userPaths)
      -- Filter for body access
      let bodyPaths = filter (not . apIsExistenceCheck) userPaths
      assertEqual "should have 1 body path" 1 (length bodyPaths)
      assertBool "user.profile.name should be narrowed via root" (isNarrowedBy $ head bodyPaths)

  -- Tests for is defined prefix-only validation
  , testCase "is defined: validates prefix, allows missing final segment" $ do
      -- {% if user.profile.missing is defined %} should validate user and user.profile
      -- but allow user.profile.missing to not exist (that's what we're checking)
      let (schema, registry) = nestedRecordSchema
      paths <- parseAndExtract "{% if nrUser.missing is defined %}yes{% endif %}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      -- The existence check path should be marked
      let existencePaths = filter apIsExistenceCheck userPaths
      assertEqual "should have 1 existence check path" 1 (length existencePaths)
      -- Validation should succeed - nrUser exists, so prefix is valid
      let errors = validatePaths registry schema existencePaths
      assertEqual "should have no errors (prefix valid, final segment can be missing)" [] errors

  , testCase "is defined: rejects invalid prefix" $ do
      -- {% if invalid.field is defined %} should fail because "invalid" doesn't exist
      let (schema, registry) = simpleRecordSchema
      paths <- parseAndExtract "{% if invalid.field is defined %}yes{% endif %}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      let existencePaths = filter apIsExistenceCheck userPaths
      assertEqual "should have 1 existence check path" 1 (length existencePaths)
      -- Validation should fail - "invalid" is not in schema
      let errors = validatePaths registry schema existencePaths
      assertEqual "should have 1 error (invalid root)" 1 (length errors)

  , testCase "is defined: root-only check always succeeds if root exists" $ do
      -- {% if srName is defined %} should validate just that srName exists
      let (schema, registry) = simpleRecordSchema
      paths <- parseAndExtract "{% if srName is defined %}yes{% endif %}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      let existencePaths = filter apIsExistenceCheck userPaths
      assertEqual "should have 1 existence check path" 1 (length existencePaths)
      let errors = validatePaths registry schema existencePaths
      assertEqual "should have no errors" [] errors

  , testCase "is defined: root-only check with missing root is allowed" $ do
      -- {% if missing is defined %} - checking if something exists at root level
      -- For existence checks with no segments, we allow the root to be missing
      let (schema, registry) = simpleRecordSchema
      paths <- parseAndExtract "{% if missing is defined %}yes{% endif %}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      let existencePaths = filter apIsExistenceCheck userPaths
      assertEqual "should have 1 existence check path" 1 (length existencePaths)
      -- This should succeed - we're just checking if "missing" exists
      let errors = validatePaths registry schema existencePaths
      assertEqual "should have no errors (checking root existence)" [] errors

  , testCase "is defined: deep prefix with valid path" $ do
      -- {% if nrUser.srName.something is defined %} validates nrUser and nrUser.srName
      let (schema, registry) = nestedRecordSchema
      paths <- parseAndExtract "{% if nrUser.srName.something is defined %}yes{% endif %}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      let existencePaths = filter apIsExistenceCheck userPaths
      assertEqual "should have 1 existence check path" 1 (length existencePaths)
      -- Prefix (nrUser.srName) is valid, so no error
      -- (Even though srName is a scalar and can't have .something, we only validate prefix)
      let errors = validatePaths registry schema existencePaths
      assertEqual "should have no errors (prefix valid)" [] errors
  ]

--------------------------------------------------------------------------------
-- End-to-End Tests (template + type validation)
--------------------------------------------------------------------------------

endToEndTests :: TestTree
endToEndTests = testGroup "End-to-End Validation"
  [ testCase "valid template against SimpleRecord" $ do
      let (schema, registry) = simpleRecordSchema
      paths <- parseAndExtract "{{ srName }} is {{ srAge }} years old"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      let errors = validatePaths registry schema userPaths
      assertEqual "should have no errors" [] errors

  , testCase "invalid field on SimpleRecord" $ do
      let (schema, registry) = simpleRecordSchema
      paths <- parseAndExtract "{{ srName }} {{ invalidField }}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      let errors = validatePaths registry schema userPaths
      assertEqual "should have 1 error" 1 (length errors)
      case head errors of
        FieldNotFound _ field -> assertEqual "should be invalidField" "invalidField" field
        other -> assertFailure $ "wrong error type: " ++ show other

  , testCase "nested access on NestedRecord" $ do
      let (schema, registry) = nestedRecordSchema
      paths <- parseAndExtract "{{ nrUser.srName }} active: {{ nrActive }}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      let errors = validatePaths registry schema userPaths
      assertEqual "should have no errors" [] errors

  , testCase "sum type shared field access" $ do
      let (schema, registry) = animalSchema
      paths <- parseAndExtract "{{ animalName }}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      let errors = validatePaths registry schema userPaths
      assertEqual "should have no errors (field in all constructors)" [] errors

  , testCase "sum type non-shared field rejected" $ do
      let (schema, registry) = animalSchema
      paths <- parseAndExtract "{{ animalBreed }}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      let errors = validatePaths registry schema userPaths
      assertEqual "should have 1 error" 1 (length errors)

  , testCase "recursive type deep access" $ do
      let (schema, registry) = treeSchema
      -- Use bracket notation for list indexing
      paths <- parseAndExtract "{{ treeChildren[0].treeChildren[0].treeValue }}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      let errors = validatePaths registry schema userPaths
      assertEqual "should have no errors" [] errors

  , testCase "list index access" $ do
      let (schema, registry) = withListSchema
      -- Use bracket notation for list indexing
      paths <- parseAndExtract "{{ wlItems[0] }} count: {{ wlCount }}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      let errors = validatePaths registry schema userPaths
      assertEqual "should have no errors" [] errors

  , testCase "for loop with type checking" $ do
      let (schema, registry) = withListSchema
      paths <- parseAndExtract "{% for item in wlItems %}{{ item }}{% endfor %}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      -- Only wlItems should be extracted (item is bound by for loop)
      assertEqual "should have 1 user path" 1 (length userPaths)
      let errors = validatePaths registry schema userPaths
      assertEqual "should have no errors" [] errors

  , testCase "set binding with type checking" $ do
      let (schema, registry) = simpleRecordSchema
      paths <- parseAndExtract "{% set fullName = srName %}{{ fullName }}"
      let userPaths = filter (not . isBuiltin . apRoot) paths
      -- Only srName should be extracted (fullName is bound by set)
      assertEqual "should have 1 user path" 1 (length userPaths)
      assertEqual "should be srName" "srName" (apRoot $ head userPaths)
      let errors = validatePaths registry schema userPaths
      assertEqual "should have no errors" [] errors
  ]

--------------------------------------------------------------------------------
-- Include Validation Tests
--------------------------------------------------------------------------------

includeValidationTests :: TestTree
includeValidationTests = testGroup "Include Validation"
  [ testCase "variables in included templates are extracted" $ do
      -- Main template includes a partial that uses nrUser.srName
      let mainSrc = "<h1>User</h1>{% include 'partials/user-info.html' %}<p>{{ nrActive }}</p>"
      let partialSrc = "<span>{{ nrUser.srName }}</span>"
      paths <- parseAndExtractWithIncludes
        "main.html"
        mainSrc
        [("./partials/user-info.html", partialSrc)]
      let userPaths = filter (not . isBuiltin . apRoot) paths
      -- Should extract paths from both main and included template
      let roots = Set.fromList $ map apRoot userPaths
      assertBool "should have nrActive from main" (Set.member "nrActive" roots)
      assertBool "should have nrUser from include" (Set.member "nrUser" roots)

  , testCase "included template variables validated against schema" $ do
      -- Test that validation catches errors in included templates
      let (schema, registry) = nestedRecordSchema
      let mainSrc = "{{ nrActive }}{% include 'partial.html' %}"
      let partialSrc = "{{ nrUser.srName }}{{ invalidField }}"  -- invalidField doesn't exist
      paths <- parseAndExtractWithIncludes
        "main.html"
        mainSrc
        [("./partial.html", partialSrc)]
      let userPaths = filter (not . isBuiltin . apRoot) paths
      let errors = validatePaths registry schema userPaths
      -- Should have 1 error for invalidField
      assertEqual "should have 1 error" 1 (length errors)
      case head errors of
        FieldNotFound _ field -> assertEqual "should be invalidField" "invalidField" field
        other -> assertFailure $ "wrong error type: " ++ show other

  , testCase "nested includes extract variables from all levels" $ do
      -- main includes header, header includes nav
      let mainSrc = "{{ nrActive }}{% include 'header.html' %}"
      let headerSrc = "Header{% include '../nav.html' %}"
      let navSrc = "{{ nrUser.srName }}"
      paths <- parseAndExtractWithIncludes
        "templates/main.html"
        mainSrc
        [ ("templates/header.html", headerSrc)
        , ("templates/../nav.html", navSrc)
        ]
      let userPaths = filter (not . isBuiltin . apRoot) paths
      let roots = Set.fromList $ map apRoot userPaths
      -- Should have paths from main and deeply nested include
      assertBool "should have nrActive from main" (Set.member "nrActive" roots)
      assertBool "should have nrUser from nav (2 levels deep)" (Set.member "nrUser" roots)

  , testCase "valid template with includes passes validation" $ do
      let (schema, registry) = nestedRecordSchema
      let mainSrc = "<h1>{{ nrActive }}</h1>{% include 'user.html' %}"
      let userSrc = "<span>{{ nrUser.srName }}</span><span>{{ nrUser.srAge }}</span>"
      paths <- parseAndExtractWithIncludes
        "main.html"
        mainSrc
        [("./user.html", userSrc)]
      let userPaths = filter (not . isBuiltin . apRoot) paths
      let errors = validatePaths registry schema userPaths
      assertEqual "should have no errors" [] errors
  ]

--------------------------------------------------------------------------------
-- Generic ToGVal Tests
--------------------------------------------------------------------------------

-- ToGVal instances for test types using genericToGVal
instance ToGVal m TestStatus where
  toGVal = genericToGVal

instance ToGVal m TestEvent where
  toGVal = genericToGVal

-- Single-constructor record types (for testing flat field access)
instance ToGVal m SimpleRecord where
  toGVal = genericToGVal

instance ToGVal m NestedRecord where
  toGVal = genericToGVal

genericToGValTests :: TestTree
genericToGValTests = testGroup "Generic ToGVal"
  [ testCase "single-field positional constructor" $ do
      -- TestBlocked Text should unwrap the inner value
      let gval = toGVal (TestBlocked "no funds") :: GVal IO
      assertEqual "asText is constructor name" "TestBlocked" (asText gval)
      assertBool "should be truthy" (asBoolean gval)
      -- Lookup the constructor field
      case asLookup gval of
        Nothing -> assertFailure "should have asLookup"
        Just lookupFn -> do
          case lookupFn "TestBlocked" of
            Just inner -> assertEqual "inner value" "no funds" (asText inner)
            Nothing -> assertFailure "TestBlocked field should be defined"
          case lookupFn "TestPursuing" of
            Just _ -> assertFailure "TestPursuing should NOT be defined"
            Nothing -> return ()  -- Expected

  , testCase "single-field Int constructor" $ do
      let gval = toGVal (TestPursuing 42) :: GVal IO
      assertEqual "asText is constructor name" "TestPursuing" (asText gval)
      assertBool "should be truthy" (asBoolean gval)
      case asLookup gval >>= ($ "TestPursuing") of
        Just inner -> assertEqual "inner value" "42" (asText inner)
        Nothing -> assertFailure "TestPursuing field should be defined"

  , testCase "nullary constructor" $ do
      let gval = toGVal TestAchieved :: GVal IO
      assertEqual "asText is constructor name" "TestAchieved" (asText gval)
      assertBool "should be truthy" (asBoolean gval)
      -- Nullary constructor should still have the field defined
      case asLookup gval >>= ($ "TestAchieved") of
        Just inner -> assertBool "inner should be truthy" (asBoolean inner)
        Nothing -> assertFailure "TestAchieved field should be defined"

  , testCase "record constructor with named fields" $ do
      let gval = toGVal (TestAttack { teAttacker = "hero", teTarget = "villain" }) :: GVal IO
      assertEqual "asText is constructor name" "TestAttack" (asText gval)
      -- Access the nested record
      case asLookup gval >>= ($ "TestAttack") of
        Just inner -> do
          case asLookup inner >>= ($ "teAttacker") of
            Just attacker -> assertEqual "attacker" "hero" (asText attacker)
            Nothing -> assertFailure "teAttacker should be defined"
          case asLookup inner >>= ($ "teTarget") of
            Just target -> assertEqual "target" "villain" (asText target)
            Nothing -> assertFailure "teTarget should be defined"
        Nothing -> assertFailure "TestAttack field should be defined"

  , testCase "different constructor not defined" $ do
      let gval = toGVal (TestHeal { teHealer = "cleric", teAmount = 50 }) :: GVal IO
      assertEqual "asText is constructor name" "TestHeal" (asText gval)
      -- TestAttack should NOT be defined
      case asLookup gval >>= ($ "TestAttack") of
        Just _ -> assertFailure "TestAttack should NOT be defined for Heal variant"
        Nothing -> return ()  -- Expected

  , testCase "nullary record constructor" $ do
      let gval = toGVal TestWait :: GVal IO
      assertEqual "asText is constructor name" "TestWait" (asText gval)
      assertBool "should be truthy" (asBoolean gval)

  -- Single-constructor record tests (flat field access)
  , testCase "single-constructor record has flat field access" $ do
      let gval = toGVal (SimpleRecord { srName = "Alice", srAge = 30 }) :: GVal IO
      assertBool "should be truthy" (asBoolean gval)
      assertBool "should not be null" (not $ isNull gval)
      -- Fields should be directly accessible (NOT wrapped in constructor name)
      case asLookup gval of
        Nothing -> assertFailure "should have asLookup"
        Just lookupFn -> do
          case lookupFn "srName" of
            Just name -> assertEqual "name" "Alice" (asText name)
            Nothing -> assertFailure "srName should be directly accessible"
          case lookupFn "srAge" of
            Just age -> assertEqual "age" "30" (asText age)
            Nothing -> assertFailure "srAge should be directly accessible"
          -- Constructor name should NOT be a field
          case lookupFn "SimpleRecord" of
            Just _ -> assertFailure "SimpleRecord should NOT be a field (flat access expected)"
            Nothing -> return ()  -- Expected: no constructor wrapping

  , testCase "nested single-constructor records have flat access" $ do
      let inner = SimpleRecord { srName = "Bob", srAge = 25 }
          gval = toGVal (NestedRecord { nrUser = inner, nrActive = True }) :: GVal IO
      case asLookup gval of
        Nothing -> assertFailure "should have asLookup"
        Just lookupFn -> do
          -- Top-level fields should be directly accessible
          case lookupFn "nrActive" of
            Just active -> assertBool "nrActive should be true" (asBoolean active)
            Nothing -> assertFailure "nrActive should be directly accessible"
          -- Nested record field should be accessible
          case lookupFn "nrUser" of
            Just userGval -> do
              -- The nested record should also have flat access
              case asLookup userGval >>= ($ "srName") of
                Just name -> assertEqual "nested name" "Bob" (asText name)
                Nothing -> assertFailure "nrUser.srName should be accessible"
            Nothing -> assertFailure "nrUser should be directly accessible"
  ]

--------------------------------------------------------------------------------
-- QuasiQuoter Tests
--------------------------------------------------------------------------------

quasiQuoterTests :: TestTree
quasiQuoterTests = testGroup "QuasiQuoter"
  [ testCase "jinja basic template" $ do
      let name = "world" :: Text
      let result :: Text = [jinja|Hello, {{ name }}!|]
      assertEqual "renders correctly" "Hello, world!" result

  , testCase "jinja as function" $ do
      let greeting n = [jinja|Hello, {{ n }}!|] :: Text
      assertEqual "renders correctly" "Hello, Alice!" (greeting ("Alice" :: Text))

  , testCase "jinja multiple variables" $ do
      let name = "Bob" :: Text
          age = 30 :: Int
      let result :: Text = [jinja|{{ name }} is {{ age }} years old|]
      assertEqual "renders correctly" "Bob is 30 years old" result

  , testCase "jinja with filters" $ do
      let items = ["a", "b", "c"] :: [Text]
      let result :: Text = [jinja|Count: {{ items | length }}|]
      assertEqual "renders with filter" "Count: 3" result

  , testCase "jinja with for loop" $ do
      let items = ["x", "y", "z"] :: [Text]
      let result :: Text = [jinja|{% for item in items %}{{ item }}{% endfor %}|]
      assertEqual "renders loop" "xyz" result

  , testCase "jinja with conditional" $ do
      let active = True
      let result :: Text = [jinja|{% if active %}yes{% else %}no{% endif %}|]
      assertEqual "renders conditional" "yes" result

  , testCase "jinja literal only" $ do
      let result :: Text = [jinja|Hello, World!|]
      assertEqual "renders literal" "Hello, World!" result

  , testCase "jinja nested if/for/if with loop.last" $ do
      let pool = [1, 2, 3] :: [Int]
      let result :: Text = [jinja|{% if pool %}[{% for die in pool %}{{ die }}{% if not loop.last %}, {% endif %}{% endfor %}]{% else %}empty{% endif %}|]
      assertEqual "renders nested" "[1, 2, 3]" result

  -- Parser edge case tests via QuasiQuoter
  , testCase "jinja deeply nested (4 levels)" $ do
      let xs = [1] :: [Int]
      let ys = [2] :: [Int]
      let result :: Text = [jinja|{% if xs %}{% for x in xs %}{% if x == 1 %}{% for y in ys %}{{ x }}{{ y }}{% endfor %}{% endif %}{% endfor %}{% endif %}|]
      assertEqual "renders deeply nested" "12" result

  , testCase "jinja for with loop.first and loop.last" $ do
      let items = [1, 2, 3] :: [Int]
      let result :: Text = [jinja|{% for i in items %}{% if loop.first %}[{% endif %}{{ i }}{% if loop.last %}]{% else %},{% endif %}{% endfor %}|]
      assertEqual "renders with loop vars" "[1,2,3]" result

  , testCase "jinja multiple for loops in sequence" $ do
      let as = [1, 2] :: [Int]
      let bs = [3, 4] :: [Int]
      let result :: Text = [jinja|{% for a in as %}{{ a }}{% endfor %}-{% for b in bs %}{{ b }}{% endfor %}|]
      assertEqual "renders sequential fors" "12-34" result

  , testCase "jinja nested for loops" $ do
      let outer = [1, 2] :: [Int]
      let inner = ["a", "b"] :: [Text]
      let result :: Text = [jinja|{% for i in outer %}{% for j in inner %}{{ i }}{{ j }}{% endfor %}{% endfor %}|]
      assertEqual "renders nested fors" "1a1b2a2b" result

  , testCase "jinja for inside elif" $ do
      let items = [1, 2] :: [Int]
      let result :: Text = [jinja|{% if false %}no{% elif items %}{% for i in items %}{{ i }}{% endfor %}{% else %}no{% endif %}|]
      assertEqual "renders for in elif" "12" result

  , testCase "jinja for inside else" $ do
      let items = [1, 2] :: [Int]
      let result :: Text = [jinja|{% if false %}no{% else %}{% for i in items %}{{ i }}{% endfor %}{% endif %}|]
      assertEqual "renders for in else" "12" result
  ]

--------------------------------------------------------------------------------
-- Property Tests
--------------------------------------------------------------------------------

propertyTests :: TestTree
propertyTests = testGroup "Properties"
  [ testGroup "Schema Validation"
      [ testProperty "record fields are findable" prop_recordFieldsFindable
      , testProperty "scalar rejects all path segments" prop_scalarRejectsSegments
      , testProperty "list accepts any key" prop_listAcceptsAnyKey
      , testProperty "dynamic key on record fails" prop_dynamicKeyOnRecordFails
      , testProperty "validation is deterministic" prop_validationDeterministic
      ]
  , testGroup "Builtins"
      [ testProperty "builtin filtering is idempotent" prop_builtinFilterIdempotent
      , testProperty "all builtins are recognized" prop_allBuiltinsRecognized
      ]
  ]

-- | All fields listed in a RecordSchema can be accessed.
prop_recordFieldsFindable :: NonEmptyList (Text, Schema) -> Bool
prop_recordFieldsFindable (NonEmpty fields) =
  let schema = RecordSchema fields
      fieldNames = map fst fields
      paths = [mkAccessPath name [] dummyPos | name <- fieldNames]
  in all (\p -> validatePath emptyRegistry schema p == Nothing) paths

-- | ScalarSchema rejects any non-empty path segment.
prop_scalarRejectsSegments :: NonEmptyList PathSegment -> Bool
prop_scalarRejectsSegments (NonEmpty segs) =
  let schema = RecordSchema [("x", ScalarSchema)]
      path = mkAccessPath "x" segs dummyPos
  in case validatePath emptyRegistry schema path of
       Just (AccessOnScalar _) -> True
       _ -> False

-- | ListSchema accepts any static key (for index access).
prop_listAcceptsAnyKey :: Text -> Bool
prop_listAcceptsAnyKey key =
  let schema = RecordSchema [("items", ListSchema ScalarSchema)]
      path = mkAccessPath "items" [StaticKey key] dummyPos
  in validatePath emptyRegistry schema path == Nothing

-- | DynamicKey on a RecordSchema always fails.
prop_dynamicKeyOnRecordFails :: NonEmptyList (Text, Schema) -> Bool
prop_dynamicKeyOnRecordFails (NonEmpty fields) =
  let schema = RecordSchema [("rec", RecordSchema fields)]
      path = mkAccessPath "rec" [DynamicKey] dummyPos
  in case validatePath emptyRegistry schema path of
       Just (DynamicAccessNotAllowed _) -> True
       _ -> False

-- | Validation of the same path against same schema is deterministic.
prop_validationDeterministic :: Schema -> AccessPath -> Bool
prop_validationDeterministic schema path =
  validatePath emptyRegistry schema path == validatePath emptyRegistry schema path

-- | Filtering builtins twice gives same result as once.
prop_builtinFilterIdempotent :: [Text] -> Bool
prop_builtinFilterIdempotent names =
  let filtered = filter (not . isBuiltin) names
      filteredAgain = filter (not . isBuiltin) filtered
  in filtered == filteredAgain

-- | All names in builtinNames are recognized by isBuiltin.
prop_allBuiltinsRecognized :: Property
prop_allBuiltinsRecognized =
  forAll (elements $ Set.toList builtinNames) isBuiltin

--------------------------------------------------------------------------------
-- Dependency Tracking Tests
--------------------------------------------------------------------------------

instance ToGVal m EmptyContext where
  toGVal _ = toGVal ()

-- Use TH to load templates at compile time
simpleTemplate :: TypedTemplate EmptyContext SourcePos
simpleTemplate = $(typedTemplateFile ''EmptyContext "test/templates/dep-simple.html")

mainTemplate :: TypedTemplate EmptyContext SourcePos
mainTemplate = $(typedTemplateFile ''EmptyContext "test/templates/dep-main.html")

dependencyTests :: TestTree
dependencyTests = testGroup "Dependency Tracking"
  [ testCase "simple template has one dependency (itself)" $ do
      let root = templateDependencyTree simpleTemplate
      let allDeps = flattenDeps root
      assertEqual "should have 1 dependency" 1 (length allDeps)
      let relPaths = relativePaths simpleTemplate
      assertBool "relative path ends with dep-simple.html"
        (any ("dep-simple.html" `isSuffixOf`) relPaths)

  , testCase "template with includes has all dependencies" $ do
      let allDeps = flattenDeps $ templateDependencyTree mainTemplate
      -- main + partial + nested = 3 files
      assertEqual "should have 3 dependencies" 3 (length allDeps)

  , testCase "absolute paths are absolute" $ do
      let absPaths = absolutePaths mainTemplate
      assertBool "all absolute paths start with /"
        (all (\p -> head p == '/') absPaths)

  , testCase "relative paths preserve include paths" $ do
      let relPaths = relativePaths mainTemplate
      -- Main template path
      assertBool "has main template" (any ("dep-main.html" `isSuffixOf`) relPaths)
      -- Partial (included from main as 'dep-partial.html')
      assertBool "has partial" (any ("dep-partial.html" `isSuffixOf`) relPaths)
      -- Nested (included from partial as 'dep-nested.html')
      assertBool "has nested" (any ("dep-nested.html" `isSuffixOf`) relPaths)

  , testCase "TemplateDependency has both path forms" $ do
      let allDeps = flattenDeps $ templateDependencyTree mainTemplate
      forM_ allDeps $ \dep -> do
        assertBool "absolute path is non-empty" (not $ null $ depAbsolutePath dep)
        assertBool "relative path is non-empty" (not $ null $ depRelativePath dep)

  -- Tree structure tests
  , testCase "root template is at tree root" $ do
      let root = templateDependencyTree mainTemplate
      assertBool "root should be main template"
        ("dep-main.html" `isSuffixOf` depRelativePath root)
      assertEqual "root has no relation" Nothing (depRelation root)
      assertEqual "root has no location" Nothing (depIncludeLocation root)

  , testCase "included templates are children of root" $ do
      let root = templateDependencyTree mainTemplate
      let children = depChildren root
      -- Main directly includes partial (1 child at root level)
      assertEqual "root should have 1 direct child" 1 (length children)
      let partialDep = head children
      assertEqual "child should be DepIncluded" (Just DepIncluded) (depRelation partialDep)
      assertBool "child should have location" (isJust $ depIncludeLocation partialDep)

  , testCase "nested includes form correct tree structure (A->B->C)" $ do
      let root = templateDependencyTree mainTemplate
      -- Root is main
      assertBool "root is main" ("dep-main" `isInfixOf` depRelativePath root)

      -- Main has one child: partial
      assertEqual "main has 1 child" 1 (length $ depChildren root)
      let partialDep = head $ depChildren root
      assertBool "child is partial" ("dep-partial" `isInfixOf` depRelativePath partialDep)

      -- Partial has one child: nested
      assertEqual "partial has 1 child" 1 (length $ depChildren partialDep)
      let nestedDep = head $ depChildren partialDep
      assertBool "grandchild is nested" ("dep-nested" `isInfixOf` depRelativePath nestedDep)

      -- Nested has no children
      assertEqual "nested has no children" 0 (length $ depChildren nestedDep)

  , testCase "depIncludeLocation has correct file and line info" $ do
      let root = templateDependencyTree mainTemplate
      let partialDep = head $ depChildren root
      case depIncludeLocation partialDep of
        Nothing -> assertFailure "should have location"
        Just loc -> do
          assertBool "location file should be main"
            ("dep-main" `isInfixOf` depLocFile loc)
          -- The include directive spans lines 2-3 in dep-main.html
          -- Parsec captures the position after the closing %}
          assertBool "location line should be near the include (2-3)"
            (depLocLine loc >= 2 && depLocLine loc <= 3)

  -- Context info tests
  , testCase "templateContextInfo has type name" $ do
      let info = templateContextInfo mainTemplate
      assertEqual "type name should be EmptyContext"
        "EmptyContext" (tciTypeName info)

  , testCase "templateContextInfo has module name" $ do
      let info = templateContextInfo mainTemplate
      assertEqual "module should be Text.Ginger.TH.TestTypes"
        (Just "Text.Ginger.TH.TestTypes") (tciModuleName info)

  , testCase "templateContextInfo has fully qualified name" $ do
      let info = templateContextInfo mainTemplate
      assertEqual "fully qualified should be Text.Ginger.TH.TestTypes.EmptyContext"
        "Text.Ginger.TH.TestTypes.EmptyContext" (tciFullyQualified info)

  -- Accessed fields tests (simple template has no fields, so use one that does)
  , testCase "templateAccessedFields is empty for template with no variable access" $ do
      let fields = templateAccessedFields simpleTemplate
      assertEqual "simple template has no accessed fields" [] fields
  ]

--------------------------------------------------------------------------------
-- Arbitrary Instances
--------------------------------------------------------------------------------

instance Arbitrary Text where
  arbitrary = Text.pack <$> listOf (elements ['a'..'z'])
  shrink t = Text.pack <$> shrink (Text.unpack t)

instance Arbitrary PathSegment where
  arbitrary = oneof
    [ StaticKey <$> arbitrary
    , pure DynamicKey
    ]
  shrink (StaticKey k) = StaticKey <$> shrink k
  shrink DynamicKey = []

instance Arbitrary Schema where
  arbitrary = sized genSchema
    where
      genSchema 0 = pure ScalarSchema
      genSchema n = oneof
        [ pure ScalarSchema
        , RecordSchema <$> resize 3 (listOf ((,) <$> fieldName <*> genSchema (n `div` 2)))
        , ListSchema <$> genSchema (n - 1)
        , SumSchema <$> resize 2 (listOf1 genConstructor)
        ]
      genConstructor = (,) <$> conName <*> resize 3 (listOf ((,) <$> fieldName <*> arbitrary))
      fieldName = Text.pack <$> listOf1 (elements ['a'..'z'])
      conName = Text.pack . ("Con" ++) <$> listOf1 (elements ['A'..'Z'])

  shrink ScalarSchema = []
  shrink (RecordSchema fields) = ScalarSchema : [RecordSchema fs | fs <- shrink fields]
  shrink (ListSchema s) = ScalarSchema : s : [ListSchema s' | s' <- shrink s]
  shrink (SumSchema cs) = ScalarSchema : [SumSchema cs' | cs' <- shrink cs, not (null cs')]

instance Arbitrary AccessPath where
  arbitrary = AccessPath
    <$> (Text.pack <$> listOf1 (elements ['a'..'z']))
    <*> resize 3 (listOf arbitrary)
    <*> pure dummyPos
    <*> pure Set.empty
    <*> pure False

  shrink (AccessPath root segs pos narrowed isCheck) =
    [AccessPath root segs' pos narrowed isCheck | segs' <- shrink segs]

