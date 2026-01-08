{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for the StructuredOutput typeclass.
--
-- Verifies:
-- * Primitive type roundtrips
-- * Container type roundtrips (Maybe, List, Set, NonEmpty)
-- * Record type roundtrips with field prefix stripping
-- * Maybe field handling (optional vs required)
-- * Sum type roundtrips
-- * Parse error diagnostics
module StructuredOutputSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (Value(..))
import qualified Data.Aeson.KeyMap as KeyMap
import Data.List.NonEmpty (NonEmpty(..))
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)

import Tidepool.StructuredOutput
import Tidepool.Schema (JSONSchema(..))


-- ════════════════════════════════════════════════════════════════════════════
-- TEST FIXTURES
-- ════════════════════════════════════════════════════════════════════════════

-- | Simple record with prefix-stripped fields.
data SimpleRecord = SimpleRecord
  { srName :: Text
  , srAge :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (StructuredOutput)

-- | Record with optional (Maybe) field.
data OptionalRecord = OptionalRecord
  { orRequired :: Text
  , orOptional :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (StructuredOutput)

-- | Record with nested Maybe.
data NestedMaybe = NestedMaybe
  { nmValue :: Maybe (Maybe Text)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (StructuredOutput)

-- | Sum type with two variants.
data SumType
  = VariantA Text
  | VariantB Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (StructuredOutput)


-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Roundtrip test: encode then parse should return original value.
roundtrip :: (StructuredOutput a, Eq a, Show a) => a -> Expectation
roundtrip x = parseStructured (encodeStructured x) `shouldBe` Right x


-- ════════════════════════════════════════════════════════════════════════════
-- TESTS
-- ════════════════════════════════════════════════════════════════════════════

spec :: Spec
spec = do
  describe "Primitive roundtrips" $ do
    it "Text" $ roundtrip ("hello" :: Text)
    it "String" $ roundtrip ("world" :: String)
    it "Int" $ roundtrip (42 :: Int)
    it "Integer" $ roundtrip (123456789 :: Integer)  -- within Int bounds
    it "Double" $ roundtrip (3.14159 :: Double)
    it "Bool True" $ roundtrip True
    it "Bool False" $ roundtrip False

  describe "Maybe roundtrips" $ do
    it "Just value" $ roundtrip (Just "hello" :: Maybe Text)
    it "Nothing" $ roundtrip (Nothing :: Maybe Text)
    it "Just nested" $ roundtrip (Just (Just 42) :: Maybe (Maybe Int))
    it "Nothing outer" $ roundtrip (Nothing :: Maybe (Maybe Int))
    -- Note: Just Nothing :: Maybe (Maybe a) is information-lossy in JSON
    -- Both Nothing and Just Nothing encode to Null, so this doesn't roundtrip

  describe "List roundtrips" $ do
    it "empty list" $ roundtrip ([] :: [Text])
    it "single element" $ roundtrip (["one"] :: [Text])
    it "multiple elements" $ roundtrip (["a", "b", "c"] :: [Text])
    it "list of ints" $ roundtrip ([1, 2, 3] :: [Int])

  describe "Set roundtrips" $ do
    it "empty set" $ roundtrip (Set.empty :: Set Text)
    it "single element" $ roundtrip (Set.singleton "one" :: Set Text)
    it "multiple elements" $ roundtrip (Set.fromList ["a", "b", "c"] :: Set Text)

  describe "NonEmpty roundtrips" $ do
    it "single element" $ roundtrip ("one" :| [] :: NonEmpty Text)
    it "multiple elements" $ roundtrip ("a" :| ["b", "c"] :: NonEmpty Text)

  describe "Tuple roundtrips" $ do
    it "pair of same type" $ roundtrip (("hello", "world") :: (Text, Text))
    it "pair of different types" $ roundtrip (("hello", 42) :: (Text, Int))

  describe "Unit roundtrip" $ do
    it "unit accepts any JSON" $ do
      parseStructured @() Null `shouldBe` Right ()
      parseStructured @() (Bool True) `shouldBe` Right ()
      parseStructured @() (Number 42) `shouldBe` Right ()

  describe "Record roundtrips" $ do
    it "simple record" $
      roundtrip (SimpleRecord "alice" 30)

    it "field names are prefix-stripped" $ do
      let encoded = encodeStructured (SimpleRecord "bob" 25)
      case encoded of
        Object obj -> do
          KeyMap.member "name" obj `shouldBe` True
          KeyMap.member "age" obj `shouldBe` True
          KeyMap.member "srName" obj `shouldBe` False
        _ -> expectationFailure "Expected Object"

    it "record with optional field present" $
      roundtrip (OptionalRecord "req" (Just 42))

    it "record with optional field absent" $
      roundtrip (OptionalRecord "req" Nothing)

  describe "Maybe field schema" $ do
    it "excludes Maybe fields from required list" $ do
      let schema = structuredSchema @OptionalRecord
      -- orRequired should be required, orOptional should not
      "required" `elem` schema.schemaRequired `shouldBe` True
      "optional" `elem` schema.schemaRequired `shouldBe` False

    it "non-Maybe fields are required" $ do
      let schema = structuredSchema @SimpleRecord
      "name" `elem` schema.schemaRequired `shouldBe` True
      "age" `elem` schema.schemaRequired `shouldBe` True

  describe "Maybe field parsing" $ do
    it "missing optional field parses as Nothing" $ do
      let json = Object $ KeyMap.fromList [("required", String "hello")]
      parseStructured @OptionalRecord json `shouldBe` Right (OptionalRecord "hello" Nothing)

    it "null optional field parses as Nothing" $ do
      let json = Object $ KeyMap.fromList
            [("required", String "hello"), ("optional", Null)]
      parseStructured @OptionalRecord json `shouldBe` Right (OptionalRecord "hello" Nothing)

    it "present optional field parses as Just" $ do
      let json = Object $ KeyMap.fromList
            [("required", String "hello"), ("optional", Number 42)]
      parseStructured @OptionalRecord json `shouldBe` Right (OptionalRecord "hello" (Just 42))

  describe "Maybe field encoding" $ do
    it "omits Nothing fields by default" $ do
      let encoded = encodeStructured (OptionalRecord "hello" Nothing)
      case encoded of
        Object obj -> KeyMap.member "optional" obj `shouldBe` False
        _ -> expectationFailure "Expected Object"

    it "includes Just fields" $ do
      let encoded = encodeStructured (OptionalRecord "hello" (Just 42))
      case encoded of
        Object obj -> KeyMap.lookup "optional" obj `shouldBe` Just (Number 42)
        _ -> expectationFailure "Expected Object"

  describe "Sum type roundtrips" $ do
    it "first variant" $ roundtrip (VariantA "hello")
    it "second variant" $ roundtrip (VariantB 42)

  describe "Sum type encoding" $ do
    it "uses tagged object format" $ do
      let encoded = encodeStructured (VariantA "test")
      case encoded of
        Object obj -> do
          KeyMap.member "tag" obj `shouldBe` True
          KeyMap.member "contents" obj `shouldBe` True
        _ -> expectationFailure "Expected Object"

  describe "Parse errors" $ do
    it "reports missing required field" $ do
      let json = Object mempty
      case parseStructured @SimpleRecord json of
        Left diag -> T.unpack (formatDiagnostic diag) `shouldContain` "name"
        Right _ -> expectationFailure "Should have failed"

    it "reports type mismatch" $ do
      let json = Object $ KeyMap.fromList [("name", Number 42), ("age", Number 1)]
      case parseStructured @SimpleRecord json of
        Left diag -> T.unpack (formatDiagnostic diag) `shouldContain` "string"
        Right _ -> expectationFailure "Should have failed"

    it "NonEmpty rejects empty array" $ do
      let json = Array mempty
      case parseStructured @(NonEmpty Text) json of
        Left diag -> T.unpack (formatDiagnostic diag) `shouldContain` "non-empty"
        Right _ -> expectationFailure "Should have failed"

  describe "Value passthrough" $ do
    it "Value roundtrips unchanged" $ do
      let obj = Object $ KeyMap.fromList [("key", String "value")]
      roundtrip obj
      roundtrip (Array mempty)
      roundtrip (String "test")
      roundtrip (Number 42)
      roundtrip (Bool True)
      roundtrip Null
