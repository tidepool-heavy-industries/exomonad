{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

-- | Tests for the StructuredOutput typeclass.
--
-- Verifies:
-- * Primitive type roundtrips
-- * Container type roundtrips (Maybe, List, Set, NonEmpty)
-- * Record type roundtrips with field prefix stripping
-- * Maybe field handling (optional vs required)
-- * Sum type roundtrips
-- * Parse error diagnostics
-- * Schema validity checking
module StructuredOutputSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (Value(..), withText)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.List.NonEmpty (NonEmpty(..))
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)

import ExoMonad.StructuredOutput
import ExoMonad.StructuredOutput.Error (ParseDiagnostic(..))
import ExoMonad.Schema (JSONSchema(..), SchemaType(..), enumSchema, schemaToValue)


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
-- This is a VALID StructuredOutput instance (it can be encoded/parsed),
-- but it will be rejected if used in llmCall (oneOf not supported).
data SumType
  = VariantA Text
  | VariantB Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (StructuredOutput)

-- | Simple enum with no data (automatic string enum).
data Priority
  = Low
  | Medium
  | High
  deriving stock (Show, Eq, Generic)
  deriving anyclass (StructuredOutput)

-- | Simple enum with manual string encoding.
data Status = Pending | Active | Completed
  deriving stock (Show, Eq, Generic)

instance StructuredOutput Status where
  structuredSchema = enumSchema ["Pending", "Active", "Completed"]
  encodeStructured = \case
    Pending -> String "Pending"
    Active -> String "Active"
    Completed -> String "Completed"
  parseStructured = \case
    String "Pending" -> Right Pending
    String "Active" -> Right Active
    String "Completed" -> Right Completed
    String other -> Left $ ParseDiagnostic [] "Pending | Active | Completed" other ("Unknown Status: " <> other)
    other -> Left $ ParseDiagnostic [] "string" (T.pack $ show other) "Expected string for Status enum"


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

  describe "NonEmpty schema" $ do
    it "sets minItems to 1" $ do
      let schema = structuredSchema @(NonEmpty Text)
      schema.schemaMinItems `shouldBe` Just 1

    it "regular list has no minItems" $ do
      let schema = structuredSchema @[Text]
      schema.schemaMinItems `shouldBe` Nothing

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

  describe "Simple enum encoding" $ do
    describe "Automatic string enum (all-nullary sum types)" $ do
      it "Priority auto-generates string enum (not tag+contents)" $ do
        let encoded = encodeStructured Low
        encoded `shouldBe` String "Low"

      it "Priority roundtrips as strings" $ do
        roundtrip Low
        roundtrip Medium
        roundtrip High

      it "Priority schema uses string enum (not oneOf)" $ do
        let schema = structuredSchema @Priority
        schema.schemaType `shouldBe` TString
        schema.schemaEnum `shouldBe` Just ["Low", "Medium", "High"]
        schema.schemaOneOf `shouldBe` Nothing

    describe "Manual string enum (same result as automatic)" $ do
      it "Status uses simple string format" $ do
        encodeStructured Pending `shouldBe` String "Pending"
        encodeStructured Active `shouldBe` String "Active"
        encodeStructured Completed `shouldBe` String "Completed"

      it "Status roundtrips as strings" $ do
        roundtrip Pending
        roundtrip Active
        roundtrip Completed

      it "Status schema uses string enum" $ do
        let schema = structuredSchema @Status
        schema.schemaType `shouldBe` TString
        schema.schemaEnum `shouldBe` Just ["Pending", "Active", "Completed"]
        schema.schemaOneOf `shouldBe` Nothing

  describe "Validation" $ do
    it "recognizes simple records as valid" $ do
      -- This test is mostly a compile-time check
      let _ = () :: ValidStructuredOutput SimpleRecord => ()
      True `shouldBe` True

    it "recognizes enums as valid" $ do
      let _ = () :: ValidStructuredOutput Priority => ()
      True `shouldBe` True

    it "containers are valid if their contents are" $ do
      let _ = () :: ValidStructuredOutput [Text] => ()
      let _ = () :: ValidStructuredOutput (Maybe Int) => ()
      True `shouldBe` True

    -- it "SumType is recognized as invalid for structured output" $ do
    --   -- THIS WOULD FAIL TO COMPILE (which is what we want to verify manually)
    --   -- let _ = () :: ValidStructuredOutput SumType => ()
    --   pendingWith "Manually verified that SumType fails compile check in llmCall"