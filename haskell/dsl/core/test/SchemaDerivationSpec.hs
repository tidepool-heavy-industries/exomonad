{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module SchemaDerivationSpec (spec) where

import Test.Hspec
import Data.Aeson (encode, decode, Value(..), toJSON, object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Text (Text)
import GHC.Generics (Generic)

import ExoMonad.Schema

-- Define test type
data TestArgs = TestArgs
  { taName :: Text
  , taCount :: Maybe Int
  , taFlag :: Bool
  } deriving (Show, Eq, Generic)

-- Generate instances
$(deriveMCPTypeWith defaultMCPOptions { fieldPrefix = "ta" } ''TestArgs
  [ 'taName ?? "The name"
  , 'taCount ~> "item_count" ? "Number of items"
  , 'taFlag ~> "is_flagged" ? "A flag"
  ])

-- Nested type for testing arrays and references
data NestedArgs = NestedArgs
  { naItems :: [Text]
  , naChild :: Maybe TestArgs
  } deriving (Show, Eq, Generic)

$(deriveMCPTypeWith defaultMCPOptions { fieldPrefix = "na" } ''NestedArgs
  [ 'naItems ?? "List of items"
  , 'naChild ?? "Optional child"
  ])

-- Helper to extract a key from a JSON object
getKey :: Text -> Value -> Maybe Value
getKey k (Object o) = KM.lookup (K.fromText k) o
getKey _ _ = Nothing

-- Helper to extract multiple keys nested
getPath :: [Text] -> Value -> Maybe Value
getPath [] v = Just v
getPath (k:ks) v = getKey k v >>= getPath ks

spec :: Spec
spec = do
  describe "deriveMCPType" $ do
    it "generates correct ToJSON" $ do
      let args = TestArgs "test" (Just 5) True
      toJSON args `shouldBe` object
        [ "name" .= ("test" :: Text)
        , "item_count" .= (5 :: Int)
        , "is_flagged" .= True
        ]

      let argsNone = TestArgs "test" Nothing False
      toJSON argsNone `shouldBe` object
        [ "name" .= ("test" :: Text)
        , "item_count" .= Null
        , "is_flagged" .= False
        ]

    it "generates correct FromJSON" $ do
      let json = "{\"name\":\"test\",\"item_count\":5,\"is_flagged\":true}"
      decode json `shouldBe` Just (TestArgs "test" (Just 5) True)

      let jsonMissing = "{\"name\":\"test\",\"is_flagged\":false}"
      decode jsonMissing `shouldBe` Just (TestArgs "test" Nothing False)

    it "generates correct HasJSONSchema" $ do
      let schema = schemaToValue (jsonSchema @TestArgs)
      BL.length (encode schema) `shouldSatisfy` (> 0)

  describe "Schema content verification" $ do
    it "generates correct top-level structure" $ do
      let schema = schemaToValue (jsonSchema @TestArgs)

      -- Verify top-level type is "object"
      getKey "type" schema `shouldBe` Just (String "object")

      -- Verify additionalProperties is false
      getKey "additionalProperties" schema `shouldBe` Just (Bool False)

      -- Verify properties object exists
      getKey "properties" schema `shouldSatisfy` \case
        Just (Object _) -> True
        _ -> False

    it "generates correct field types" $ do
      let schema = schemaToValue (jsonSchema @TestArgs)

      -- name should be string
      getPath ["properties", "name", "type"] schema `shouldBe` Just (String "string")

      -- item_count should be integer
      getPath ["properties", "item_count", "type"] schema `shouldBe` Just (String "integer")

      -- is_flagged should be boolean
      getPath ["properties", "is_flagged", "type"] schema `shouldBe` Just (String "boolean")

    it "preserves field descriptions" $ do
      let schema = schemaToValue (jsonSchema @TestArgs)

      -- Verify descriptions are set
      getPath ["properties", "name", "description"] schema `shouldBe` Just (String "The name")
      getPath ["properties", "item_count", "description"] schema `shouldBe` Just (String "Number of items")
      getPath ["properties", "is_flagged", "description"] schema `shouldBe` Just (String "A flag")

    it "correctly identifies required fields (non-Maybe fields)" $ do
      let schema = schemaToValue (jsonSchema @TestArgs)

      -- Required should include "name" and "is_flagged" but NOT "item_count"
      case getKey "required" schema of
        Just (Array arr) -> do
          let required = [s | String s <- vectorToList arr]
          required `shouldContain` ["name"]
          required `shouldContain` ["is_flagged"]
          required `shouldNotContain` ["item_count"]
        _ -> expectationFailure "Expected required to be an array"

  describe "Nested type schema" $ do
    it "handles array types correctly" $ do
      let schema = schemaToValue (jsonSchema @NestedArgs)

      -- items should be array type
      getPath ["properties", "items", "type"] schema `shouldBe` Just (String "array")

      -- items array should have string items
      getPath ["properties", "items", "items", "type"] schema `shouldBe` Just (String "string")

    it "handles nested object types correctly" $ do
      let schema = schemaToValue (jsonSchema @NestedArgs)

      -- child should be object type (from TestArgs)
      getPath ["properties", "child", "type"] schema `shouldBe` Just (String "object")

      -- child should have its own properties
      getPath ["properties", "child", "properties", "name", "type"] schema `shouldBe` Just (String "string")

    it "correctly identifies required fields for nested type" $ do
      let schema = schemaToValue (jsonSchema @NestedArgs)

      -- Required should include "items" but NOT "child" (Maybe)
      case getKey "required" schema of
        Just (Array arr) -> do
          let required = [s | String s <- vectorToList arr]
          required `shouldContain` ["items"]
          required `shouldNotContain` ["child"]
        _ -> expectationFailure "Expected required to be an array"

  where
    vectorToList :: V.Vector Value -> [Value]
    vectorToList = V.toList
