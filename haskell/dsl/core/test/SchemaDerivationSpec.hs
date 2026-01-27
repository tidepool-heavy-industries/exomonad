{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module SchemaDerivationSpec (spec) where

import Test.Hspec
import Data.Aeson (encode, decode, Value(..), toJSON, object, (.=))
import qualified Data.ByteString.Lazy as BL
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
