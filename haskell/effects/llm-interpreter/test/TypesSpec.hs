-- | Tests for LLM interpreter types.
--
-- Verifies that tool types serialize to the correct provider-specific formats.
module TypesSpec (spec) where

import Data.Aeson (Value(..), decode, encode, object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Test.Hspec

import Tidepool.LLM.Types (AnthropicTool(..), anthropicToolToJSON)


spec :: Spec
spec = do
  describe "AnthropicTool" $ do
    describe "JSON serialization" $ do
      it "uses 'input_schema' key (not 'parameters')" $ do
        let tool = AnthropicTool
              { atName = "search"
              , atDescription = "Search the web"
              , atInputSchema = object
                  [ "type" .= ("object" :: String)
                  , "properties" .= object
                      [ "query" .= object ["type" .= ("string" :: String)]
                      ]
                  , "required" .= (["query"] :: [String])
                  ]
              }
            json = anthropicToolToJSON tool
            Object obj = json

        -- Must have 'input_schema', not 'parameters'
        KM.member "input_schema" obj `shouldBe` True
        KM.member "parameters" obj `shouldBe` False

      it "serializes name and description correctly" $ do
        let tool = AnthropicTool
              { atName = "my_tool"
              , atDescription = "Does something useful"
              , atInputSchema = object ["type" .= ("object" :: String)]
              }
            json = anthropicToolToJSON tool
            Object obj = json

        KM.lookup "name" obj `shouldBe` Just (String "my_tool")
        KM.lookup "description" obj `shouldBe` Just (String "Does something useful")

      it "round-trips through JSON encoding" $ do
        let tool = AnthropicTool
              { atName = "test"
              , atDescription = "A test tool"
              , atInputSchema = object
                  [ "type" .= ("object" :: String)
                  , "properties" .= object []
                  ]
              }
            encoded = encode tool
            decoded = decode encoded :: Maybe Value
            original = anthropicToolToJSON tool

        decoded `shouldBe` Just original

      it "does NOT wrap in OpenAI function format" $ do
        let tool = AnthropicTool
              { atName = "test"
              , atDescription = "Test"
              , atInputSchema = object ["type" .= ("object" :: String)]
              }
            json = anthropicToolToJSON tool
            Object obj = json

        -- OpenAI format has 'type: "function"' at top level
        -- Anthropic format does NOT
        KM.member "type" obj `shouldBe` False
        KM.member "function" obj `shouldBe` False

    describe "schema format comparison" $ do
      it "differs from OpenAI format in key ways" $ do
        -- This test documents the expected differences between formats
        let anthropicTool = AnthropicTool
              { atName = "search"
              , atDescription = "Search"
              , atInputSchema = object ["type" .= ("object" :: String)]
              }

            -- What OpenAI format would look like (for comparison)
            openAIFormat = object
              [ "type" .= ("function" :: String)
              , "function" .= object
                  [ "name" .= ("search" :: String)
                  , "description" .= ("Search" :: String)
                  , "parameters" .= object ["type" .= ("object" :: String)]
                  ]
              ]

            anthropicFormat = anthropicToolToJSON anthropicTool
            Object anthropicObj = anthropicFormat
            Object openAIObj = openAIFormat

        -- Anthropic: flat structure with input_schema
        KM.member "name" anthropicObj `shouldBe` True
        KM.member "input_schema" anthropicObj `shouldBe` True
        KM.member "function" anthropicObj `shouldBe` False

        -- OpenAI: nested structure with parameters
        KM.member "type" openAIObj `shouldBe` True
        KM.member "function" openAIObj `shouldBe` True
        KM.member "name" openAIObj `shouldBe` False  -- name is inside 'function'
