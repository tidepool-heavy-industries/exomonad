-- | Tests for LLM interpreter types.
--
-- Verifies that tool types serialize to the correct provider-specific formats.
module TypesSpec (spec) where

import Data.Aeson (Value (..), decode, encode, object, (.=))
import Data.Aeson.KeyMap qualified as KM
import ExoMonad.LLM.Interpreter.Types (AnthropicTool (..), anthropicToolToJSON)
import Test.Hspec

spec :: Spec
spec = do
  describe "AnthropicTool" $ do
    describe "JSON serialization" $ do
      it "uses 'input_schema' key" $ do
        let tool =
              AnthropicTool
                { atName = "search",
                  atDescription = "Search the web",
                  atInputSchema =
                    object
                      [ "type" .= ("object" :: String),
                        "properties"
                          .= object
                            [ "query" .= object ["type" .= ("string" :: String)]
                            ],
                        "required" .= (["query"] :: [String])
                      ]
                }
            json = anthropicToolToJSON tool
            Object obj = json

        KM.member "input_schema" obj `shouldBe` True

      it "serializes name and description correctly" $ do
        let tool =
              AnthropicTool
                { atName = "my_tool",
                  atDescription = "Does something useful",
                  atInputSchema = object ["type" .= ("object" :: String)]
                }
            json = anthropicToolToJSON tool
            Object obj = json

        KM.lookup "name" obj `shouldBe` Just (String "my_tool")
        KM.lookup "description" obj `shouldBe` Just (String "Does something useful")

      it "round-trips through JSON encoding" $ do
        let tool =
              AnthropicTool
                { atName = "test",
                  atDescription = "A test tool",
                  atInputSchema =
                    object
                      [ "type" .= ("object" :: String),
                        "properties" .= object []
                      ]
                }
            encoded = encode tool
            decoded = decode encoded :: Maybe Value
            original = anthropicToolToJSON tool

        decoded `shouldBe` Just original
