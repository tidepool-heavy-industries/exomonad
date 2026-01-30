-- | Tests for LLM request building.
--
-- Verifies that API requests are built with correct formats for each provider.
module RequestBuildingSpec (spec) where

import Data.Aeson (ToJSON (..), Value (..), object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.Vector qualified as V
import ExoMonad.Effects.LLMProvider (AnthropicConfig (..), ThinkingBudget (..))
import ExoMonad.LLM.Interpreter (AnthropicTool (..), buildAnthropicRequest)
import Test.Hspec

spec :: Spec
spec = do
  describe "buildAnthropicRequest" $ do
    let baseConfig =
          AnthropicConfig
            { acModel = "claude-sonnet-4-20250514",
              acMaxTokens = 1024,
              acThinking = ThinkingDisabled,
              acSystemPrompt = Nothing
            }

    describe "basic request structure" $ do
      it "includes model and max_tokens" $ do
        let req = buildAnthropicRequest baseConfig "Hello" Nothing Nothing
            Object fields = toJSON req

        KM.lookup "model" fields `shouldBe` Just (String "claude-sonnet-4-20250514")
        KM.lookup "max_tokens" fields `shouldBe` Just (Number 1024)

      it "includes user message in messages array" $ do
        let req = buildAnthropicRequest baseConfig "Hello world" Nothing Nothing
            Object fields = toJSON req
            Just (Array messages) = KM.lookup "messages" fields
            [Object msg] = V.toList messages

        KM.lookup "role" msg `shouldBe` Just (String "user")
        KM.lookup "content" msg `shouldBe` Just (String "Hello world")

      it "includes system prompt when provided" $ do
        let configWithSystem = baseConfig {acSystemPrompt = Just "You are a helpful assistant"}
            req = buildAnthropicRequest configWithSystem "Hi" Nothing Nothing
            Object fields = toJSON req

        KM.lookup "system" fields `shouldBe` Just (String "You are a helpful assistant")

      it "omits system prompt when not provided" $ do
        let req = buildAnthropicRequest baseConfig "Hi" Nothing Nothing
            Object fields = toJSON req

        -- Null values are filtered out, so 'system' should not be present
        KM.member "system" fields `shouldBe` False

    describe "tool format" $ do
      it "passes tools array directly" $ do
        let anthropicTool =
              object
                [ "name" .= ("search" :: String),
                  "description" .= ("Search the web" :: String),
                  "input_schema"
                    .= object
                      [ "type" .= ("object" :: String),
                        "properties"
                          .= object
                            [ "query" .= object ["type" .= ("string" :: String)]
                            ]
                      ]
                ]
            req = buildAnthropicRequest baseConfig "Search for cats" (Just [anthropicTool]) Nothing
            Object fields = toJSON req
            Just (Array tools) = KM.lookup "tools" fields
            [Object tool] = V.toList tools

        -- Tool should have input_schema (Anthropic format)
        KM.member "input_schema" tool `shouldBe` True

      it "preserves input_schema key from AnthropicTool" $ do
        let tool =
              AnthropicTool
                { atName = "get_weather",
                  atDescription = "Get weather for a location",
                  atInputSchema =
                    object
                      [ "type" .= ("object" :: String),
                        "properties"
                          .= object
                            [ "location" .= object ["type" .= ("string" :: String)]
                            ],
                        "required" .= (["location"] :: [String])
                      ]
                }
            toolJson =
              object
                [ "name" .= tool.atName,
                  "description" .= tool.atDescription,
                  "input_schema" .= tool.atInputSchema
                ]
            req = buildAnthropicRequest baseConfig "What's the weather?" (Just [toolJson]) Nothing
            Object fields = toJSON req
            Just (Array tools) = KM.lookup "tools" fields
            [Object parsedTool] = V.toList tools

        KM.lookup "name" parsedTool `shouldBe` Just (String "get_weather")
        KM.member "input_schema" parsedTool `shouldBe` True

      it "omits tools when None provided" $ do
        let req = buildAnthropicRequest baseConfig "Hello" Nothing Nothing
            Object fields = toJSON req

        KM.member "tools" fields `shouldBe` False

    describe "thinking budget" $ do
      it "includes thinking config when budget provided" $ do
        let configWithThinking = baseConfig {acThinking = ThinkingEnabled 5000}
            req = buildAnthropicRequest configWithThinking "Think hard" Nothing Nothing
            Object fields = toJSON req
            Just (Object thinking) = KM.lookup "thinking" fields

        KM.lookup "type" thinking `shouldBe` Just (String "enabled")
        KM.lookup "budget_tokens" thinking `shouldBe` Just (Number 5000)

      it "omits thinking when no budget" $ do
        let req = buildAnthropicRequest baseConfig "Hello" Nothing Nothing
            Object fields = toJSON req

        KM.member "thinking" fields `shouldBe` False
