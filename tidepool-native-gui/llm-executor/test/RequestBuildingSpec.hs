-- | Tests for LLM request building.
--
-- Verifies that API requests are built with correct formats for each provider.
module RequestBuildingSpec (spec) where

import Data.Aeson (Value(..), decode, eitherDecode, object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (fromJust, isJust)
import Data.Vector qualified as V
import Test.Hspec

import Tidepool.LLM.Executor (buildAnthropicRequest, buildOpenAIRequest, AnthropicTool(..))
import Tidepool.Effects.LLMProvider (AnthropicConfig(..), OpenAIConfig(..))


spec :: Spec
spec = do
  describe "buildAnthropicRequest" $ do
    let baseConfig = AnthropicConfig
          { acModel = "claude-sonnet-4-20250514"
          , acMaxTokens = 1024
          , acThinkingBudget = Nothing
          , acSystemPrompt = Nothing
          }

    describe "basic request structure" $ do
      it "includes model and max_tokens" $ do
        let body = buildAnthropicRequest baseConfig "Hello" Nothing
            Just obj = decode body :: Maybe Value
            Object fields = obj

        KM.lookup "model" fields `shouldBe` Just (String "claude-sonnet-4-20250514")
        KM.lookup "max_tokens" fields `shouldBe` Just (Number 1024)

      it "includes user message in messages array" $ do
        let body = buildAnthropicRequest baseConfig "Hello world" Nothing
            Just obj = decode body :: Maybe Value
            Object fields = obj
            Just (Array messages) = KM.lookup "messages" fields
            [Object msg] = V.toList messages

        KM.lookup "role" msg `shouldBe` Just (String "user")
        KM.lookup "content" msg `shouldBe` Just (String "Hello world")

      it "includes system prompt when provided" $ do
        let configWithSystem = baseConfig { acSystemPrompt = Just "You are a helpful assistant" }
            body = buildAnthropicRequest configWithSystem "Hi" Nothing
            Just obj = decode body :: Maybe Value
            Object fields = obj

        KM.lookup "system" fields `shouldBe` Just (String "You are a helpful assistant")

      it "omits system prompt when not provided" $ do
        let body = buildAnthropicRequest baseConfig "Hi" Nothing
            Just obj = decode body :: Maybe Value
            Object fields = obj

        -- Null values are filtered out, so 'system' should not be present
        KM.member "system" fields `shouldBe` False

    describe "tool format" $ do
      it "passes tools array directly (no OpenAI wrapping)" $ do
        let anthropicTool = object
              [ "name" .= ("search" :: String)
              , "description" .= ("Search the web" :: String)
              , "input_schema" .= object
                  [ "type" .= ("object" :: String)
                  , "properties" .= object
                      [ "query" .= object ["type" .= ("string" :: String)]
                      ]
                  ]
              ]
            body = buildAnthropicRequest baseConfig "Search for cats" (Just [anthropicTool])
            Just obj = decode body :: Maybe Value
            Object fields = obj
            Just (Array tools) = KM.lookup "tools" fields
            [Object tool] = V.toList tools

        -- Tool should have input_schema (Anthropic format)
        KM.member "input_schema" tool `shouldBe` True
        -- Tool should NOT be wrapped in OpenAI format
        KM.member "type" tool `shouldBe` False
        KM.member "function" tool `shouldBe` False

      it "preserves input_schema key from AnthropicTool" $ do
        let tool = AnthropicTool
              { atName = "get_weather"
              , atDescription = "Get weather for a location"
              , atInputSchema = object
                  [ "type" .= ("object" :: String)
                  , "properties" .= object
                      [ "location" .= object ["type" .= ("string" :: String)]
                      ]
                  , "required" .= (["location"] :: [String])
                  ]
              }
            toolJson = object
              [ "name" .= tool.atName
              , "description" .= tool.atDescription
              , "input_schema" .= tool.atInputSchema
              ]
            body = buildAnthropicRequest baseConfig "What's the weather?" (Just [toolJson])
            Just obj = decode body :: Maybe Value
            Object fields = obj
            Just (Array tools) = KM.lookup "tools" fields
            [Object parsedTool] = V.toList tools

        KM.lookup "name" parsedTool `shouldBe` Just (String "get_weather")
        KM.member "input_schema" parsedTool `shouldBe` True
        KM.member "parameters" parsedTool `shouldBe` False

      it "omits tools when None provided" $ do
        let body = buildAnthropicRequest baseConfig "Hello" Nothing
            Just obj = decode body :: Maybe Value
            Object fields = obj

        KM.member "tools" fields `shouldBe` False

    describe "thinking budget" $ do
      it "includes thinking config when budget provided" $ do
        let configWithThinking = baseConfig { acThinkingBudget = Just 5000 }
            body = buildAnthropicRequest configWithThinking "Think hard" Nothing
            Just obj = decode body :: Maybe Value
            Object fields = obj
            Just (Object thinking) = KM.lookup "thinking" fields

        KM.lookup "type" thinking `shouldBe` Just (String "enabled")
        KM.lookup "budget_tokens" thinking `shouldBe` Just (Number 5000)

      it "omits thinking when no budget" $ do
        let body = buildAnthropicRequest baseConfig "Hello" Nothing
            Just obj = decode body :: Maybe Value
            Object fields = obj

        KM.member "thinking" fields `shouldBe` False

  describe "buildOpenAIRequest" $ do
    let baseConfig = OpenAIConfig
          { oaModel = "gpt-4o"
          , oaMaxTokens = 2048
          , oaTemperature = Nothing
          , oaSystemPrompt = Nothing
          }

    describe "basic request structure" $ do
      it "includes model and max_tokens" $ do
        let body = buildOpenAIRequest baseConfig "Hello" Nothing
            Just obj = decode body :: Maybe Value
            Object fields = obj

        KM.lookup "model" fields `shouldBe` Just (String "gpt-4o")
        KM.lookup "max_tokens" fields `shouldBe` Just (Number 2048)

      it "puts system prompt in messages array (not top-level)" $ do
        let configWithSystem = baseConfig { oaSystemPrompt = Just "Be helpful" }
            body = buildOpenAIRequest configWithSystem "Hi" Nothing
            Just obj = decode body :: Maybe Value
            Object fields = obj
            Just (Array messages) = KM.lookup "messages" fields

        -- OpenAI uses messages array for system prompt
        V.length messages `shouldBe` 2
        let [Object sysMsg, Object userMsg] = V.toList messages
        KM.lookup "role" sysMsg `shouldBe` Just (String "system")
        KM.lookup "content" sysMsg `shouldBe` Just (String "Be helpful")
        KM.lookup "role" userMsg `shouldBe` Just (String "user")

    describe "tool format" $ do
      it "wraps tools in OpenAI function format" $ do
        -- OpenAI expects: { "type": "function", "function": { "name": ..., "parameters": ... } }
        let flatTool = object
              [ "name" .= ("search" :: String)
              , "description" .= ("Search" :: String)
              , "parameters" .= object ["type" .= ("object" :: String)]
              ]
            body = buildOpenAIRequest baseConfig "Search" (Just [flatTool])
            Just obj = decode body :: Maybe Value
            Object fields = obj
            Just (Array tools) = KM.lookup "tools" fields
            [Object wrappedTool] = V.toList tools

        -- Should be wrapped in OpenAI format
        KM.lookup "type" wrappedTool `shouldBe` Just (String "function")
        KM.member "function" wrappedTool `shouldBe` True

  describe "format comparison" $ do
    it "Anthropic and OpenAI use different tool key names" $ do
      -- Document the key difference: input_schema vs parameters
      let anthropicConfig = AnthropicConfig
            { acModel = "claude-sonnet-4-20250514"
            , acMaxTokens = 1024
            , acThinkingBudget = Nothing
            , acSystemPrompt = Nothing
            }
          openaiConfig = OpenAIConfig
            { oaModel = "gpt-4o"
            , oaMaxTokens = 1024
            , oaTemperature = Nothing
            , oaSystemPrompt = Nothing
            }

          -- Anthropic tool uses input_schema
          anthropicTool = object
            [ "name" .= ("test" :: String)
            , "description" .= ("Test" :: String)
            , "input_schema" .= object ["type" .= ("object" :: String)]
            ]

          -- OpenAI tool uses parameters
          openaiTool = object
            [ "name" .= ("test" :: String)
            , "description" .= ("Test" :: String)
            , "parameters" .= object ["type" .= ("object" :: String)]
            ]

          anthropicBody = buildAnthropicRequest anthropicConfig "Test" (Just [anthropicTool])
          openaiBody = buildOpenAIRequest openaiConfig "Test" (Just [openaiTool])

          Just (Object anthropicFields) = decode anthropicBody
          Just (Object openaiFields) = decode openaiBody

          Just (Array anthropicTools) = KM.lookup "tools" anthropicFields
          Just (Array openaiTools) = KM.lookup "tools" openaiFields
          [Object anthropicParsedTool] = V.toList anthropicTools
          [Object openaiWrappedTool] = V.toList openaiTools

      -- Anthropic: flat with input_schema
      KM.member "input_schema" anthropicParsedTool `shouldBe` True
      KM.member "function" anthropicParsedTool `shouldBe` False

      -- OpenAI: wrapped with function containing parameters
      KM.member "type" openaiWrappedTool `shouldBe` True
      KM.member "function" openaiWrappedTool `shouldBe` True
