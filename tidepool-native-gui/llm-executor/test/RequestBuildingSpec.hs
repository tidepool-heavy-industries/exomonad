-- | Tests for LLM request building.
--
-- Verifies that API requests are built with correct formats for each provider.
module RequestBuildingSpec (spec) where

import Data.Aeson (Value(..), ToJSON(..), object, (.=))
import Data.Aeson.KeyMap qualified as KM
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
        let req = buildAnthropicRequest baseConfig "Hello" Nothing
            Object fields = toJSON req

        KM.lookup "model" fields `shouldBe` Just (String "claude-sonnet-4-20250514")
        KM.lookup "max_tokens" fields `shouldBe` Just (Number 1024)

      it "includes user message in messages array" $ do
        let req = buildAnthropicRequest baseConfig "Hello world" Nothing
            Object fields = toJSON req
            Just (Array messages) = KM.lookup "messages" fields
            [Object msg] = V.toList messages

        KM.lookup "role" msg `shouldBe` Just (String "user")
        KM.lookup "content" msg `shouldBe` Just (String "Hello world")

      it "includes system prompt when provided" $ do
        let configWithSystem = baseConfig { acSystemPrompt = Just "You are a helpful assistant" }
            req = buildAnthropicRequest configWithSystem "Hi" Nothing
            Object fields = toJSON req

        KM.lookup "system" fields `shouldBe` Just (String "You are a helpful assistant")

      it "omits system prompt when not provided" $ do
        let req = buildAnthropicRequest baseConfig "Hi" Nothing
            Object fields = toJSON req

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
            req = buildAnthropicRequest baseConfig "Search for cats" (Just [anthropicTool])
            Object fields = toJSON req
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
            req = buildAnthropicRequest baseConfig "What's the weather?" (Just [toolJson])
            Object fields = toJSON req
            Just (Array tools) = KM.lookup "tools" fields
            [Object parsedTool] = V.toList tools

        KM.lookup "name" parsedTool `shouldBe` Just (String "get_weather")
        KM.member "input_schema" parsedTool `shouldBe` True
        KM.member "parameters" parsedTool `shouldBe` False

      it "omits tools when None provided" $ do
        let req = buildAnthropicRequest baseConfig "Hello" Nothing
            Object fields = toJSON req

        KM.member "tools" fields `shouldBe` False

    describe "thinking budget" $ do
      it "includes thinking config when budget provided" $ do
        let configWithThinking = baseConfig { acThinkingBudget = Just 5000 }
            req = buildAnthropicRequest configWithThinking "Think hard" Nothing
            Object fields = toJSON req
            Just (Object thinking) = KM.lookup "thinking" fields

        KM.lookup "type" thinking `shouldBe` Just (String "enabled")
        KM.lookup "budget_tokens" thinking `shouldBe` Just (Number 5000)

      it "omits thinking when no budget" $ do
        let req = buildAnthropicRequest baseConfig "Hello" Nothing
            Object fields = toJSON req

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
        let req = buildOpenAIRequest baseConfig "Hello" Nothing
            Object fields = toJSON req

        KM.lookup "model" fields `shouldBe` Just (String "gpt-4o")
        KM.lookup "max_tokens" fields `shouldBe` Just (Number 2048)

      it "puts system prompt in messages array (not top-level)" $ do
        let configWithSystem = baseConfig { oaSystemPrompt = Just "Be helpful" }
            req = buildOpenAIRequest configWithSystem "Hi" Nothing
            Object fields = toJSON req
            Just (Array messages) = KM.lookup "messages" fields

        -- OpenAI uses messages array for system prompt
        V.length messages `shouldBe` 2
        let [Object sysMsg, Object userMsg] = V.toList messages
        KM.lookup "role" sysMsg `shouldBe` Just (String "system")
        KM.lookup "content" sysMsg `shouldBe` Just (String "Be helpful")
        KM.lookup "role" userMsg `shouldBe` Just (String "user")

    describe "tool format" $ do
      it "passes pre-formatted OpenAI tools directly" $ do
        -- OpenAI expects: { "type": "function", "function": { "name": ..., "parameters": ... } }
        -- Callers should pass tools already in this format (via CfTool)
        let openaiTool = object
              [ "type" .= ("function" :: String)
              , "function" .= object
                  [ "name" .= ("search" :: String)
                  , "description" .= ("Search" :: String)
                  , "parameters" .= object ["type" .= ("object" :: String)]
                  ]
              ]
            req = buildOpenAIRequest baseConfig "Search" (Just [openaiTool])
            Object fields = toJSON req
            Just (Array tools) = KM.lookup "tools" fields
            [Object passedTool] = V.toList tools

        -- Tool should be passed through unchanged
        KM.lookup "type" passedTool `shouldBe` Just (String "function")
        KM.member "function" passedTool `shouldBe` True

  describe "format comparison" $ do
    it "Anthropic and OpenAI use different tool formats" $ do
      -- Document the key difference: Anthropic is flat with input_schema,
      -- OpenAI is wrapped with type: "function" and function: {..., parameters}
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

          -- Anthropic tool uses input_schema (flat format)
          anthropicTool = object
            [ "name" .= ("test" :: String)
            , "description" .= ("Test" :: String)
            , "input_schema" .= object ["type" .= ("object" :: String)]
            ]

          -- OpenAI tool uses wrapped format with parameters (via CfTool)
          openaiTool = object
            [ "type" .= ("function" :: String)
            , "function" .= object
                [ "name" .= ("test" :: String)
                , "description" .= ("Test" :: String)
                , "parameters" .= object ["type" .= ("object" :: String)]
                ]
            ]

          anthropicReq = buildAnthropicRequest anthropicConfig "Test" (Just [anthropicTool])
          openaiReq = buildOpenAIRequest openaiConfig "Test" (Just [openaiTool])

          Object anthropicFields = toJSON anthropicReq
          Object openaiFields = toJSON openaiReq

          Just (Array anthropicTools) = KM.lookup "tools" anthropicFields
          Just (Array openaiTools) = KM.lookup "tools" openaiFields
          [Object anthropicParsedTool] = V.toList anthropicTools
          [Object openaiPassedTool] = V.toList openaiTools

      -- Anthropic: flat with input_schema
      KM.member "input_schema" anthropicParsedTool `shouldBe` True
      KM.member "function" anthropicParsedTool `shouldBe` False

      -- OpenAI: wrapped with type and function (passed through from CfTool format)
      KM.member "type" openaiPassedTool `shouldBe` True
      KM.member "function" openaiPassedTool `shouldBe` True
