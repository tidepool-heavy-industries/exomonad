{-# LANGUAGE TypeFamilies #-}
-- | LLM effect executor - Anthropic/OpenAI HTTP client.
--
-- Implements LLMComplete effect with native HTTP client.
-- Type-level provider determines request/response schema.
module Tidepool.LLM.Executor
  ( -- * Executor
    runLLMComplete

    -- * Configuration
  , LLMSecrets(..)

    -- * Errors
  , LLMError(..)
  ) where

import Control.Monad.Freer (Eff, LastMember, sendM, interpret)
import Data.Aeson
import Data.Aeson.Types (Parser, parseMaybe)
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Tidepool.Effects.LLMProvider
  ( LLMComplete(..)
  , SProvider(..)
  , LLMProvider(..)
  , LLMProviderConfig
  , LLMProviderResponse
  , AnthropicConfig(..)
  , OpenAIConfig(..)
  , AnthropicResponse(..)
  , OpenAIResponse(..)
  , ContentBlock(..)
  , Usage(..)
  , Choice(..)
  , Message(..)
  , ToolCall(..)
  , FunctionCall(..)
  )


-- ════════════════════════════════════════════════════════════════════════════
-- CONFIGURATION
-- ════════════════════════════════════════════════════════════════════════════

-- | LLM API secrets.
data LLMSecrets = LLMSecrets
  { lsAnthropicKey :: Maybe Text
  , lsOpenAIKey :: Maybe Text
  }
  deriving (Show, Eq)


-- ════════════════════════════════════════════════════════════════════════════
-- ERRORS
-- ════════════════════════════════════════════════════════════════════════════

-- | Errors that can occur during LLM calls.
data LLMError
  = MissingApiKey Text           -- ^ API key not configured
  | HttpError Text               -- ^ Network or HTTP error
  | ParseError Text              -- ^ Failed to parse response
  | ApiError Text                -- ^ API returned an error
  deriving (Show, Eq)


-- ════════════════════════════════════════════════════════════════════════════
-- INTERPRETER
-- ════════════════════════════════════════════════════════════════════════════

-- | Run LLMComplete effects by making actual HTTP calls.
--
-- This interpreter handles both Anthropic and OpenAI providers
-- based on the type-level provider switch.
--
-- Note: Requires IO as the last effect in the stack.
--
-- Example usage:
--
-- @
-- import Control.Monad.Freer (runM)
--
-- main :: IO ()
-- main = runM $ runLLMComplete secrets $ do
--   response <- complete SAnthropic config "Hello" Nothing
--   -- use response
-- @
runLLMComplete
  :: LastMember IO effs
  => LLMSecrets
  -> Eff (LLMComplete ': effs) a
  -> Eff effs a
runLLMComplete secrets = interpret $ \case
  Complete provider config msg mTools ->
    sendM $ dispatchLLMIO secrets provider config msg mTools


-- | Dispatch to the appropriate provider based on the singleton.
dispatchLLMIO
  :: LLMSecrets
  -> SProvider p
  -> LLMProviderConfig p
  -> Text
  -> Maybe [Value]
  -> IO (LLMProviderResponse p)
dispatchLLMIO secrets provider config msg mTools = case provider of
  SAnthropic -> callAnthropicIO secrets config msg mTools
  SOpenAI    -> callOpenAIIO secrets config msg mTools


-- ════════════════════════════════════════════════════════════════════════════
-- ANTHROPIC IMPLEMENTATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Call Anthropic Messages API.
callAnthropicIO
  :: LLMSecrets
  -> AnthropicConfig
  -> Text
  -> Maybe [Value]
  -> IO AnthropicResponse
callAnthropicIO secrets config msg mTools = do
  apiKey <- case secrets.lsAnthropicKey of
    Nothing -> fail "Anthropic API key not configured"
    Just k -> pure k

  manager <- newManager tlsManagerSettings

  -- Build request body
  let body = object $ filter ((/= Null) . snd)
        [ "model" .= config.acModel
        , "max_tokens" .= config.acMaxTokens
        , "messages" .= [object
            [ "role" .= ("user" :: Text)
            , "content" .= msg
            ]]
        , "system" .= config.acSystemPrompt
        , "tools" .= mTools
        , "thinking" .= ((\budget -> object
            [ "type" .= ("enabled" :: Text)
            , "budget_tokens" .= budget
            ]) <$> config.acThinkingBudget)
        ]

  -- Build HTTP request
  initReq <- parseRequest "https://api.anthropic.com/v1/messages"
  let request = initReq
        { method = "POST"
        , requestHeaders =
            [ ("content-type", "application/json")
            , ("x-api-key", TE.encodeUtf8 apiKey)
            , ("anthropic-version", "2023-06-01")
            , ("anthropic-beta", "interleaved-thinking-2025-05-14")
            ]
        , requestBody = RequestBodyLBS (encode body)
        }

  -- Make request
  response <- httpLbs request manager

  -- Parse response
  let respBody = responseBody response
  case eitherDecode respBody of
    Left err -> fail $ "Parse error: " <> err
    Right jsonVal -> case parseAnthropicResponse jsonVal of
      Nothing -> fail "Failed to parse Anthropic response structure"
      Just resp -> pure resp


-- | Parse Anthropic API response JSON.
parseAnthropicResponse :: Value -> Maybe AnthropicResponse
parseAnthropicResponse = parseMaybe $ withObject "AnthropicResponse" $ \v -> do
  -- Check for error first
  errorObj <- v .:? "error"
  case errorObj of
    Just (Object o) -> do
      errMsg <- o .:? "message" .!= "Unknown error"
      fail $ "API Error: " <> T.unpack errMsg
    Just _ -> fail "API Error: Unknown error"
    Nothing -> do
      -- Parse success response
      content <- v .: "content"
      stopReason <- v .: "stop_reason"
      usage <- v .: "usage"
      promptTokens <- usage .: "input_tokens"
      completionTokens <- usage .: "output_tokens"
      pure AnthropicResponse
        { arContent = parseContentBlocks content
        , arStopReason = stopReason
        , arUsage = Usage
            { uPromptTokens = promptTokens
            , uCompletionTokens = completionTokens
            }
        }


-- | Parse content blocks from response.
parseContentBlocks :: [Value] -> [ContentBlock]
parseContentBlocks = mapMaybe parseBlock
  where
    parseBlock :: Value -> Maybe ContentBlock
    parseBlock = parseMaybe $ withObject "ContentBlock" $ \v -> do
      blockType <- v .: "type" :: Parser Text
      case blockType of
        "text" -> TextContent <$> v .: "text"
        "tool_use" -> ToolUseContent <$> v .: "name" <*> v .: "input"
        _ -> fail "Unknown content block type"


-- ════════════════════════════════════════════════════════════════════════════
-- OPENAI IMPLEMENTATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Call OpenAI Chat Completions API.
callOpenAIIO
  :: LLMSecrets
  -> OpenAIConfig
  -> Text
  -> Maybe [Value]
  -> IO OpenAIResponse
callOpenAIIO secrets config msg mTools = do
  apiKey <- case secrets.lsOpenAIKey of
    Nothing -> fail "OpenAI API key not configured"
    Just k -> pure k

  manager <- newManager tlsManagerSettings

  -- Build messages
  let messages = case config.oaSystemPrompt of
        Nothing -> [userMsg]
        Just sys -> [systemMsg sys, userMsg]
      userMsg = object
        [ "role" .= ("user" :: Text)
        , "content" .= msg
        ]
      systemMsg sys = object
        [ "role" .= ("system" :: Text)
        , "content" .= sys
        ]

  -- Build request body
  let body = object $ filter ((/= Null) . snd)
        [ "model" .= config.oaModel
        , "max_tokens" .= config.oaMaxTokens
        , "messages" .= messages
        , "temperature" .= config.oaTemperature
        , "tools" .= (convertToolsToOpenAI <$> mTools)
        ]

  -- Build HTTP request
  initReq <- parseRequest "https://api.openai.com/v1/chat/completions"
  let request = initReq
        { method = "POST"
        , requestHeaders =
            [ ("content-type", "application/json")
            , ("Authorization", "Bearer " <> TE.encodeUtf8 apiKey)
            ]
        , requestBody = RequestBodyLBS (encode body)
        }

  -- Make request
  response <- httpLbs request manager

  -- Parse response
  let respBody = responseBody response
  case eitherDecode respBody of
    Left err -> fail $ "Parse error: " <> err
    Right jsonVal -> case parseOpenAIResponse jsonVal of
      Nothing -> fail "Failed to parse OpenAI response structure"
      Just resp -> pure resp


-- | Convert Anthropic-style tool definitions to OpenAI format.
convertToolsToOpenAI :: [Value] -> [Value]
convertToolsToOpenAI = map convertTool
  where
    convertTool :: Value -> Value
    convertTool (Object o) = object
      [ "type" .= ("function" :: Text)
      , "function" .= object
          [ "name" .= KM.lookup "name" o
          , "description" .= KM.lookup "description" o
          , "parameters" .= KM.lookup "input_schema" o
          ]
      ]
    convertTool v = v


-- | Parse OpenAI API response JSON.
parseOpenAIResponse :: Value -> Maybe OpenAIResponse
parseOpenAIResponse = parseMaybe $ withObject "OpenAIResponse" $ \v -> do
  -- Check for error first
  errorObj <- v .:? "error"
  case errorObj of
    Just (Object o) -> do
      errMsg <- o .:? "message" .!= "Unknown error"
      fail $ "API Error: " <> T.unpack errMsg
    Just _ -> fail "API Error: Unknown error"
    Nothing -> do
      -- Parse success response
      choices <- v .: "choices"
      usage <- v .: "usage"
      promptTokens <- usage .: "prompt_tokens"
      completionTokens <- usage .: "completion_tokens"
      pure OpenAIResponse
        { orChoices = parseChoices choices
        , orUsage = Usage
            { uPromptTokens = promptTokens
            , uCompletionTokens = completionTokens
            }
        }


-- | Parse choices from OpenAI response.
parseChoices :: [Value] -> [Choice]
parseChoices = mapMaybe parseChoice
  where
    parseChoice :: Value -> Maybe Choice
    parseChoice = parseMaybe $ withObject "Choice" $ \v -> do
      message <- v .: "message"
      finishReason <- v .:? "finish_reason"
      msgRole <- message .: "role"
      msgContent <- message .:? "content"
      msgToolCalls <- message .:? "tool_calls"
      pure Choice
        { cMessage = Message
            { mRole = msgRole
            , mContent = msgContent
            , mToolCalls = parseToolCalls <$> msgToolCalls
            }
        , cFinishReason = finishReason
        }

    parseToolCalls :: [Value] -> [ToolCall]
    parseToolCalls = mapMaybe parseToolCall

    parseToolCall :: Value -> Maybe ToolCall
    parseToolCall = parseMaybe $ withObject "ToolCall" $ \v -> do
      tcId <- v .: "id"
      tcType <- v .: "type"
      func <- v .: "function"
      funcName <- func .: "name"
      funcArgs <- func .: "arguments"
      pure ToolCall
        { tcId = tcId
        , tcType = tcType
        , tcFunction = FunctionCall
            { fcName = funcName
            , fcArguments = funcArgs
            }
        }
