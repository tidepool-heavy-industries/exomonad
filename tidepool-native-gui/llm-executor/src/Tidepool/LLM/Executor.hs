-- | LLM effect executor - Anthropic/OpenAI HTTP client.
--
-- Implements LLMComplete effect with native HTTP client.
-- Type-level provider determines request/response schema.
--
-- = Usage
--
-- @
-- import Tidepool.LLM.Executor (runLLMComplete, LLMEnv, mkLLMEnv)
-- import Tidepool.LLM.Types (LLMConfig(..), AnthropicSecrets(..))
-- import Tidepool.Effects.LLMProvider (LLMComplete, complete, SAnthropic, AnthropicConfig(..))
--
-- config = LLMConfig
--   { lcAnthropicSecrets = Just $ defaultAnthropicConfig "sk-..."
--   , lcOpenAISecrets = Nothing
--   }
--
-- main = do
--   env <- mkLLMEnv config
--   runM $ runLLMComplete env $ do
--     response <- complete SAnthropic anthropicCfg "Hello" Nothing
--     -- process response
-- @
module Tidepool.LLM.Executor
  ( -- * Executor
    runLLMComplete

    -- * Re-exports for convenience
  , LLMEnv
  , LLMConfig(..)
  , mkLLMEnv

    -- * Tool Types
  , AnthropicTool(..)
  , anthropicToolToJSON

    -- * Request Building (exported for testing)
  , buildAnthropicRequest
  , buildOpenAIRequest
  ) where

import Control.Exception (try, SomeException)
import Control.Monad.Freer (Eff, LastMember, interpret, sendM)
import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , Value(..)
  , eitherDecode
  , encode
  , object
  , withObject
  , (.:)
  , (.:?)
  , (.=)
  )
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.HTTP.Client
  ( Request(..)
  , RequestBody(..)
  , Response(..)
  , httpLbs
  , parseRequest
  )
import Network.HTTP.Types.Status (statusCode)

import Tidepool.LLM.Types
  ( LLMEnv(..)
  , LLMConfig(..)
  , AnthropicSecrets(..)
  , OpenAISecrets(..)
  , mkLLMEnv
  , AnthropicTool(..)
  , anthropicToolToJSON
  )
import Tidepool.Effects.LLMProvider
  ( LLMComplete(..)
  , LLMError(..)
  , SProvider(..)
  , AnthropicConfig(..)
  , OpenAIConfig(..)
  , AnthropicResponse(..)
  , OpenAIResponse(..)
  , ContentBlock(..)
  , Choice(..)
  , Message(..)
  , ToolCall(..)
  , FunctionCall(..)
  , Usage(..)
  )


-- ════════════════════════════════════════════════════════════════════════════
-- ANTHROPIC HTTP CLIENT
-- ════════════════════════════════════════════════════════════════════════════

-- | Make a request to the Anthropic Messages API.
anthropicRequest
  :: LLMEnv
  -> AnthropicConfig
  -> Text              -- ^ User message
  -> Maybe [Value]     -- ^ Optional tools
  -> IO (Either LLMError AnthropicResponse)
anthropicRequest env config userMsg maybeTools = do
  case env.leConfig.lcAnthropicSecrets of
    Nothing -> pure $ Left LLMNoProviderConfigured
    Just secrets -> do
      let url = T.unpack secrets.asBaseUrl <> "/v1/messages"
          body = buildAnthropicRequest config userMsg maybeTools

      result <- try @SomeException $ do
        req0 <- parseRequest url
        let req = req0
              { method = "POST"
              , requestHeaders =
                  [ ("x-api-key", TE.encodeUtf8 secrets.asApiKey)
                  , ("anthropic-version", "2023-06-01")
                  , ("content-type", "application/json")
                  ]
              , requestBody = RequestBodyLBS body
              }
        httpLbs req env.leManager

      case result of
        Left exc -> pure $ Left $ LLMHttpError $ T.pack (show exc)
        Right resp -> parseAnthropicResponse resp

-- | Build Anthropic Messages API request body.
--
-- Tools should be in Anthropic format (with @input_schema@, not @parameters@).
-- Use 'AnthropicTool' or 'Tidepool.Tool.toolToJSON' which produce the correct format.
buildAnthropicRequest :: AnthropicConfig -> Text -> Maybe [Value] -> LBS.ByteString
buildAnthropicRequest config userMsg maybeTools =
  encode $ object $ filter ((/= Null) . snd)
    [ "model" .= config.acModel
    , "max_tokens" .= config.acMaxTokens
    , "messages" .= [object ["role" .= ("user" :: Text), "content" .= userMsg]]
    , "system" .= config.acSystemPrompt
    , "tools" .= maybeTools
    , "thinking" .= case config.acThinkingBudget of
        Nothing -> Null
        Just budget -> object
          [ "type" .= ("enabled" :: Text)
          , "budget_tokens" .= budget
          ]
    ]

-- | Parse Anthropic API response.
parseAnthropicResponse :: Response LBS.ByteString -> IO (Either LLMError AnthropicResponse)
parseAnthropicResponse resp = do
  let status = statusCode (responseStatus resp)
      body = responseBody resp

  case status of
    401 -> pure $ Left LLMUnauthorized
    429 -> pure $ Left LLMRateLimited
    529 -> pure $ Left LLMOverloaded
    _ | status >= 200 && status < 300 ->
        case eitherDecode body of
          Left err -> pure $ Left $ LLMParseError $ T.pack err
          Right r -> pure $ Right r
      | otherwise ->
        pure $ Left $ parseErrorResponse body


-- ════════════════════════════════════════════════════════════════════════════
-- OPENAI HTTP CLIENT
-- ════════════════════════════════════════════════════════════════════════════

-- | Make a request to the OpenAI Chat Completions API.
openaiRequest
  :: LLMEnv
  -> OpenAIConfig
  -> Text              -- ^ User message
  -> Maybe [Value]     -- ^ Optional tools
  -> IO (Either LLMError OpenAIResponse)
openaiRequest env config userMsg maybeTools = do
  case env.leConfig.lcOpenAISecrets of
    Nothing -> pure $ Left LLMNoProviderConfigured
    Just secrets -> do
      let url = T.unpack secrets.osBaseUrl <> "/v1/chat/completions"
          body = buildOpenAIRequest config userMsg maybeTools

      result <- try @SomeException $ do
        req0 <- parseRequest url
        let req = req0
              { method = "POST"
              , requestHeaders =
                  [ ("Authorization", "Bearer " <> TE.encodeUtf8 secrets.osApiKey)
                  , ("Content-Type", "application/json")
                  ] <> maybe [] (\org -> [("OpenAI-Organization", TE.encodeUtf8 org)]) secrets.osOrgId
              , requestBody = RequestBodyLBS body
              }
        httpLbs req env.leManager

      case result of
        Left exc -> pure $ Left $ LLMHttpError $ T.pack (show exc)
        Right resp -> parseOpenAIResponse resp

-- | Build OpenAI Chat Completions API request body.
--
-- __Tools format__: Tools should be in OpenAI function format:
--
-- @
-- { "type": "function", "function": { "name": "...", "description": "...", "parameters": {...} } }
-- @
--
-- Use 'CfTool' from "Tidepool.Tool.Wire" or the 'ToCfTool' typeclass to produce
-- correctly formatted tools. Do NOT pass Anthropic-format tools here.
buildOpenAIRequest :: OpenAIConfig -> Text -> Maybe [Value] -> LBS.ByteString
buildOpenAIRequest config userMsg maybeTools =
  encode $ object $ filter ((/= Null) . snd)
    [ "model" .= config.oaModel
    , "max_tokens" .= config.oaMaxTokens
    , "messages" .=
        (maybe [] (\sys -> [object ["role" .= ("system" :: Text), "content" .= sys]]) config.oaSystemPrompt
         ++ [object ["role" .= ("user" :: Text), "content" .= userMsg]])
    , "temperature" .= config.oaTemperature
    , "tools" .= maybeTools  -- Tools should already be in OpenAI format (via CfTool)
    ]

-- | Parse OpenAI API response.
parseOpenAIResponse :: Response LBS.ByteString -> IO (Either LLMError OpenAIResponse)
parseOpenAIResponse resp = do
  let status = statusCode (responseStatus resp)
      body = responseBody resp

  case status of
    401 -> pure $ Left LLMUnauthorized
    429 -> pure $ Left LLMRateLimited
    _ | status >= 200 && status < 300 ->
        case eitherDecode body of
          Left err -> pure $ Left $ LLMParseError $ T.pack err
          Right r -> pure $ Right r
      | otherwise ->
        pure $ Left $ parseErrorResponse body


-- ════════════════════════════════════════════════════════════════════════════
-- ERROR PARSING
-- ════════════════════════════════════════════════════════════════════════════

-- | Parse error response body.
parseErrorResponse :: LBS.ByteString -> LLMError
parseErrorResponse body = case eitherDecode body of
  Left _ -> LLMApiError "unknown" "Failed to parse error response"
  Right val -> parseErrorValue val

-- | Parse error from JSON value.
parseErrorValue :: Value -> LLMError
parseErrorValue (Object obj) =
  case KM.lookup (Key.fromText "error") obj of
    Just (Object errObj) ->
      let errType = case KM.lookup (Key.fromText "type") errObj of
            Just (String t) -> t
            _ -> "unknown"
          errMsg = case KM.lookup (Key.fromText "message") errObj of
            Just (String m) -> m
            _ -> ""
      in  checkContextLength errType errMsg
    _ -> LLMApiError "unknown" "Malformed error response"
parseErrorValue _ = LLMApiError "unknown" "Non-object error response"

-- | Check for context length errors.
checkContextLength :: Text -> Text -> LLMError
checkContextLength errType errMsg
  | "context" `T.isInfixOf` T.toLower errMsg && "long" `T.isInfixOf` T.toLower errMsg
    = LLMContextTooLong
  | otherwise
    = LLMApiError errType errMsg


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT INTERPRETER
-- ════════════════════════════════════════════════════════════════════════════

-- | Run LLMComplete effects using native HTTP client.
--
-- This interpreter makes real API calls to Anthropic or OpenAI based on
-- the type-level provider in the effect.
--
-- = Error Handling
--
-- * 'Complete' - throws via 'error' on failure (use for fatal errors)
-- * 'CompleteTry' - returns @Either LLMError@ (use for graceful handling)
--
-- = Example
--
-- @
-- import Control.Monad.Freer (runM)
--
-- main = do
--   let config = LLMConfig { lcAnthropicSecrets = Just secrets, ... }
--   env <- mkLLMEnv config
--   runM $ runLLMComplete env $ do
--     -- Throwing variant (crashes on error)
--     response <- complete SAnthropic cfg "Hello" Nothing
--
--     -- Try variant (returns Either)
--     result <- completeTry SAnthropic cfg "Hello" Nothing
--     case result of
--       Left err -> handleError err
--       Right response -> pure response
-- @
runLLMComplete :: LastMember IO effs => LLMEnv -> Eff (LLMComplete ': effs) a -> Eff effs a
runLLMComplete env = interpret $ \case
  -- Throwing variants (for when errors are fatal)
  Complete SAnthropic config msg tools -> sendM $ do
    result <- anthropicRequest env config msg tools
    case result of
      Left err -> error $ "LLMComplete (Anthropic): " <> show err
      Right resp -> pure resp

  Complete SOpenAI config msg tools -> sendM $ do
    result <- openaiRequest env config msg tools
    case result of
      Left err -> error $ "LLMComplete (OpenAI): " <> show err
      Right resp -> pure resp

  -- Try variants (for graceful error handling)
  CompleteTry SAnthropic config msg tools -> sendM $
    anthropicRequest env config msg tools

  CompleteTry SOpenAI config msg tools -> sendM $
    openaiRequest env config msg tools
