-- | LLM effect executor - Anthropic/OpenAI HTTP client.
--
-- Implements LLMComplete effect with servant-client.
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

    -- * Helpers (exported for testing)
  , parseBaseUrl
  , clientErrorToLLMError
  ) where

import Control.Monad.Freer (Eff, LastMember, interpret, sendM)
import Data.Aeson (Value)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.HTTP.Client (Manager)
import Servant.Client
  ( BaseUrl(..)
  , ClientError(..)
  , ClientM
  , Scheme(..)
  , mkClientEnv
  , runClientM
  )
import Servant.Client.Core (ResponseF(..))
import Network.HTTP.Types.Status (statusCode)

import Tidepool.LLM.Types
  ( LLMEnv(..)
  , LLMConfig(..)
  , AnthropicSecrets(..)
  , OpenAISecrets(..)
  , mkLLMEnv
  , AnthropicTool(..)
  , anthropicToolToJSON
  , getApiKey
  , getBaseUrl
  )
import Tidepool.Effects.LLMProvider
  ( LLMComplete(..)
  , LLMError(..)
  , SProvider(..)
  , AnthropicConfig(..)
  , OpenAIConfig(..)
  , ThinkingBudget(..)
  , AnthropicResponse(..)
  , OpenAIResponse(..)
  )
import Tidepool.LLM.API.Anthropic qualified as Anthropic
import Tidepool.LLM.API.OpenAI qualified as OpenAI


-- ════════════════════════════════════════════════════════════════════════════
-- ANTHROPIC CLIENT
-- ════════════════════════════════════════════════════════════════════════════

-- | Make a request to the Anthropic Messages API using servant-client.
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
      let baseUrl = parseBaseUrl (getBaseUrl secrets.asBaseUrl)
          clientEnv = mkClientEnv env.leManager baseUrl
          req = buildAnthropicRequest config userMsg maybeTools

      result <- runClientM (Anthropic.anthropicComplete (getApiKey secrets.asApiKey) req) clientEnv
      pure $ either (Left . clientErrorToLLMError) Right result

-- | Build Anthropic Messages API request.
--
-- Tools should be in Anthropic format (with @input_schema@, not @parameters@).
-- Use 'AnthropicTool' or 'Tidepool.Tool.toolToJSON' which produce the correct format.
buildAnthropicRequest :: AnthropicConfig -> Text -> Maybe [Value] -> Anthropic.AnthropicRequest
buildAnthropicRequest config userMsg maybeTools = Anthropic.AnthropicRequest
  { Anthropic.arModel = config.acModel
  , Anthropic.arMaxTokens = config.acMaxTokens
  , Anthropic.arMessages = [Anthropic.AnthropicMessage "user" userMsg]
  , Anthropic.arSystem = config.acSystemPrompt
  , Anthropic.arTools = maybeTools
  , Anthropic.arThinking = case config.acThinking of
      ThinkingDisabled -> Nothing
      ThinkingEnabled budget -> Just Anthropic.ThinkingConfig
        { Anthropic.tcType = "enabled"
        , Anthropic.tcBudgetTokens = budget
        }
  }


-- ════════════════════════════════════════════════════════════════════════════
-- OPENAI CLIENT
-- ════════════════════════════════════════════════════════════════════════════

-- | Make a request to the OpenAI Chat Completions API using servant-client.
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
      let baseUrl = parseBaseUrl (getBaseUrl secrets.osBaseUrl)
          clientEnv = mkClientEnv env.leManager baseUrl
          req = buildOpenAIRequest config userMsg maybeTools

      result <- runClientM (OpenAI.openaiComplete (getApiKey secrets.osApiKey) secrets.osOrgId req) clientEnv
      pure $ either (Left . clientErrorToLLMError) Right result

-- | Build OpenAI Chat Completions API request.
--
-- __Tools format__: Tools should be in OpenAI function format:
--
-- @
-- { "type": "function", "function": { "name": "...", "description": "...", "parameters": {...} } }
-- @
--
-- Use 'CfTool' from "Tidepool.Tool.Wire" or the 'ToCfTool' typeclass to produce
-- correctly formatted tools. Do NOT pass Anthropic-format tools here.
buildOpenAIRequest :: OpenAIConfig -> Text -> Maybe [Value] -> OpenAI.OpenAIRequest
buildOpenAIRequest config userMsg maybeTools = OpenAI.OpenAIRequest
  { OpenAI.orModel = config.oaModel
  , OpenAI.orMaxTokens = config.oaMaxTokens
  , OpenAI.orMessages = systemMsgs ++ [OpenAI.OpenAIMessage "user" (Just userMsg)]
  , OpenAI.orTemperature = config.oaTemperature
  , OpenAI.orTools = maybeTools
  }
  where
    systemMsgs = case config.oaSystemPrompt of
      Nothing -> []
      Just sys -> [OpenAI.OpenAIMessage "system" (Just sys)]


-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Parse a base URL from Text.
--
-- Defaults to HTTPS on port 443 if not specified.
-- Handles edge cases like empty strings, missing ports, malformed URLs.
parseBaseUrl :: Text -> BaseUrl
parseBaseUrl url =
  let -- Strip trailing slash if present
      cleanUrl = case T.stripSuffix "/" url of
        Just t  -> t
        Nothing -> url
      -- Check for scheme
      (scheme, rest) = case T.stripPrefix "https://" cleanUrl of
        Just r -> (Https, T.unpack r)
        Nothing -> case T.stripPrefix "http://" cleanUrl of
          Just r -> (Http, T.unpack r)
          Nothing -> (Https, T.unpack cleanUrl)
      -- Default port based on scheme
      defaultPort = if scheme == Https then 443 else 80
      -- Split host and path
      (hostPort, path) = break (== '/') rest
      -- Split host and port (safely)
      (host, port) = case break (== ':') hostPort of
        (h, ':':p) -> case safeReadPort p of
          Just port' -> (h, port')
          Nothing    -> (h, defaultPort)
        (h, _) -> (h, defaultPort)
      -- Safe port parsing
      safeReadPort :: String -> Maybe Int
      safeReadPort s = case reads s of
        [(n, "")] -> Just n
        _         -> Nothing
  in BaseUrl scheme host port path

-- | Convert servant-client errors to LLMError.
--
-- Note: Error messages are sanitized to avoid leaking credentials.
-- The full exception details (which may contain API keys in request headers)
-- are NOT included in the error message.
clientErrorToLLMError :: ClientError -> LLMError
clientErrorToLLMError err = case err of
  FailureResponse _ resp ->
    let status = statusCode (responseStatusCode resp)
        -- Check response body for context length error (safely)
        bodyText = T.toLower $ TE.decodeUtf8Lenient $ LBS.toStrict $ responseBody resp
        isContextError = "context" `T.isInfixOf` bodyText && "long" `T.isInfixOf` bodyText
    in case status of
      401 -> LLMUnauthorized
      429 -> LLMRateLimited
      529 -> LLMOverloaded
      _ | isContextError -> LLMContextTooLong
        | otherwise -> LLMApiError "http_error" (T.pack $ "Status " <> show status)
  DecodeFailure msg _ ->
    LLMParseError msg
  -- Sanitized error messages - do NOT expose request details which contain API keys
  UnsupportedContentType{} ->
    LLMApiError "unsupported_content_type" "Unsupported content type in response"
  InvalidContentTypeHeader{} ->
    LLMApiError "invalid_content_type_header" "Invalid Content-Type header in response"
  ConnectionError{} ->
    LLMHttpError "Connection error while calling LLM provider"


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
