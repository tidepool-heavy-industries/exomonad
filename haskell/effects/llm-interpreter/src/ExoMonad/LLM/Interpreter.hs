-- | LLM effect interpreter - Anthropic/OpenAI HTTP client.
--
-- Implements LLMComplete effect with servant-client.
-- Type-level provider determines request/response schema.
--
-- = Usage
--
-- @
-- import ExoMonad.LLM.Interpreter (runLLMComplete, LLMEnv, mkLLMEnv)
-- import ExoMonad.LLM.Types (LLMConfig(..), AnthropicSecrets(..))
-- import ExoMonad.Effects.LLMProvider (LLMComplete, complete, SAnthropic, AnthropicConfig(..))
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
module ExoMonad.LLM.Interpreter
  ( -- * Interpreter
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
import Data.Aeson (Value, toJSON, fromJSON)
import qualified Data.Aeson as Aeson
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

import ExoMonad.Effects.SocketClient
  ( SocketConfig(..)
  , ServiceRequest(..)
  , ServiceResponse(..)
  , ServiceError(..)
  , sendRequest
  )

import ExoMonad.LLM.Types
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
import ExoMonad.Effects.LLMProvider
  ( LLMComplete(..)
  , LLMError(..)
  , SProvider(..)
  , AnthropicConfig(..)
  , OpenAIConfig(..)
  , ThinkingBudget(..)
  , AnthropicResponse(..)
  , OpenAIResponse(..)
  , LLMProviderConfig
  , LLMProviderResponse
  )
import ExoMonad.LLM.API.Anthropic qualified as Anthropic
import ExoMonad.LLM.API.OpenAI qualified as OpenAI


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
          req = buildAnthropicRequest config userMsg maybeTools Nothing

      result <- runClientM (Anthropic.anthropicComplete (getApiKey secrets.asApiKey) req) clientEnv
      pure $ either (Left . clientErrorToLLMError) Right result

-- | Build Anthropic Messages API request.
--
-- Tools should be in Anthropic format (with @input_schema@, not @parameters@).
-- Use 'AnthropicTool' or 'ExoMonad.Tool.toolToJSON' which produce the correct format.
--
-- Note: tool_choice 'any' and 'tool' are NOT compatible with extended thinking.
-- If you pass a ToolChoice that forces tool use, thinking will be ignored.
buildAnthropicRequest
  :: AnthropicConfig
  -> Text
  -> Maybe [Value]
  -> Maybe Anthropic.ToolChoice
  -> Anthropic.AnthropicRequest
buildAnthropicRequest config userMsg maybeTools maybeToolChoice = Anthropic.AnthropicRequest
  { Anthropic.arModel = config.acModel
  , Anthropic.arMaxTokens = config.acMaxTokens
  , Anthropic.arMessages = [Anthropic.AnthropicMessage "user" userMsg]
  , Anthropic.arSystem = config.acSystemPrompt
  , Anthropic.arTools = maybeTools
  , Anthropic.arToolChoice = maybeToolChoice
  , Anthropic.arThinking = case (config.acThinking, maybeToolChoice) of
      -- Disable thinking if we're forcing tool use (incompatible)
      (_, Just Anthropic.ToolChoiceAny) -> Nothing
      (_, Just (Anthropic.ToolChoiceTool _)) -> Nothing
      (ThinkingDisabled, _) -> Nothing
      (ThinkingEnabled budget, _) -> Just Anthropic.ThinkingConfig
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
-- Use 'CfTool' from "ExoMonad.Tool.Wire" or the 'ToCfTool' typeclass to produce
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
-- SOCKET CLIENT
-- ════════════════════════════════════════════════════════════════════════════

-- | Make a request to the socket-based service.
socketRequest
  :: SProvider p
  -> LLMEnv
  -> LLMProviderConfig p
  -> Text
  -> Maybe [Value]
  -> IO (Either LLMError (LLMProviderResponse p))
socketRequest provider env config userMsg maybeTools = case env.leConfig of
  LLMHttpConfig{} -> case provider of
    SAnthropic -> anthropicRequest env config userMsg maybeTools
    SOpenAI -> openaiRequest env config userMsg maybeTools
  LLMSocketConfig path -> do
    let socketCfg = SocketConfig path 30000
    case provider of
      SAnthropic -> do
        let req = AnthropicChat
              { model = config.acModel
              , messages = [toJSON $ Anthropic.AnthropicMessage "user" userMsg]
              , maxTokens = config.acMaxTokens
              , tools = maybeTools
              , system = config.acSystemPrompt
              , thinking = case config.acThinking of
                  ThinkingDisabled -> Nothing
                  ThinkingEnabled budget -> Just $ toJSON $ Anthropic.ThinkingConfig "enabled" budget
              }
        result <- sendRequest socketCfg req
        case result of
          Right (AnthropicChatResponse content stop usage) ->
            case (fromJSON (toJSON content), fromJSON (toJSON usage)) of
              (Aeson.Success c, Aeson.Success u) -> pure $ Right $ AnthropicResponse c stop u
              (err, _) -> pure $ Left $ LLMParseError $ T.pack $ "Failed to parse content: " <> show err
          Right (ErrorResponse code msg) -> pure $ Left $ LLMApiError (T.pack $ show code) msg
          Right _ -> pure $ Left $ LLMParseError "Unexpected response type for Anthropic"
          Left err -> pure $ Left $ socketErrorToLLMError err

      SOpenAI -> do
        -- For now, we assume OpenAI via socket uses a similar protocol if needed,
        -- or we fall back/fail if not supported. Task only showed AnthropicChat.
        pure $ Left $ LLMApiError "not_implemented" "OpenAI via socket not yet supported"

-- | Convert SocketError to LLMError.
socketErrorToLLMError :: ServiceError -> LLMError
socketErrorToLLMError = \case
  SocketError msg -> LLMHttpError msg
  DecodeError msg -> LLMParseError (T.pack msg)
  TimeoutError -> LLMHttpError "Socket request timed out"


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT INTERPRETER
-- ════════════════════════════════════════════════════════════════════════════

-- | Helper to get provider name as string.
providerName :: SProvider p -> String
providerName SAnthropic = "Anthropic"
providerName SOpenAI = "OpenAI"

-- | Run LLMComplete effects using native HTTP client or socket.
--
-- This interpreter makes real API calls to Anthropic or OpenAI based on
-- the type-level provider in the effect and the environment configuration.
runLLMComplete :: LastMember IO effs => LLMEnv -> Eff (LLMComplete ': effs) a -> Eff effs a
runLLMComplete env = interpret $ \case
  -- Throwing variants (for when errors are fatal)
  Complete provider config msg tools -> sendM $ do
    result <- socketRequest provider env config msg tools
    case result of
      Left err -> error $ "LLMComplete (" <> providerName provider <> "): " <> show err
      Right resp -> pure resp

  -- Try variants (for graceful error handling)
  CompleteTry provider config msg tools -> sendM $
    socketRequest provider env config msg tools
