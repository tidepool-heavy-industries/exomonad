-- | LLM effect interpreter - Anthropic client.
--
-- Implements LLMComplete effect via Unix Socket only.
-- Type-level provider determines request/response schema.
--
-- = Usage
--
-- @
-- import ExoMonad.LLM.Interpreter (runLLMComplete, LLMEnv, mkLLMEnv)
-- import ExoMonad.LLM.Types (LLMConfig(..), AnthropicSecrets(..))
-- import ExoMonad.Effects.LLMProvider (LLMComplete, complete, SAnthropic, AnthropicConfig(..))
--
-- config = LLMSocketConfig ".exomonad/sockets/service.sock"
--
-- main = do
--   env <- mkLLMEnv config
--   runM $ runLLMComplete env $ do
--     response <- complete SAnthropic anthropicCfg "Hello" Nothing
--     -- process response
-- @
module ExoMonad.LLM.Interpreter
  ( -- * Interpreter
    runLLMComplete,

    -- * Re-exports for convenience
    LLMEnv,
    LLMConfig (..),
    mkLLMEnv,

    -- * Tool Types
    AnthropicTool (..),
    anthropicToolToJSON,

    -- * Request Building (exported for testing)
    buildAnthropicRequest,

    -- * Helpers (exported for testing)
    parseBaseUrl,
  )
where

import Data.Aeson (Value, fromJSON, toJSON)
import Data.Aeson qualified as Aeson
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Effects.LLMProvider
  ( AnthropicConfig (..),
    AnthropicResponse (..),
    LLMComplete (..),
    LLMError (..),
    LLMProviderConfig,
    LLMProviderResponse,
    Message (..),
    Role (..),
    SProvider (..),
    ThinkingBudget (..),
  )
import ExoMonad.Effects.SocketClient
  ( ServiceError (..),
    ServiceRequest (..),
    ServiceResponse (..),
    SocketConfig (..),
    sendRequest,
  )
import ExoMonad.LLM.Interpreter.Types
import Polysemy (Member, Sem, embed, interpret)
import Polysemy.Embed (Embed)
import PyF (fmt)

-- ════════════════════════════════════════════════════════════════════════════
-- ANTHROPIC CLIENT
-- ════════════════════════════════════════════════════════════════════════════

-- | Convert a Message to AnthropicMessage wire format.
messageToAnthropicMessage :: Message -> AnthropicMessage
messageToAnthropicMessage msg =
  let roleText = case msg.msgRole of
        User -> "user"
        Assistant -> "assistant"
      -- Convert ContentBlocks to JSON array
      contentJson = toJSON msg.msgContent
   in AnthropicMessage roleText contentJson

-- | Build Anthropic Messages API request from single user message.
--
-- Tools should be in Anthropic format (with @input_schema@, not @parameters@).
-- Use 'AnthropicTool' or 'ExoMonad.Tool.toolToJSON' which produce the correct format.
--
-- Note: tool_choice 'any' and 'tool' are NOT compatible with extended thinking.
-- If you pass a ToolChoice that forces tool use, thinking will be ignored.
buildAnthropicRequest ::
  AnthropicConfig ->
  Text ->
  Maybe [Value] ->
  Maybe ToolChoice ->
  AnthropicRequest
buildAnthropicRequest config userMsg maybeTools maybeToolChoice =
  AnthropicRequest
    { arModel = config.acModel,
      arMaxTokens = config.acMaxTokens,
      arMessages = AnthropicMessage "user" (toJSON userMsg) :| [],
      arSystem = config.acSystemPrompt,
      arTools = maybeTools,
      arToolChoice = maybeToolChoice,
      arThinking = case (config.acThinking, maybeToolChoice) of
        -- Disable thinking if we're forcing tool use (incompatible)
        (_, Just ToolChoiceAny) -> Nothing
        (_, Just (ToolChoiceTool _)) -> Nothing
        (ThinkingDisabled, _) -> Nothing
        (ThinkingEnabled budget, _) ->
          Just
            ThinkingConfig
              { tcType = "enabled",
                tcBudgetTokens = budget
              }
    }

-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Parse a base URL from Text.
--
-- Defaults to HTTPS on port 443 if not specified.
-- Handles edge cases like empty strings, missing ports, malformed URLs.
parseBaseUrl :: Text -> ParsedBaseUrl
parseBaseUrl url =
  let -- Strip trailing slash if present
      cleanUrl = case T.stripSuffix "/" url of
        Just t -> t
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
        (h, ':' : p) -> case safeReadPort p of
          Just port' -> (h, port')
          Nothing -> (h, defaultPort)
        (h, _) -> (h, defaultPort)
      -- Safe port parsing
      safeReadPort :: String -> Maybe Int
      safeReadPort s = case reads s of
        [(n, "")] -> Just n
        _ -> Nothing
   in ParsedBaseUrl scheme host port path

-- ════════════════════════════════════════════════════════════════════════════
-- SOCKET CLIENT
-- ════════════════════════════════════════════════════════════════════════════

-- | Make a request to the socket-based service with a single user message.
socketRequest ::
  SProvider p ->
  LLMEnv ->
  LLMProviderConfig p ->
  Text ->
  Maybe [Value] ->
  IO (Either LLMError (LLMProviderResponse p))
socketRequest provider env config userMsg maybeTools = case env.leConfig of
  LLMSocketConfig path -> do
    let socketCfg = SocketConfig path 30000
    case provider of
      SAnthropic -> do
        let req =
              AnthropicChat
                { model = config.acModel,
                  messages = toJSON (AnthropicMessage "user" (toJSON userMsg)) :| [],
                  maxTokens = config.acMaxTokens,
                  tools = maybeTools,
                  system = config.acSystemPrompt,
                  thinking = case config.acThinking of
                    ThinkingDisabled -> Nothing
                    ThinkingEnabled budget -> Just $ toJSON $ ThinkingConfig "enabled" budget
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

-- | Make a request to the socket-based service with conversation history.
socketRequestConversation ::
  SProvider p ->
  LLMEnv ->
  LLMProviderConfig p ->
  [Message] ->
  Maybe [Value] ->
  IO (Either LLMError (LLMProviderResponse p))
socketRequestConversation provider env config messages maybeTools = case env.leConfig of
  LLMSocketConfig path -> do
    let socketCfg = SocketConfig path 30000
    case provider of
      SAnthropic -> do
        let anthropicMessages = map messageToAnthropicMessage messages
        let messagesNE = case map toJSON anthropicMessages of
              [] -> toJSON (AnthropicMessage "user" (toJSON ("" :: Text))) :| [] -- shouldn't happen
              (m : ms) -> m :| ms
        let req =
              AnthropicChat
                { model = config.acModel,
                  messages = messagesNE,
                  maxTokens = config.acMaxTokens,
                  tools = maybeTools,
                  system = config.acSystemPrompt,
                  thinking = case config.acThinking of
                    ThinkingDisabled -> Nothing
                    ThinkingEnabled budget -> Just $ toJSON $ ThinkingConfig "enabled" budget
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

-- | Run LLMComplete effects using socket.
--
-- This interpreter makes real API calls to Anthropic (via service socket)
-- based on the type-level provider in the effect and the environment configuration.
runLLMComplete :: (Member (Embed IO) r) => LLMEnv -> Sem (LLMComplete p ': r) a -> Sem r a
runLLMComplete env = interpret $ \case
  -- Throwing variants (for when errors are fatal)
  Complete provider config msg tools -> embed $ do
    result <- socketRequest provider env config msg tools
    case result of
      Left err -> error $ "LLMComplete (" <> providerName provider <> "): " <> show err
      Right resp -> pure resp

  -- Try variants (for graceful error handling)
  CompleteTry provider config msg tools ->
    embed $
      socketRequest provider env config msg tools
  -- Conversation variants (multi-turn)
  CompleteConversation provider config messages tools -> embed $ do
    result <- socketRequestConversation provider env config messages tools
    case result of
      Left err -> error $ "LLMCompleteConversation (" <> providerName provider <> "): " <> show err
      Right resp -> pure resp
  CompleteConversationTry provider config messages tools ->
    embed $
      socketRequestConversation provider env config messages tools
