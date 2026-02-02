{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Interpreter for the LLMCall effect.
--
-- This module provides the interpreter that handles LLM API calls,
-- including:
--
-- * Building API requests from config and prompts
-- * Parsing structured output from responses
-- * Nudge loops for schema conformance retries
-- * Tool use loops for multi-turn tool calls (when tools provided)
--
-- = Architecture
--
-- @
-- ┌─────────────────────────────────────────────────────────────────────────┐
-- │ LLMCall effect (from Effect.hs)                                         │
-- │   callNoTools cfg (System sys) (User usr)                               │
-- └───────────────────────────────────────────┬─────────────────────────────┘
--                                             │ interpret
--                                             ▼
-- ┌─────────────────────────────────────────────────────────────────────────┐
-- │ runLLMCall interpreter (this module)                                    │
-- │   • Builds Anthropic request                                            │
-- │   • Handles nudge loop (schema retries)                                 │
-- │   • Handles tool loop (multi-turn)                                      │
-- └───────────────────────────────────────────┬─────────────────────────────┘
--                                             │ LLMComplete effect
--                                             ▼
-- ┌─────────────────────────────────────────────────────────────────────────┐
-- │ LLMComplete interpreter (llm-interpreter package)                       │
-- │   • Makes HTTP call to Anthropic API                                    │
-- │   • Returns AnthropicResponse                                           │
-- └─────────────────────────────────────────────────────────────────────────┘
-- @
--
-- = Usage
--
-- @
-- import ExoMonad.LLM.Interpret (runLLMCall)
-- import ExoMonad.LLM.Interpreter (runLLMComplete, mkLLMEnv)
--
-- main = do
--   env <- mkLLMEnv config
--   runM
--     $ runLLMComplete env
--     $ runLLMCall
--     $ do
--       let cfg = defaultLLM \@Report & model Sonnet
--       callNoTools cfg (System "Analyze this") (User content)
-- @
module ExoMonad.LLM.Interpret
  ( -- * Interpreters
    runLLMCall,
    runLLMCallWith,
    runLLMCallWithTools,

    -- * Configuration
    InterpretConfig (..),
    defaultInterpretConfig,
  )
where

import Polysemy (Sem, Member, interpret, embed)
import Polysemy.Error (Error, throw, catch)
import Polysemy.Embed (Embed)
import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import ExoMonad.Effects.LLMProvider
  ( AnthropicConfig (..),
    AnthropicResponse (..),
    ContentBlock (..),
    LLMComplete,
    LLMError (..),
    SProvider (SAnthropic),
    ThinkingBudget (..),
    complete,
  )
import ExoMonad.LLM.Effect (LLMCall (..))
import ExoMonad.LLM.Tools (ToolDispatchError (..), ToolRecord (dispatchTool))
import ExoMonad.LLM.Types
import ExoMonad.StructuredOutput (StructuredOutput (..), formatDiagnostic)
import ExoMonad.Prelude (LastMember)

-- ════════════════════════════════════════════════════════════════════════════
-- CONFIGURATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Configuration for the interpreter.
data InterpretConfig = InterpretConfig
  { -- | Maximum nudge retries for schema conformance (default: 3)
    icMaxNudges :: Int,
    -- | Maximum tool use loop iterations (default: 10)
    icMaxToolLoops :: Int,
    -- | Default max tokens if not specified in config (default: 4096)
    icDefaultMaxTokens :: Int
  }
  deriving stock (Show, Eq)

-- | Default interpreter configuration.
defaultInterpretConfig :: InterpretConfig
defaultInterpretConfig =
  InterpretConfig
    { icMaxNudges = 3,
      icMaxToolLoops = 10,
      icDefaultMaxTokens = 4096
    }

-- ════════════════════════════════════════════════════════════════════════════
-- INTERPRETER
-- ════════════════════════════════════════════════════════════════════════════

-- | Run the LLMCall effect.
--
-- This interpreter requires 'LLMComplete' in the effect stack to make
-- actual API calls. It handles:
--
-- * Converting config to Anthropic request format
-- * Parsing structured output from responses
-- * Nudge retries when output doesn't match schema
--
-- Throws 'CallError' via Polysemy Error effect.
runLLMCall ::
  (Member LLMComplete es, Member (Error LLMError) es, Member (Error CallError) es, Member (Embed IO) es) =>
  Sem (LLMCall ': es) a ->
  Sem es a
runLLMCall = runLLMCallWith defaultInterpretConfig

-- | Run the LLMCall effect with custom configuration.
runLLMCallWith ::
  (Member LLMComplete es, Member (Error LLMError) es, Member (Error CallError) es, Member (Embed IO) es) =>
  InterpretConfig ->
  Sem (LLMCall ': es) a ->
  Sem es a
runLLMCallWith interpCfg = interpret $ \case
  PerformLLMCall mdl maxTok (System sys) (User usr) _schema -> do
    let anthropicCfg =
          AnthropicConfig
            { acModel = modelToText mdl,
              acMaxTokens = fromMaybe interpCfg.icDefaultMaxTokens maxTok,
              acThinking = ThinkingDisabled,
              acSystemPrompt = Just sys
            }
    runWithNudge interpCfg.icMaxNudges anthropicCfg usr Nothing
  PerformLLMCallWithTools mdl maxTok (System sys) (User usr) _schema toolSchemas -> do
    -- Note: runLLMCall without tool record just ignores tool_use responses.
    -- Use runLLMCallWithTools to properly handle tool execution.
    let anthropicCfg =
          AnthropicConfig
            { acModel = modelToText mdl,
              acMaxTokens = fromMaybe interpCfg.icDefaultMaxTokens maxTok,
              acThinking = ThinkingDisabled,
              acSystemPrompt = Just sys
            }
    runWithNudge interpCfg.icMaxNudges anthropicCfg usr (Just toolSchemas)

-- | Run the LLMCall effect with tool support.
runLLMCallWithTools ::
  forall tools es a.
  ( Member LLMComplete es,
    Member (Error LLMError) es,
    Member (Error CallError) es,
    Member (Embed IO) es,
    ToolRecord tools
  ) =>
  tools es ->
  Sem (LLMCall ': es) a ->
  Sem es a
runLLMCallWithTools toolRecord = interpret $ \case
  PerformLLMCall mdl maxTok (System sys) (User usr) _schema -> do
    let cfg =
          AnthropicConfig
            { acModel = modelToText mdl,
              acMaxTokens = fromMaybe defaultInterpretConfig.icDefaultMaxTokens maxTok,
              acThinking = ThinkingDisabled,
              acSystemPrompt = Just sys
            }
    runWithNudge defaultInterpretConfig.icMaxNudges cfg usr Nothing
  PerformLLMCallWithTools mdl maxTok (System sys) (User usr) _schema toolSchemas -> do
    let cfg =
          AnthropicConfig
            { acModel = modelToText mdl,
              acMaxTokens = fromMaybe defaultInterpretConfig.icDefaultMaxTokens maxTok,
              acThinking = ThinkingDisabled,
              acSystemPrompt = Just sys
            }
    runToolLoop
      defaultInterpretConfig.icMaxToolLoops
      defaultInterpretConfig.icMaxNudges
      cfg
      usr
      toolSchemas
      toolRecord

-- ════════════════════════════════════════════════════════════════════════════
-- INTERNAL: TOOL USE LOOP
-- ════════════════════════════════════════════════════════════════════════════

-- | Run with tool loop support.
runToolLoop ::
  forall tools out es.
  ( Member LLMComplete es,
    Member (Error LLMError) es,
    Member (Error CallError) es,
    Member (Embed IO) es,
    StructuredOutput out,
    ToolRecord tools
  ) =>
  -- | Remaining tool loop iterations
  Int ->
  -- | Max nudges for final response
  Int ->
  -- | API config
  AnthropicConfig ->
  -- | User message
  Text ->
  -- | Tool schemas
  [Value] ->
  -- | Tool record for dispatch
  tools es ->
  Sem es out
runToolLoop loopsLeft nudgesLeft cfg userMsg toolSchemas tools
  | loopsLeft <= 0 = throw $ CallMaxToolLoops defaultInterpretConfig.icMaxToolLoops
  | otherwise = do
      response <- catch (complete SAnthropic cfg userMsg (Just toolSchemas)) (throw . llmErrorToCallError)
      -- Check if this is a tool_use response
      case extractToolUse response.arContent of
        -- No tool use - try to parse as structured output
        [] -> parseResponse nudgesLeft cfg userMsg (Just toolSchemas) response
        -- Tool use requested - execute and continue loop
        toolCalls -> do
          results <- executeToolCalls tools toolCalls
          -- Format tool results as follow-up message
          let followUp = formatToolResults results userMsg
          runToolLoop (loopsLeft - 1) nudgesLeft cfg followUp toolSchemas tools

-- | Extract tool uses from content blocks.
extractToolUse :: NonEmpty ContentBlock -> [(Text, Value)]
extractToolUse blocks = mapMaybe toToolInvocation (NE.toList blocks)
  where
    toToolInvocation :: ContentBlock -> Maybe (Text, Value)
    toToolInvocation (ToolUseContent _toolUseId name input_) = Just (name, input_)
    toToolInvocation _ = Nothing

-- | Execute tool calls and collect results.
executeToolCalls ::
  forall tools es.
  (ToolRecord tools, Member (Error CallError) es) =>
  tools es ->
  [(Text, Value)] ->
  Sem es [(Text, Either ToolDispatchError Value)]
executeToolCalls tools calls = do
  results <- traverse executeOne calls
  -- Check if any tool dispatch failed fatally
  let fatalErrors = [(n, e) | (n, Left e@(ToolNotFound _)) <- results]
  case fatalErrors of
    ((name, ToolNotFound _) : _) ->
      throw $ CallToolError name "Tool not found"
    _ -> pure results
  where
    executeOne (name, input_) = do
      result <- dispatchTool tools name input_
      pure (name, result)

-- | Format tool results as a follow-up user message.
formatToolResults :: [(Text, Either ToolDispatchError Value)] -> Text -> Text
formatToolResults results originalMsg =
  T.unlines
    [ originalMsg,
      "Tool results:",
      T.unlines $ map formatOne results,
      "Please provide your final response based on these tool results."
    ]
  where
    formatOne (name, Right val) = "- " <> name <> ": " <> T.pack (show val)
    formatOne (name, Left (ToolInputParseError _ err)) = "- " <> name <> " (error): Failed to parse input - " <> err
    formatOne (name, Left (ToolNotFound _)) = "- " <> name <> " (error): Tool not found"

-- | Parse response as structured output with nudge retries.
parseResponse ::
  forall out es.
  ( Member LLMComplete es,
    Member (Error LLMError) es,
    Member (Error CallError) es,
    Member (Embed IO) es,
    StructuredOutput out
  ) =>
  Int ->
  AnthropicConfig ->
  Text ->
  Maybe [Value] ->
  AnthropicResponse ->
  Sem es out
parseResponse nudgesLeft cfg userMsg toolSchemas response = do
  let textContent = extractTextContent response.arContent

  case Aeson.eitherDecodeStrict (TE.encodeUtf8 textContent) of
    Left jsonErr ->
      if nudgesLeft > 0
        then
          nudgeAndRetry
            (nudgesLeft - 1)
            cfg
            userMsg
            toolSchemas
            ("JSON parse error: " <> T.pack jsonErr)
        else throw $ CallParseFailed $ "JSON parse error: " <> T.pack jsonErr
    Right jsonVal ->
      case parseStructured jsonVal of
        Right out -> pure out
        Left diag ->
          if nudgesLeft > 0
            then
              nudgeAndRetry
                (nudgesLeft - 1)
                cfg
                userMsg
                toolSchemas
                (formatDiagnostic diag)
            else throw $ CallParseFailed $ formatDiagnostic diag

-- ════════════════════════════════════════════════════════════════════════════
-- INTERNAL: NUDGE LOOP
-- ════════════════════════════════════════════════════════════════════════════

-- | Run with nudge retries for schema conformance.
runWithNudge ::
  forall out es.
  ( Member LLMComplete es,
    Member (Error LLMError) es,
    Member (Error CallError) es,
    Member (Embed IO) es,
    StructuredOutput out
  ) =>
  -- | Remaining nudges
  Int ->
  -- | API config
  AnthropicConfig ->
  -- | Original user message
  Text ->
  -- | Tool schemas (if any)
  Maybe [Value] ->
  Sem es out
runWithNudge nudgesLeft cfg userMsg toolSchemas = do
  response <- catch (complete SAnthropic cfg userMsg toolSchemas) (throw . llmErrorToCallError)
  -- Extract text content from response
  let textContent = extractTextContent response.arContent

  -- Try to parse as JSON and then as structured output
  case Aeson.eitherDecodeStrict (TE.encodeUtf8 textContent) of
    Left jsonErr ->
      if nudgesLeft > 0
        then
          nudgeAndRetry
            (nudgesLeft - 1)
            cfg
            userMsg
            toolSchemas
            ("JSON parse error: " <> T.pack jsonErr)
        else throw $ CallParseFailed $ "JSON parse error: " <> T.pack jsonErr
    Right jsonVal ->
      case parseStructured jsonVal of
        Right out -> pure out
        Left diag ->
          if nudgesLeft > 0
            then
              nudgeAndRetry
                (nudgesLeft - 1)
                cfg
                userMsg
                toolSchemas
                (formatDiagnostic diag)
            else throw $ CallParseFailed $ formatDiagnostic diag

-- | Send a nudge message and retry.
nudgeAndRetry ::
  forall out es.
  ( Member LLMComplete es,
    Member (Error LLMError) es,
    Member (Error CallError) es,
    Member (Embed IO) es,
    StructuredOutput out
  ) =>
  -- | Remaining nudges
  Int ->
  -- | API config
  AnthropicConfig ->
  -- | Original user message
  Text ->
  -- | Tool schemas
  Maybe [Value] ->
  -- | Error message from previous attempt
  Text ->
  Sem es out
nudgeAndRetry nudgesLeft cfg origMsg toolSchemas errMsg =
  let nudgeMsg =
        T.unlines
          [ "Your previous response didn't match the required JSON schema.",
            "Error: " <> errMsg,
            "Please respond again with valid JSON matching the schema.",
            "Original request: " <> origMsg
          ]
   in runWithNudge nudgesLeft cfg nudgeMsg toolSchemas

-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

extractTextContent :: NonEmpty ContentBlock -> Text
extractTextContent blocks = T.intercalate "\n" $ mapMaybe toText (NE.toList blocks)
  where
    toText :: ContentBlock -> Maybe Text
    toText (TextContent t) = Just t
    toText _ = Nothing

-- | Convert LLMError to CallError.
llmErrorToCallError :: LLMError -> CallError
llmErrorToCallError = \case
  LLMHttpError msg -> CallNetworkError msg
  LLMParseError msg -> CallParseFailed msg
  LLMRateLimited -> CallRateLimited
  LLMUnauthorized -> CallUnauthorized
  LLMContextTooLong -> CallContextTooLong
  LLMOverloaded -> CallRateLimited -- Treat overloaded as rate limit
  LLMApiError typ msg -> CallApiError typ msg
  LLMNoProviderConfigured -> CallUnauthorized

