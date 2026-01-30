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
  ( -- * Interpreter
    runLLMCall
  , runLLMCallWith

    -- * Configuration
  , InterpretConfig(..)
  , defaultInterpretConfig
  ) where

import Control.Monad.Freer (Eff, Member, interpret, LastMember)
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import ExoMonad.LLM.Types
import ExoMonad.LLM.Effect (LLMCall(..))
import ExoMonad.Effects.LLMProvider
  ( LLMComplete
  , completeTry
  , SProvider(SAnthropic)
  , AnthropicConfig(..)
  , AnthropicResponse(..)
  , ThinkingBudget(..)
  , ContentBlock(..)
  , LLMError(..)
  )
import ExoMonad.StructuredOutput (StructuredOutput(..), formatDiagnostic)


-- ════════════════════════════════════════════════════════════════════════════
-- CONFIGURATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Configuration for the interpreter.
data InterpretConfig = InterpretConfig
  { icMaxNudges :: Int
    -- ^ Maximum nudge retries for schema conformance (default: 3)
  , icMaxToolLoops :: Int
    -- ^ Maximum tool use loop iterations (default: 10)
  , icDefaultMaxTokens :: Int
    -- ^ Default max tokens if not specified in config (default: 4096)
  }
  deriving stock (Show, Eq)

-- | Default interpreter configuration.
defaultInterpretConfig :: InterpretConfig
defaultInterpretConfig = InterpretConfig
  { icMaxNudges = 3
  , icMaxToolLoops = 10
  , icDefaultMaxTokens = 4096
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
-- @
-- runM
--   $ runLLMComplete env
--   $ runLLMCall
--   $ yourEffectfulCode
-- @
runLLMCall
  :: (Member LLMComplete es, LastMember IO es)
  => Eff (LLMCall ': es) a
  -> Eff es a
runLLMCall = runLLMCallWith defaultInterpretConfig

-- | Run the LLMCall effect with custom configuration.
runLLMCallWith
  :: (Member LLMComplete es, LastMember IO es)
  => InterpretConfig
  -> Eff (LLMCall ': es) a
  -> Eff es a
runLLMCallWith interpCfg = interpret $ \case

  CallLLMNoTools mdl maxTok _temp (System sys) (User usr) _schema -> do
    let anthropicCfg = AnthropicConfig
          { acModel = modelToText mdl
          , acMaxTokens = fromMaybe interpCfg.icDefaultMaxTokens maxTok
          , acThinking = ThinkingDisabled
          , acSystemPrompt = Just sys
          }
    runWithNudge interpCfg.icMaxNudges anthropicCfg usr Nothing

  CallLLMWithTools mdl maxTok _temp (System sys) (User usr) _schema toolSchemas -> do
    let anthropicCfg = AnthropicConfig
          { acModel = modelToText mdl
          , acMaxTokens = fromMaybe interpCfg.icDefaultMaxTokens maxTok
          , acThinking = ThinkingDisabled
          , acSystemPrompt = Just sys
          }
    runWithNudge interpCfg.icMaxNudges anthropicCfg usr (Just toolSchemas)


-- ════════════════════════════════════════════════════════════════════════════
-- INTERNAL: NUDGE LOOP
-- ════════════════════════════════════════════════════════════════════════════

-- | Run with nudge retries for schema conformance.
runWithNudge
  :: forall out es.
     ( Member LLMComplete es
     , LastMember IO es
     , StructuredOutput out
     )
  => Int                  -- ^ Remaining nudges
  -> AnthropicConfig      -- ^ API config
  -> Text                 -- ^ User message
  -> Maybe [Value]        -- ^ Tool schemas (if any)
  -> Eff es (Either CallError out)
runWithNudge nudgesLeft cfg userMsg toolSchemas = do
  result <- completeTry SAnthropic cfg userMsg toolSchemas
  case result of
    Left err -> pure $ Left $ llmErrorToCallError err

    Right response -> do
      -- Extract text content from response
      let textContent = extractTextContent response.arContent

      -- Try to parse as JSON and then as structured output
      case Aeson.eitherDecodeStrict (TE.encodeUtf8 textContent) of
        Left jsonErr ->
          if nudgesLeft > 0
          then nudgeAndRetry (nudgesLeft - 1) cfg userMsg toolSchemas
                 ("JSON parse error: " <> T.pack jsonErr)
          else pure $ Left $ CallParseFailed $ "JSON parse error: " <> T.pack jsonErr

        Right jsonVal ->
          case parseStructured jsonVal of
            Right out -> pure $ Right out
            Left diag ->
              if nudgesLeft > 0
              then nudgeAndRetry (nudgesLeft - 1) cfg userMsg toolSchemas
                     (formatDiagnostic diag)
              else pure $ Left $ CallParseFailed $ formatDiagnostic diag

-- | Send a nudge message and retry.
nudgeAndRetry
  :: forall out es.
     ( Member LLMComplete es
     , LastMember IO es
     , StructuredOutput out
     )
  => Int                  -- ^ Remaining nudges
  -> AnthropicConfig      -- ^ API config
  -> Text                 -- ^ Original user message
  -> Maybe [Value]        -- ^ Tool schemas
  -> Text                 -- ^ Error message from previous attempt
  -> Eff es (Either CallError out)
nudgeAndRetry nudgesLeft cfg origMsg toolSchemas errMsg =
  let nudgeMsg = T.unlines
        [ "Your previous response didn't match the required JSON schema."
        , ""
        , "Error: " <> errMsg
        , ""
        , "Please respond again with valid JSON matching the schema."
        , "Original request: " <> origMsg
        ]
  in runWithNudge nudgesLeft cfg nudgeMsg toolSchemas


-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Extract text content from response blocks.
extractTextContent :: [ContentBlock] -> Text
extractTextContent blocks = T.intercalate "\n" $ concatMap extractText blocks
  where
    extractText (TextContent t) = [t]
    extractText _ = []

-- | Convert LLMError to CallError.
llmErrorToCallError :: LLMError -> CallError
llmErrorToCallError = \case
  LLMHttpError msg -> CallNetworkError msg
  LLMParseError msg -> CallParseFailed msg
  LLMRateLimited -> CallRateLimited
  LLMUnauthorized -> CallUnauthorized
  LLMContextTooLong -> CallContextTooLong
  LLMOverloaded -> CallRateLimited  -- Treat overloaded as rate limit
  LLMApiError typ msg -> CallApiError typ msg
  LLMNoProviderConfigured -> CallUnauthorized
