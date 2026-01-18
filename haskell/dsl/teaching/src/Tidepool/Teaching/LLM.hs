{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Teaching LLM interpreter that captures full turns for training data.
--
-- This interpreter intercepts 'RunTurnOp' from the LLM effect and:
-- 1. Wraps system prompt with teacher guidance
-- 2. Calls Haiku (via Anthropic API)
-- 3. Records full turn with node context
-- 4. Returns the response to continue normal execution
--
-- = Quick Start
--
-- @
-- import Tidepool.Teaching.LLM
--
-- main :: IO ()
-- main = do
--   -- Check if teaching is enabled via environment
--   mConfig <- loadTeachingConfig
--   case mConfig of
--     Nothing -> runProductionMode
--     Just config -> do
--       let guidance = "Focus on precise tool argument formatting..."
--       withTeaching config guidance $ \\env -> do
--         result <- runM $ runLLMWithTeaching env $ myGraph
--         pure result
-- @
--
-- = Environment Variables
--
-- * @TEACHING_ENABLED=true@ - Enable teaching mode
-- * @TEACHING_OUTPUT_DIR@ - Output dir (default: .tidepool/training)
-- * @ANTHROPIC_API_KEY@ - Required for Haiku calls
--
-- = Data Flow
--
-- @
-- RunTurnOp meta systemPrompt userContent schema tools
--     │
--     ▼
-- runLLMWithTeaching (intercepts)
--     │
--     ├─▶ Wrap system prompt with teacher guidance
--     ├─▶ Call Haiku via Anthropic API
--     ├─▶ Record TeachingTurn (node name, graph name, full request/response)
--     │
--     ▼
-- Return TurnOutcome (normal execution continues)
-- @
--
-- = Output Format
--
-- Training data is written to @\<outputDir\>/session-\<uuid\>/@:
--
-- * @anthropic.jsonl@ - Full TeachingTurn records (one JSON per line)
-- * @gemma.jsonl@ - Reserved for FunctionGemma format conversion
-- * @metadata.json@ - Session configuration and timestamp
module Tidepool.Teaching.LLM
  ( -- * Teaching Interpreter
    runLLMWithTeaching

    -- * Environment Setup
  , initTeachingEnv
  , closeTeachingEnv
  , withTeaching
  , loadTeachingConfig

    -- * Re-exports
  , TeachingEnv(..)
  , TeachingConfig(..)
  , TeachingTurn(..)
  ) where

import Control.Exception (bracket)
import Control.Monad.Freer (Eff, LastMember, interpret, sendM, runM)
import Data.Aeson (Value(..), toJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Data.UUID.V4 (nextRandom)
import System.Environment (lookupEnv)

import Tidepool.Effect.Types
  ( LLM(..)
  , TurnOutcome(..)
  , TurnResult(..)
  , ToolInvocation(..)
  , ContentBlock(..)
  )
import Tidepool.Effect.NodeMeta (NodeMetadata(..))
import Tidepool.Effects.LLMProvider
  ( AnthropicConfig(..)
  , AnthropicResponse(..)
  , ThinkingBudget(..)
  , complete
  , SProvider(SAnthropic)
  )
import qualified Tidepool.Effects.LLMProvider as LP
import Tidepool.LLM.Interpreter (runLLMComplete, mkLLMEnv)
import Tidepool.LLM.Types (LLMConfig(..), AnthropicSecrets(..), BaseUrl(..), ApiKey(..))
import Tidepool.Teaching.Types
  ( TeachingEnv(..)
  , TeachingConfig(..)
  , TeachingTurn(..)
  , RecordingHandles(..)
  )
import Tidepool.Teaching.Record (initRecording, recordTurn, closeRecording, writeMetadata)


-- ════════════════════════════════════════════════════════════════════════════
-- ENVIRONMENT SETUP
-- ════════════════════════════════════════════════════════════════════════════

-- | Initialize a teaching environment.
--
-- Creates session directory, opens recording handles, writes metadata.
-- Should be paired with 'closeTeachingEnv' (use 'bracket' for safety).
initTeachingEnv :: TeachingConfig -> Text -> IO TeachingEnv
initTeachingEnv config guidance = do
  -- Use pattern match to extract fields (NoFieldSelectors)
  let TeachingConfig { tcOutputDir = outputDir, tcSessionId = sessionId } = config
  handles <- initRecording outputDir sessionId
  writeMetadata (rhSessionDir handles) config
  pure TeachingEnv
    { teConfig = config
    , teHandles = handles
    , teGuidance = guidance
    }


-- | Close a teaching environment.
--
-- Flushes and closes recording handles.
closeTeachingEnv :: TeachingEnv -> IO ()
closeTeachingEnv TeachingEnv{..} = closeRecording teHandles


-- | Bracket-style teaching session management.
--
-- Safely initializes and cleans up teaching environment:
--
-- @
-- withTeaching config guidance $ \env -> do
--   result <- runM $ runLLMWithTeaching env $ myGraph
--   pure result
-- @
--
-- Equivalent to:
--
-- @
-- bracket (initTeachingEnv config guidance) closeTeachingEnv $ \env -> ...
-- @
withTeaching :: TeachingConfig -> Text -> (TeachingEnv -> IO a) -> IO a
withTeaching config guidance =
  bracket (initTeachingEnv config guidance) closeTeachingEnv


-- | Load teaching configuration from environment variables.
--
-- Environment variables:
--
-- * @TEACHING_ENABLED@ - Set to \"true\" to enable teaching mode
-- * @TEACHING_OUTPUT_DIR@ - Output directory for training data (default: .tidepool/training)
-- * @ANTHROPIC_API_KEY@ - Required API key for Haiku calls
--
-- Returns 'Nothing' if teaching is not enabled or API key is missing.
--
-- @
-- mConfig <- loadTeachingConfig
-- case mConfig of
--   Nothing -> runProductionLLM ...
--   Just config -> withTeaching config guidance $ \env -> ...
-- @
loadTeachingConfig :: IO (Maybe TeachingConfig)
loadTeachingConfig = do
  enabled <- lookupEnv "TEACHING_ENABLED"
  case enabled of
    Just "true" -> do
      apiKey <- lookupEnv "ANTHROPIC_API_KEY"
      case apiKey of
        Nothing -> do
          putStrLn "[Teaching] TEACHING_ENABLED=true but ANTHROPIC_API_KEY not set"
          pure Nothing
        Just key -> do
          outputDir <- maybe ".tidepool/training" id <$> lookupEnv "TEACHING_OUTPUT_DIR"
          sessionId <- nextRandom
          pure $ Just TeachingConfig
            { tcEnabled = True
            , tcOutputDir = outputDir
            , tcSessionId = sessionId
            , tcAnthropicKey = T.pack key
            }
    _ -> pure Nothing


-- ════════════════════════════════════════════════════════════════════════════
-- TEACHING INTERPRETER
-- ════════════════════════════════════════════════════════════════════════════

-- | Teaching LLM interpreter: calls Haiku and records training data.
--
-- This interpreter replaces the production LLM interpreter when teaching
-- mode is enabled. It:
--
-- 1. **Intercepts** 'RunTurnOp' from the effect stack
-- 2. **Extracts** node metadata (node name, graph name) from the operation
-- 3. **Wraps** the system prompt with teacher guidance
-- 4. **Calls Haiku** via the Anthropic Messages API
-- 5. **Records** the full turn (request + response + metadata)
-- 6. **Returns** the response to continue normal graph execution
--
-- The recorded data is written to:
-- - @anthropic.jsonl@ - Full turn records (JSON per line)
-- - @gemma.jsonl@ - Converted FunctionGemma format (TODO: add conversion)
--
-- = Thread Safety
--
-- The interpreter is NOT thread-safe - each teaching session should use
-- a single thread. For concurrent sessions, create separate TeachingEnv
-- instances.
runLLMWithTeaching
  :: forall effs a.
     LastMember IO effs
  => TeachingEnv
  -> Eff (LLM ': effs) a
  -> Eff effs a
runLLMWithTeaching env = interpret $ \case
  RunTurnOp meta systemPrompt userContent schema tools -> do
    -- Extract node metadata via pattern match (NoFieldSelectors)
    let NodeMetadata nodeName graphName = meta

    sendM $ putStrLn $ "[Teaching] RunTurnOp intercepted for node: "
      <> T.unpack nodeName
      <> " in graph: " <> T.unpack graphName

    -- 1. Wrap system prompt with teacher guidance
    let wrappedPrompt = wrapWithGuidance (teGuidance env) systemPrompt

    -- 2. Build user content as text (for Haiku call)
    let userText = contentBlocksToText userContent

    -- 3. Call Haiku
    sendM $ putStrLn "[Teaching] Calling Haiku..."
    response <- sendM $ callHaiku env wrappedPrompt userText tools

    -- 4. Record the turn
    now <- sendM getCurrentTime
    let turn = TeachingTurn
          { ttNodeName = nodeName
          , ttGraphName = graphName
          , ttSystemPrompt = systemPrompt  -- Original, not wrapped
          , ttUserContent = toJSON userContent
          , ttOutputSchema = schema
          , ttToolDefs = tools
          , ttResponse = toJSON response
          , ttTimestamp = now
          }
    sendM $ recordTurn (teHandles env) turn
    sendM $ putStrLn $ "[Teaching] Recorded turn for: " <> T.unpack nodeName

    -- 5. Convert response to TurnOutcome
    pure $ convertToOutcome response


-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Wrap system prompt with teacher guidance.
wrapWithGuidance :: Text -> Text -> Text
wrapWithGuidance guidance original =
  if T.null guidance
    then original
    else T.unlines
      [ "# Teaching Mode Active"
      , ""
      , guidance
      , ""
      , "---"
      , ""
      , original
      ]


-- | Convert content blocks to plain text for Haiku call.
--
-- Currently just extracts text blocks. Tool results and images
-- would need additional handling.
-- Uses ContentBlock from Effect.Types (re-exported from Anthropic.Types)
contentBlocksToText :: [ContentBlock] -> Text
contentBlocksToText blocks = T.intercalate "\n" $ concatMap extractText blocks
  where
    extractText :: ContentBlock -> [Text]
    extractText (TextBlock t) = [t]
    extractText (ToolResultBlock tr) = [T.pack $ show tr]
    extractText (ImageBlock _) = ["[image]"]
    extractText (ToolUseBlock tu) = [T.pack $ show tu]
    extractText (ThinkingBlock tc) = [T.pack $ show tc]
    extractText (RedactedThinkingBlock _) = ["[redacted thinking]"]
    extractText (JsonBlock v) = [T.pack $ show v]


-- | Call Haiku via Anthropic API.
--
-- Uses the existing LLM infrastructure to make the API call.
callHaiku :: TeachingEnv -> Text -> Text -> [Value] -> IO AnthropicResponse
callHaiku TeachingEnv{..} systemPrompt userText tools = do
  -- Extract API key via pattern match (NoFieldSelectors)
  let TeachingConfig { tcAnthropicKey = apiKey } = teConfig

  let llmConfig = LLMConfig
        { lcAnthropicSecrets = Just $ AnthropicSecrets
            { asApiKey = ApiKey apiKey
            , asBaseUrl = BaseUrl "https://api.anthropic.com"
            }
        , lcOpenAISecrets = Nothing
        }

  let anthropicCfg = AnthropicConfig
        { acModel = "claude-3-5-haiku-20241022"
        , acMaxTokens = 4096
        , acThinking = ThinkingEnabled 1024  -- Enable thinking for quality training data
        , acSystemPrompt = Just systemPrompt
        }

  -- Make the actual API call
  llmEnv <- mkLLMEnv llmConfig
  runM $ runLLMComplete llmEnv $
    complete SAnthropic anthropicCfg userText (if null tools then Nothing else Just tools)


-- | Convert Anthropic response to TurnOutcome.
--
-- Maps the Anthropic response structure to our internal TurnOutcome type.
-- Currently produces a simple success outcome; tool use handling would
-- need extension.
convertToOutcome :: AnthropicResponse -> TurnOutcome (TurnResult Value)
convertToOutcome (AnthropicResponse contentBlocks _usage _stopReason) =
  -- Extract text and tool use from content blocks (LP = LLMProvider ContentBlock)
  let textContent = T.intercalate "\n" [t | LP.TextContent t <- contentBlocks]
      toolUses = [(name, input_) | LP.ToolUseContent name input_ <- contentBlocks]
  in case toolUses of
    [] ->
      -- No tool use - return text as structured output
      TurnCompleted $ TurnResult
        { trOutput = toJSON textContent  -- Parsed output
        , trToolsInvoked = []
        , trNarrative = textContent
        , trThinking = ""
        }
    [(toolName, toolArgs)] ->
      -- Single tool use - record as tool invocation
      TurnCompleted $ TurnResult
        { trOutput = toolArgs  -- Tool args are the structured output
        , trToolsInvoked = [ToolInvocation
            { tiName = toolName
            , tiInput = toolArgs
            , tiOutput = Null  -- Not executed yet
            }]
        , trNarrative = textContent
        , trThinking = ""
        }
    multiTools ->
      -- Multiple tool uses - record all invocations
      TurnCompleted $ TurnResult
        { trOutput = toJSON $ map snd multiTools
        , trToolsInvoked = [ToolInvocation
            { tiName = name
            , tiInput = args
            , tiOutput = Null  -- Not executed yet
            } | (name, args) <- multiTools]
        , trNarrative = textContent
        , trThinking = ""
        }
