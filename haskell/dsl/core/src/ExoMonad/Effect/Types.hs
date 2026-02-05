{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Core effect types for ExoMonad - pure definitions without IO runners
--
-- This module contains the effect type definitions and pure operations
-- that are WASM-compatible. For IO-based runners, see 'ExoMonad.Effect'
-- in exomonad-platform.
module ExoMonad.Effect.Types
  ( -- * Core Effects
    State,
    Random (..),
    Time (..),
    LLM (..),
    ChatHistory (..),
    Emit (..),
    RequestInput (..), -- Export constructors for makeSem
    Log (..),
    LogLevel (..),
    DecisionLog (..),
    TUI (..),

    -- * Return Effect
    Return (..),
    returnValue,
    runReturn,

    -- * Effect Operations
    get,
    gets,
    put,
    modify,
    randomInt,
    randomDouble,
    getCurrentTime,
    emitEvent,
    requestChoice,
    requestText,
    requestTextWithPhoto,
    requestCustom,
    logMsg,
    logMsgWith,
    logTrace,
    logTraceWith,
    logDebug,
    logDebugWith,
    logInfo,
    logInfoWith,
    logWarn,
    logWarnWith,
    logError,
    logErrorWith,
    LogFields,
    getHistory,
    appendMessages,
    clearHistory,
    estimateTokens,
    estimateMessageChars,
    estimateBlockChars,

    -- * LLM Operations
    runTurn,
    runTurnContent,
    llmCall,
    llmCallEither,
    llmCallEitherWithTools,
    llmCallStructured,
    llmCallStructuredWithTools,
    withImages,

    -- * LLM Error Types
    CallError (..),

    -- * Result Types
    TurnResult (..),
    TurnParseResult (..),
    ToolInvocation (..),
    ToolResult (..),
    LLMConfig (..),
    LLMHooks (..),
    noHooks,
    InputHandler (..),

    -- * Tool Dispatcher Type
    ToolDispatcher,

    -- * Tool Validation (Parse Don't Validate)
    ValidatedToolInput (..),
    ToolError (..),
    toolErrorToText,

    -- * Simple Runners (pure/IO)
    runState,
    runRandom,
    runTime,
    runEmit,
    runLog,
    runChatHistory,
    runRequestInput,
    runDecisionLogPure,

    -- * TUI Effect Operations
    showUI,

    -- * Decision Types
    Decision (..),
    DecisionContext (..),
    DecisionTrace (..),
    recordDecision,

    -- * TUI Types (popup-tui protocol)

    -- Note: Import ExoMonad.Effect.TUI directly for full component types
    PopupDefinition (..),
    PopupResult (..),

    -- * Content Types (re-exports from Anthropic.Types)
    ContentBlock (..),
    ImageSource (..),
    Message (..),
    Role (..),
    ThinkingContent (..),
    RedactedThinking (..),

    -- * Polysemy Compatibility
    Eff,
  )
where

-- for manual definitions if needed, or generated code

import Data.Aeson (FromJSON, ToJSON, Value (..), encode)
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Data.Kind (Type)
-- Re-exports from Anthropic.Types (pure types)
-- Note: We import ToolUse but not ToolResult from Anthropic.Types
-- because ExoMonad.Effect.Types defines its own ToolResult for tool dispatchers.

import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Time qualified as Time
import ExoMonad.Anthropic.Types
  ( ContentBlock (..),
    ImageSource (..),
    Message (..),
    RedactedThinking (..),
    Role (..),
    ThinkingContent (..),
  )
import ExoMonad.Effect.Decision.Types
  ( Decision (..),
    DecisionContext (..),
    DecisionTrace (..),
  )
import ExoMonad.LLM.Types (CallError (..))
import ExoMonad.Effect.Log
import ExoMonad.Effect.TUI
  ( PopupDefinition (..),
    PopupResult (..),
    TUI (..),
    showUI,
  )
import ExoMonad.StructuredOutput (StructuredOutput (..), formatDiagnostic)
import Lens.Micro.TH (makeLenses)
import Polysemy (Embed, Member, Sem, embed, interpret, makeSem)
import Polysemy.Error (Error, catch, throw)
import Polysemy.Internal (send)
import Polysemy.State (State, get, gets, modify, put, runState)
import System.Random (randomRIO)
import Prelude hiding (State, get, gets, modify, modifyIORef, newIORef, put, readIORef, runState, writeIORef)
import Prelude qualified as P

-- | Compatibility alias for Polysemy's Sem
type Eff = Sem

-- ══════════════════════════════════════════════════════════════
-- STATE EFFECT
-- ══════════════════════════════════════════════════════════════

-- State effect is now provided by Polysemy.State

-- ══════════════════════════════════════════════════════════════
-- RANDOM EFFECT
-- ══════════════════════════════════════════════════════════════

data Random m a where
  RandomInt :: Int -> Int -> Random m Int -- lo hi inclusive
  RandomDouble :: Random m Double

makeSem ''Random

runRandom :: (Member (Embed IO) r) => Sem (Random ': r) a -> Sem r a
runRandom = interpret $ \case
  RandomInt lo hi -> embed $ randomRIO (lo, hi)
  RandomDouble -> embed $ randomRIO (0.0, 1.0)

-- ══════════════════════════════════════════════════════════════
-- TIME EFFECT
-- ══════════════════════════════════════════════════════════════

data Time m a where
  GetCurrentTime :: Time m UTCTime

makeSem ''Time

runTime :: (Member (Embed IO) r) => Sem (Time ': r) a -> Sem r a
runTime = interpret $ \case
  GetCurrentTime -> embed Time.getCurrentTime

-- ══════════════════════════════════════════════════════════════
-- LLM EFFECT TYPES (must be defined before GADT for TH staging)
-- ══════════════════════════════════════════════════════════════

-- | Record of a tool invocation
data ToolInvocation = ToolInvocation
  { tiName :: Text,
    tiInput :: Value,
    tiOutput :: Value
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Result of running an LLM turn
data TurnResult output = TurnResult
  { trOutput :: output,
    trToolsInvoked :: [ToolInvocation],
    trNarrative :: Text,
    trThinking :: Text
  }
  deriving (Show, Eq, Generic, Functor)

-- | Result of parsing LLM output
data TurnParseResult output
  = TurnParsed (TurnResult output)
  | TurnParseFailed
      { tpfRawJson :: Value,
        tpfNarrative :: Text,
        tpfError :: String,
        tpfToolsInvoked :: [ToolInvocation]
      }
  deriving (Show, Eq, Functor)

-- ══════════════════════════════════════════════════════════════
-- LLM EFFECT
-- ══════════════════════════════════════════════════════════════

-- | The LLM effect runs a complete turn with template and tools
data LLM m a where
  RunTurnOp ::
    Text -> -- System prompt
    NonEmpty ContentBlock -> -- User content
    Value -> -- Output schema
    [Value] -> -- Tool definitions
    LLM m (TurnResult Value)

makeSem ''LLM

-- | Result of executing a tool
data ToolResult
  = ToolSuccess Value
  | ToolBreak Text
  deriving stock (Generic)

deriving stock instance Show ToolResult

deriving stock instance Eq ToolResult

-- | LLM configuration
data LLMConfig = LLMConfig
  { llmApiKey :: Text,
    llmModel :: Text,
    llmMaxTokens :: Int,
    llmThinkingBudget :: Maybe Int
  }
  deriving (Show, Eq, Generic)

-- | Hooks for LLM lifecycle events
data LLMHooks = LLMHooks
  { onTurnStart :: IO (),
    onTurnEnd :: IO ()
  }

noHooks :: LLMHooks
noHooks = LLMHooks (pure ()) (pure ())

-- | A tool dispatcher executes tools by name, returning results
type ToolDispatcher event effs =
  Text -> Value -> Eff effs (Either Text ToolResult)

-- ══════════════════════════════════════════════════════════════
-- TOOL VALIDATION (Parse Don't Validate)
-- ══════════════════════════════════════════════════════════════

-- | Evidence that a tool input has been validated against its schema.
--
-- This newtype provides proof that JSON was successfully parsed into
-- the tool's input type. Functions receiving @ValidatedToolInput a@
-- can trust that validation has already occurred - no need to re-check.
--
-- = The "Parse Don't Validate" Pattern
--
-- Instead of:
--
-- @
-- validateInput :: Value -> Bool
-- executeIfValid :: Value -> IO Result
-- executeIfValid val = if validateInput val then ... else error "invalid"
-- @
--
-- Use:
--
-- @
-- parseInput :: Value -> Either ToolError (ValidatedToolInput MyInput)
-- execute :: ValidatedToolInput MyInput -> IO Result
-- execute (ValidatedToolInput input) = ...  -- validation guaranteed
-- @
--
-- The type carries the proof that validation succeeded.
newtype ValidatedToolInput a = ValidatedToolInput {getValidated :: a}
  deriving (Show, Eq, Functor)

-- | Structured errors for tool execution.
--
-- Use pattern matching to handle specific error cases:
--
-- @
-- case result of
--   Left (ToolNotFound name) -> logWarn $ "Unknown tool: " <> name
--   Left (ToolValidationFailed err) -> logWarn $ "Bad input: " <> tveParseError err
--   Left (ToolExecutionFailed msg) -> logWarn $ "Execution error: " <> msg
--   Right output -> ...
-- @
data ToolError
  = -- | Tool name not recognized in registry
    ToolNotFound Text
  | ToolValidationFailed
      { -- | Name of the tool that failed validation
        tveToolName :: Text,
        -- | JSON Schema the input should have matched
        tveExpectedSchema :: Value,
        -- | The actual input that failed to parse
        tveActualInput :: Value,
        -- | The parse error message
        tveParseError :: Text
      }
  | -- \^ Tool input failed schema validation

    -- | Tool execution threw an error
    ToolExecutionFailed Text
  deriving (Show, Eq, Generic)

instance ToJSON ToolError

instance FromJSON ToolError

-- | Convert a ToolError to a human-readable message.
--
-- Useful for compatibility with code expecting @Either Text ToolResult@.
toolErrorToText :: ToolError -> Text
toolErrorToText = \case
  ToolNotFound name ->
    "Unknown tool: " <> name
  ToolValidationFailed {tveToolName, tveParseError} ->
    "Tool '" <> tveToolName <> "' input validation failed: " <> tveParseError
  ToolExecutionFailed msg ->
    "Tool execution failed: " <> msg

-- | Build content blocks from text + images
withImages :: Text -> [ImageSource] -> NonEmpty ContentBlock
withImages text images = Text {text = text} :| map Image images

-- | Run a turn and throw CallError on failure.
runTurn ::
  forall output effs.
  (Member LLM effs, Member (Error CallError) effs, StructuredOutput output) =>
  Text ->
  Text ->
  Value ->
  [Value] ->
  Sem effs (TurnResult output)
runTurn systemPrompt userAction =
  runTurnContent systemPrompt (Text {text = userAction} :| [])

runTurnContent ::
  forall output effs.
  (Member LLM effs, Member (Error CallError) effs, StructuredOutput output) =>
  Text ->
  NonEmpty ContentBlock ->
  Value ->
  [Value] ->
  Sem effs (TurnResult output)
runTurnContent systemPrompt userContent schema tools = do
  tr <- runTurnOp systemPrompt userContent schema tools
  let rawJson = tr.trOutput
  case parseStructured rawJson of
    Right parsed -> pure tr {trOutput = parsed}
    Left diag ->
      throw $
        CallParseFailed (formatDiagnostic diag)

-- | Make an LLM call that throws CallError on failure.
llmCall ::
  forall output effs.
  (Member LLM effs, Member (Error CallError) effs, StructuredOutput output) =>
  Text ->
  Text ->
  Value ->
  Sem effs output
llmCall systemPrompt userInput schema = do
  tr <- runTurn @output systemPrompt userInput schema []
  pure tr.trOutput

-- | Make an LLM call with tool support, throwing CallError on failure.
llmCallWithTools ::
  forall output effs.
  (Member LLM effs, Member (Error CallError) effs, StructuredOutput output) =>
  Text ->
  Text ->
  Value ->
  [Value] ->
  Sem effs (TurnResult output)
llmCallWithTools = runTurn @output

-- | Deprecated in favor of llmCall
llmCallEither ::
  forall output effs.
  (Member LLM effs, Member (Error CallError) effs, StructuredOutput output) =>
  Text ->
  Text ->
  Value ->
  Sem effs (Either Text output)
llmCallEither systemPrompt userInput schema =
  catch
    (Right <$> llmCall @output systemPrompt userInput schema)
    (\e -> pure $ Left $ T.pack $ show e)

-- | Deprecated in favor of llmCall
llmCallStructured ::
  forall output effs.
  (Member LLM effs, Member (Error CallError) effs, StructuredOutput output) =>
  Text ->
  Text ->
  Value ->
  Sem effs (Either CallError output)
llmCallStructured systemPrompt userInput schema =
  catch
    (Right <$> llmCall @output systemPrompt userInput schema)
    (\e -> pure $ Left e)

-- | Deprecated in favor of llmCallWithTools
llmCallEitherWithTools ::
  forall output effs.
  (Member LLM effs, Member (Error CallError) effs, StructuredOutput output) =>
  Text ->
  Text ->
  Value ->
  [Value] ->
  Sem effs (Either Text output)
llmCallEitherWithTools systemPrompt userInput schema tools =
  catch
    (Right . (.trOutput) <$> llmCallWithTools @output systemPrompt userInput schema tools)
    (\e -> pure $ Left $ T.pack $ show e)

-- | Deprecated in favor of llmCallWithTools
llmCallStructuredWithTools ::
  forall output effs.
  (Member LLM effs, Member (Error CallError) effs, StructuredOutput output) =>
  Text ->
  Text ->
  Value ->
  [Value] ->
  Sem effs (Either CallError output)
llmCallStructuredWithTools systemPrompt userInput schema tools =
  catch
    (Right . (.trOutput) <$> llmCallWithTools @output systemPrompt userInput schema tools)
    (\e -> pure $ Left e)

-- ══════════════════════════════════════════════════════════════
-- CHAT HISTORY EFFECT
-- ══════════════════════════════════════════════════════════════

data ChatHistory m a where
  GetHistory :: ChatHistory m [Message]
  AppendMessages :: [Message] -> ChatHistory m ()
  ClearHistory :: ChatHistory m ()

makeSem ''ChatHistory

runChatHistory :: (Member (Embed IO) r) => Sem (ChatHistory ': r) a -> Sem r a
runChatHistory action = do
  ref <- embed $ newIORef ([] :: [Message])
  runChatHistoryWith ref action

runChatHistoryWith :: (Member (Embed IO) r) => IORef [Message] -> Sem (ChatHistory ': r) a -> Sem r a
runChatHistoryWith ref = interpret $ \case
  GetHistory -> embed $ readIORef ref
  AppendMessages msgs -> embed $ modifyIORef ref (++ msgs)
  ClearHistory -> embed $ writeIORef ref []

-- | Roughly estimate token count for a list of messages.
--
-- Uses a common @character_count / 4@ heuristic as a quick approximation.
-- This is intentionally coarse and:
--
--   * Is not model- or tokenizer-aware
--   * Can be significantly off for technical content, code, or non-English text
--   * Does not account for provider-specific image/tool token handling
--
-- Use this only for budgeting and back-of-the-envelope estimates,
-- not for precise token accounting.
estimateTokens :: [Message] -> Int
estimateTokens msgs = sum (map estimateMessageChars msgs) `div` 4

-- | Estimate character count for a single message.
-- This is a rough estimate for use with 'estimateTokens'.
estimateMessageChars :: Message -> Int
estimateMessageChars msg = sum (map estimateBlockChars (toList msg.content))

-- | Estimate character count for a content block.
-- All estimates are approximate and may under- or over-estimate true token usage.
estimateBlockChars :: ContentBlock -> Int
estimateBlockChars = \case
  Text {text = t} -> T.length t
  Image {source = _} -> 1000 -- Rough placeholder; actual image tokens vary by provider
  ToolUse {name = n, input = i} -> T.length n + estimateValueChars i
  ToolResult {content = c} -> T.length c
  Thinking {thinking = t} -> T.length t
  RedactedThinking {data_ = _} -> 100 -- Encrypted/hidden; arbitrary rough estimate
  Json {json = v} -> estimateValueChars v

-- | Estimate characters in a JSON value via its encoded length.
estimateValueChars :: Value -> Int
estimateValueChars = fromIntegral . LBS.length . encode

-- ══════════════════════════════════════════════════════════════
-- EMIT EFFECT
-- ══════════════════════════════════════════════════════════════

data Emit event m a where
  EmitEvent :: event -> Emit event m ()

makeSem ''Emit

runEmit :: (Member (Embed IO) r) => (event -> IO ()) -> Sem (Emit event ': r) a -> Sem r a
runEmit handler = interpret $ \case
  EmitEvent e -> embed $ handler e

-- ══════════════════════════════════════════════════════════════
-- REQUEST INPUT EFFECT
-- ══════════════════════════════════════════════════════════════

-- | User input request effect (choice, text, etc.).
--
-- This effect is opaque - use the smart constructors:
-- - 'requestChoice' for multiple-choice questions
-- - 'requestText' for free text input
-- - 'requestTextWithPhoto' for text input with photo support
-- - 'requestCustom' for custom request types
data RequestInput m a where
  RequestChoice :: Text -> [(Text, a)] -> RequestInput m a
  RequestText :: Text -> RequestInput m Text
  RequestTextWithPhoto :: Text -> RequestInput m (Text, [(Text, Text)])
  RequestCustom :: Text -> Value -> RequestInput m Value

makeSem ''RequestInput

-- | Send a custom request to the UI (extensibility point).
-- requestCustom is generated by makeSem
data InputHandler = InputHandler
  { ihChoice :: forall a. Text -> [(Text, a)] -> IO a,
    ihText :: Text -> IO Text,
    ihTextWithPhoto :: Text -> IO (Text, [(Text, Text)]),
    ihCustom :: Text -> Value -> IO Value
  }

runRequestInput :: (Member (Embed IO) r) => InputHandler -> Sem (RequestInput ': r) a -> Sem r a
runRequestInput handler = interpret $ \case
  RequestChoice prompt choices ->
    let InputHandler {ihChoice = choiceHandler} = handler
     in embed $ choiceHandler prompt choices
  RequestText prompt -> embed $ handler.ihText prompt
  RequestTextWithPhoto prompt -> embed $ handler.ihTextWithPhoto prompt
  RequestCustom tag payload -> embed $ handler.ihCustom tag payload

-- ══════════════════════════════════════════════════════════════
-- DECISION LOG EFFECT
-- ══════════════════════════════════════════════════════════════

data DecisionLog m a where
  RecordDecision :: DecisionTrace -> DecisionLog m ()

makeSem ''DecisionLog

-- | Pure runner for DecisionLog (drops logs).
runDecisionLogPure :: Sem (DecisionLog ': r) a -> Sem r a
runDecisionLogPure = interpret $ \case
  RecordDecision _ -> pure ()

-- ══════════════════════════════════════════════════════════════
-- RETURN EFFECT
-- ══════════════════════════════════════════════════════════════

-- | Return effect - terminates execution with a value.
--
-- Use 'returnValue' to exit a computation with a typed result.
-- Useful for MCP tool handlers that need to return structured output.
--
-- @
-- findCallers :: Args -> Sem (Return Result ': r) Result
-- findCallers args = do
--   result <- computeResult args
--   returnValue result
-- @
data Return (a :: Type) m b where
  ReturnValue :: a -> Return a m a

makeSem ''Return

-- | Run the Return effect, extracting the returned value.
--
-- The computation must end with 'returnValue' to provide the result.
--
-- @
-- result <- runReturn $ do
--   value <- computeSomething
--   returnValue value
-- -- result :: a
-- @
runReturn :: forall a r. Sem (Return a ': r) a -> Sem r a
runReturn = interpret $ \case
  ReturnValue a -> pure a
