{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
-- | Core effect types for Tidepool - pure definitions without IO runners
--
-- This module contains the effect type definitions and pure operations
-- that are WASM-compatible. For IO-based runners, see 'Tidepool.Effect'
-- in tidepool-platform.
module Tidepool.Effect.Types
  ( -- * Core Effects
    State(..)
  , Random(..)
  , Time(..)
  , LLM(..)
  , ChatHistory(..)
  , Emit(..)
  , RequestInput        -- ^ Use smart constructors (requestChoice, requestText, etc.)
  , Log(..)
  , LogLevel(..)
  , QuestionUI(..)
  , TUI(..)

    -- * Return Effect (graph termination)
  , Return(..)
  , returnValue
  , runReturn

    -- * Node Metadata Effect (re-exports from Tidepool.Effect.NodeMeta)
  , NodeMeta(..)
  , NodeMetadata(..)
  , getNodeMeta
  , runNodeMeta
  , defaultNodeMeta

    -- * Effect Operations
  , get
  , gets
  , put
  , modify
  , randomInt
  , randomDouble
  , getCurrentTime
  , emit
  , requestChoice
  , requestText
  , requestTextWithPhoto
  , requestCustom
  , requestQuestion
  , logMsg
  , logMsgWith
  , logDebug
  , logDebugWith
  , logInfo
  , logInfoWith
  , logWarn
  , logWarnWith
  , LogFields
  , getHistory
  , appendMessages
  , clearHistory
  , estimateTokens
  , estimateMessageChars
  , estimateBlockChars

    -- * LLM Operations
  , runTurn
  , runTurnContent
  , llmCall
  , llmCallEither
  , llmCallEitherWithTools
  , llmCallStructured
  , llmCallStructuredWithTools
  , withImages

    -- * LLM Error Types
  , LlmError(..)

    -- * Result Types
  , TurnResult(..)
  , TurnParseResult(..)
  , ToolInvocation(..)
  , TurnOutcome(..)
  , ToolResult(..)
  , LLMConfig(..)
  , LLMHooks(..)
  , noHooks
  , InputHandler(..)
  , QuestionHandler

    -- * Tool Dispatcher Type
  , ToolDispatcher

    -- * Goto Types (for tool transitions)
  , GotoChoice
  , To

    -- * Tool Validation (Parse Don't Validate)
  , ValidatedToolInput(..)
  , ToolError(..)
  , toolErrorToText

    -- * Simple Runners (pure/IO)
  , runState
  , runRandom
  , runTime
  , runEmit
  , runLog
  , runChatHistory
  , runRequestInput
  , runQuestionUI

    -- * TUI Effect Operations
  , showUI
  , updateUI
  , closeUI

    -- * Decision Types
  , Decision(..)
  , DecisionContext(..)
  , DecisionTrace(..)

    -- * TUI Types (protocol)
  , UISpec(..)
  , Layout(..)
  , Element(..)
  , Interaction(..)
  , UIUpdate(..)
  , ElementUpdate(..)

    -- * Content Types (re-exports from Anthropic.Types)
  , ContentBlock(..)
  , ImageSource(..)
  , Message(..)
  , Role(..)
  , ThinkingContent(..)
  , RedactedThinking(..)

    -- * Question DSL (re-exports from Tidepool.Question)
  , Question(..)
  , Answer(..)
  , ItemDisposition(..)
  , Choice(..)
  , ChoiceOption(..)
  ) where

import Control.Monad.Freer (Eff, Member, send, interpret, sendM, LastMember)
import Control.Monad.Freer.Internal (handleRelayS)
import System.Random (randomRIO)
import System.IO (stderr)
import Data.Time (UTCTime)
import qualified Data.Time as Time
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Aeson (Value(..), FromJSON, ToJSON, encode)
import Tidepool.StructuredOutput (StructuredOutput(..), formatDiagnostic, ValidStructuredOutput)
import qualified Data.ByteString.Lazy as LBS
import GHC.Generics (Generic)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Data.Kind (Type)

-- Node metadata for teaching infrastructure
import Tidepool.Effect.NodeMeta (NodeMeta(..), NodeMetadata(..), getNodeMeta, runNodeMeta, defaultNodeMeta)

-- Re-exports from Anthropic.Types (pure types)
-- Note: We import ToolUse but not ToolResult from Anthropic.Types
-- because Tidepool.Effect.Types defines its own ToolResult for tool dispatchers.
import Tidepool.Anthropic.Types
  ( ContentBlock(..), ImageSource(..), Message(..), Role(..)
  , ThinkingContent(..), RedactedThinking(..)
  , ToolUse(..)
  )
import qualified Tidepool.Anthropic.Types as AT (ToolResult(..))

-- Question DSL types (shared across agents)
import Tidepool.Question (Question(..), Answer(..), ItemDisposition(..), Choice(..), ChoiceOption(..))

-- Goto types for tool transitions
import Tidepool.Graph.Goto (GotoChoice, To)

-- TUI effect and types
import Tidepool.Effect.TUI
  ( TUI(..), showUI, updateUI, closeUI
    , UISpec(..), Layout(..), Element(..), Interaction(..), UIUpdate(..), ElementUpdate(..)
  )

-- Decision types
import Tidepool.Effect.Decision
  ( Decision(..), DecisionContext(..), DecisionTrace(..)
  )

-- ══════════════════════════════════════════════════════════════
-- STATE EFFECT
-- ══════════════════════════════════════════════════════════════

data State s r where
  Get :: State s s
  Put :: s -> State s ()

get :: Member (State s) effs => Eff effs s
get = send Get

gets :: Member (State s) effs => (s -> a) -> Eff effs a
gets f = f <$> get

put :: Member (State s) effs => s -> Eff effs ()
put = send . Put

modify :: Member (State s) effs => (s -> s) -> Eff effs ()
modify f = get >>= put . f

runState :: s -> Eff (State s ': effs) a -> Eff effs (a, s)
runState initial = handleRelayS initial (\s a -> pure (a, s)) $ \s -> \case
  Get   -> \k -> k s s
  Put s' -> \k -> k s' ()

-- ══════════════════════════════════════════════════════════════
-- RANDOM EFFECT
-- ══════════════════════════════════════════════════════════════

data Random r where
  RandomInt :: Int -> Int -> Random Int  -- lo hi inclusive
  RandomDouble :: Random Double

randomInt :: Member Random effs => Int -> Int -> Eff effs Int
randomInt lo hi = send (RandomInt lo hi)

randomDouble :: Member Random effs => Eff effs Double
randomDouble = send RandomDouble

runRandom :: LastMember IO effs => Eff (Random ': effs) a -> Eff effs a
runRandom = interpret $ \case
  RandomInt lo hi -> sendM $ randomRIO (lo, hi)
  RandomDouble    -> sendM $ randomRIO (0.0, 1.0)

-- ══════════════════════════════════════════════════════════════
-- TIME EFFECT
-- ══════════════════════════════════════════════════════════════

data Time r where
  GetCurrentTime :: Time UTCTime

getCurrentTime :: Member Time effs => Eff effs UTCTime
getCurrentTime = send GetCurrentTime

runTime :: LastMember IO effs => Eff (Time ': effs) a -> Eff effs a
runTime = interpret $ \case
  GetCurrentTime -> sendM Time.getCurrentTime

-- ══════════════════════════════════════════════════════════════
-- LLM EFFECT
-- ══════════════════════════════════════════════════════════════

-- | The LLM effect runs a complete turn with template and tools
data LLM r where
  RunTurnOp
    :: NodeMetadata                  -- Node/graph context for teaching
    -> Text                          -- System prompt
    -> [ContentBlock]                -- User content
    -> Value                         -- Output schema
    -> [Value]                       -- Tool definitions
    -> LLM (TurnOutcome (TurnResult Value))

-- | Outcome of running a turn
--
-- Internal representation (unparameterized) used by the LLM effect.
-- Transitions are carried as untyped values and typed by the interpreter.
data TurnOutcome a
  = TurnCompleted a
  | TurnBroken Text
  | TurnTransitionHint Text Value  -- target name + payload (untyped)
  deriving (Show, Eq, Functor)

-- | Result of running an LLM turn
data TurnResult output = TurnResult
  { trOutput :: output
  , trToolsInvoked :: [ToolInvocation]
  , trNarrative :: Text
  , trThinking :: Text
  }
  deriving (Show, Eq, Generic, Functor)

-- | Result of parsing LLM output
data TurnParseResult output
  = TurnParsed (TurnResult output)
  | TurnParseFailed
      { tpfRawJson :: Value
      , tpfNarrative :: Text
      , tpfError :: String
      , tpfToolsInvoked :: [ToolInvocation]
      }
  deriving (Show, Eq, Functor)

-- | Record of a tool invocation
data ToolInvocation = ToolInvocation
  { tiName :: Text
  , tiInput :: Value
  , tiOutput :: Value
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Result of executing a tool
--
-- Parameterized by @targets@ to allow tools to trigger graph transitions.
-- Tools that don't transition can use an empty list: @ToolResult '[]@
-- Note: ToolTransition carries untyped target information; the interpreter will type-check
-- and convert it to a GotoChoice when the target types are known.
data ToolResult (targets :: [Type])
  = ToolSuccess Value
  | ToolBreak Text
  | ToolTransition Text Value  -- target name + payload (untyped, will be type-checked at interpreter)

instance Show (ToolResult targets) where
  show (ToolSuccess val) = "ToolSuccess " <> show val
  show (ToolBreak reason) = "ToolBreak " <> show reason
  show (ToolTransition target payload) = "ToolTransition " <> show target <> " " <> show payload

instance Eq (ToolResult targets) where
  (ToolSuccess v1) == (ToolSuccess v2) = v1 == v2
  (ToolBreak t1) == (ToolBreak t2) = t1 == t2
  (ToolTransition t1 p1) == (ToolTransition t2 p2) = t1 == t2 && p1 == p2
  _ == _ = False

-- | LLM configuration
data LLMConfig = LLMConfig
  { llmApiKey :: Text
  , llmModel :: Text
  , llmMaxTokens :: Int
  , llmThinkingBudget :: Maybe Int
  }
  deriving (Show, Eq, Generic)

-- | Hooks for LLM lifecycle events
data LLMHooks = LLMHooks
  { onTurnStart :: IO ()
  , onTurnEnd :: IO ()
  }

noHooks :: LLMHooks
noHooks = LLMHooks (pure ()) (pure ())

-- | A tool dispatcher executes tools by name, returning results
--
-- Parameterized by @targets@ to match the node's 'UsesEffects' targets.
-- Tools that don't transition can use an empty list.
type ToolDispatcher (targets :: [Type]) event effs =
  Text -> Value -> Eff effs (Either Text (ToolResult targets))

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
newtype ValidatedToolInput a = ValidatedToolInput { getValidated :: a }
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
  = ToolNotFound Text
    -- ^ Tool name not recognized in registry
  | ToolValidationFailed
    { tveToolName :: Text
      -- ^ Name of the tool that failed validation
    , tveExpectedSchema :: Value
      -- ^ JSON Schema the input should have matched
    , tveActualInput :: Value
      -- ^ The actual input that failed to parse
    , tveParseError :: Text
      -- ^ The parse error message
    }
    -- ^ Tool input failed schema validation
  | ToolExecutionFailed Text
    -- ^ Tool execution threw an error
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
  ToolValidationFailed{tveToolName, tveParseError} ->
    "Tool '" <> tveToolName <> "' input validation failed: " <> tveParseError
  ToolExecutionFailed msg ->
    "Tool execution failed: " <> msg

-- | Build content blocks from text + images
withImages :: Text -> [ImageSource] -> [ContentBlock]
withImages text images = TextBlock text : map ImageBlock images

runTurn
  :: forall output effs.
     (Member LLM effs, Member NodeMeta effs, StructuredOutput output, ValidStructuredOutput output)
  => Text -> Text -> Value -> [Value]
  -> Eff effs (TurnOutcome (TurnParseResult output))
runTurn systemPrompt userAction =
  runTurnContent systemPrompt [TextBlock userAction]

runTurnContent
  :: forall output effs.
     (Member LLM effs, Member NodeMeta effs, StructuredOutput output, ValidStructuredOutput output)
  => Text -> [ContentBlock] -> Value -> [Value]
  -> Eff effs (TurnOutcome (TurnParseResult output))
runTurnContent systemPrompt userContent schema tools = do
  meta <- getNodeMeta
  rawResult <- send (RunTurnOp meta systemPrompt userContent schema tools)
  case rawResult of
    TurnBroken reason -> return (TurnBroken reason)
    TurnTransitionHint target payload -> return (TurnTransitionHint target payload)
    TurnCompleted tr -> do
      let rawJson = tr.trOutput
      case parseStructured rawJson of
        Right parsed -> return $ TurnCompleted $ TurnParsed tr { trOutput = parsed }
        Left diag -> return $ TurnCompleted $ TurnParseFailed
          { tpfRawJson = rawJson
          , tpfNarrative = tr.trNarrative
          , tpfError = T.unpack (formatDiagnostic diag)
          , tpfToolsInvoked = tr.trToolsInvoked
          }

-- | Make an LLM call that throws on error.
--
-- Throws an exception if the LLM call fails. For error handling, use
-- 'llmCallEither' (returns Either with Text errors) or 'llmCallStructured'
-- (returns Either with structured LlmError type).
llmCall
  :: forall output effs. (Member LLM effs, Member NodeMeta effs, StructuredOutput output, ValidStructuredOutput output)
  => Text -> Text -> Value -> Eff effs output
llmCall systemPrompt userInput schema = do
  result <- llmCallEither @output systemPrompt userInput schema
  case result of
    Left err -> error $ "LLM call failed: " <> T.unpack err
    Right output -> pure output

-- | Make an LLM call returning Either with simple Text errors (no tools).
--
-- Returns @Left (error message)@ on failure or @Right output@ on success.
-- Errors are represented as plain Text. Use 'llmCallStructured' for structured
-- error types (rate limits, timeouts, etc.).
llmCallEither
  :: forall output effs. (Member LLM effs, Member NodeMeta effs, StructuredOutput output, ValidStructuredOutput output)
  => Text -> Text -> Value -> Eff effs (Either Text output)
llmCallEither systemPrompt userInput schema = do
  result <- runTurn @output systemPrompt userInput schema []
  case result of
    TurnBroken reason -> pure $ Left $ "Unexpected break: " <> reason
    TurnTransitionHint target _payload -> pure $ Left $ "Unexpected transition to " <> target <> " (no tools available)"
    TurnCompleted (TurnParsed tr) -> pure $ Right tr.trOutput
    TurnCompleted (TurnParseFailed {tpfError}) -> pure $ Left $ "Parse failed: " <> T.pack tpfError

-- | Make an LLM call with tool support, returning Either with simple Text errors.
--
-- Like 'llmCallEither' but supports tool definitions and invocations.
-- Returns @Left (error message)@ on failure or @Right output@ on success.
llmCallEitherWithTools
  :: forall output effs. (Member LLM effs, Member NodeMeta effs, StructuredOutput output, ValidStructuredOutput output)
  => Text -> Text -> Value -> [Value] -> Eff effs (Either Text output)
llmCallEitherWithTools systemPrompt userInput schema tools = do
  result <- runTurn @output systemPrompt userInput schema tools
  case result of
    TurnBroken reason -> pure $ Left $ "Turn broken: " <> reason
    TurnTransitionHint target _payload -> pure $ Left $ "Unexpected transition to " <> target <> " (not in interpreter context)"
    TurnCompleted (TurnParsed tr) -> pure $ Right tr.trOutput
    TurnCompleted (TurnParseFailed {tpfError}) -> pure $ Left $ "Parse failed: " <> T.pack tpfError

-- | Structured error type for LLM call failures.
--
-- Note: Currently 'llmCallStructured' and 'llmCallStructuredWithTools' only
-- produce 'LlmTurnBroken' and 'LlmParseFailed'. The other variants (RateLimited,
-- Timeout, NetworkError, etc.) are defined for future use when the LLM runner
-- is updated to return errors instead of crashing. This matches the pattern used
-- in Habitica/Telegram where stub runners return Left but real implementations
-- will use all variants.
data LlmError
  = LlmRateLimited              -- ^ Rate limit hit, retry later
  | LlmTimeout                  -- ^ Request timed out
  | LlmContextTooLong           -- ^ Input too long for context window
  | LlmParseFailed Text         -- ^ Schema parsing failed (includes error message)
  | LlmTurnBroken Text          -- ^ Turn was broken by tool (unexpected)
  | LlmNetworkError Text        -- ^ Network/connection failure
  | LlmUnauthorized             -- ^ Invalid API key
  | LlmOther Text               -- ^ Other errors with message
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Make an LLM call returning Either with structured errors (no tools).
--
-- Like 'llmCallEither' but returns structured 'LlmError' type instead of Text.
-- This allows better error handling with pattern matching on specific error cases
-- (rate limits, timeouts, etc.).
llmCallStructured
  :: forall output effs. (Member LLM effs, Member NodeMeta effs, StructuredOutput output, ValidStructuredOutput output)
  => Text -> Text -> Value -> Eff effs (Either LlmError output)
llmCallStructured systemPrompt userInput schema = do
  result <- runTurn @output systemPrompt userInput schema []
  case result of
    TurnBroken reason -> pure $ Left $ LlmTurnBroken reason
    TurnTransitionHint target _payload -> pure $ Left $ LlmTurnBroken $ "Unexpected transition to " <> target <> " (no tools available)"
    TurnCompleted (TurnParsed tr) -> pure $ Right tr.trOutput
    TurnCompleted (TurnParseFailed {tpfError}) -> pure $ Left $ LlmParseFailed (T.pack tpfError)

-- | Make an LLM call with tool support, returning Either with structured errors.
--
-- Like 'llmCallEitherWithTools' but returns structured 'LlmError' type instead of Text.
-- This allows better error handling with pattern matching on specific error cases.
llmCallStructuredWithTools
  :: forall output effs. (Member LLM effs, Member NodeMeta effs, StructuredOutput output, ValidStructuredOutput output)
  => Text -> Text -> Value -> [Value] -> Eff effs (Either LlmError output)
llmCallStructuredWithTools systemPrompt userInput schema tools = do
  result <- runTurn @output systemPrompt userInput schema tools
  case result of
    TurnBroken reason -> pure $ Left $ LlmTurnBroken reason
    TurnTransitionHint target _payload -> pure $ Left $ LlmTurnBroken $ "Unexpected transition to " <> target <> " (not in interpreter context)"
    TurnCompleted (TurnParsed tr) -> pure $ Right tr.trOutput
    TurnCompleted (TurnParseFailed {tpfError}) -> pure $ Left $ LlmParseFailed (T.pack tpfError)

-- ══════════════════════════════════════════════════════════════
-- CHAT HISTORY EFFECT
-- ══════════════════════════════════════════════════════════════

data ChatHistory r where
  GetHistory :: ChatHistory [Message]
  AppendMessages :: [Message] -> ChatHistory ()
  ClearHistory :: ChatHistory ()

getHistory :: Member ChatHistory effs => Eff effs [Message]
getHistory = send GetHistory

appendMessages :: Member ChatHistory effs => [Message] -> Eff effs ()
appendMessages = send . AppendMessages

clearHistory :: Member ChatHistory effs => Eff effs ()
clearHistory = send ClearHistory

runChatHistory :: LastMember IO effs => Eff (ChatHistory ': effs) a -> Eff effs a
runChatHistory action = do
  ref <- sendM $ newIORef ([] :: [Message])
  runChatHistoryWith ref action

runChatHistoryWith :: LastMember IO effs => IORef [Message] -> Eff (ChatHistory ': effs) a -> Eff effs a
runChatHistoryWith ref = interpret $ \case
  GetHistory -> sendM $ readIORef ref
  AppendMessages msgs -> sendM $ modifyIORef ref (++ msgs)
  ClearHistory -> sendM $ writeIORef ref []

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
estimateMessageChars msg = sum (map estimateBlockChars msg.content)

-- | Estimate character count for a content block.
-- All estimates are approximate and may under- or over-estimate true token usage.
estimateBlockChars :: ContentBlock -> Int
estimateBlockChars = \case
  TextBlock t -> T.length t
  ImageBlock _ -> 1000  -- Rough placeholder; actual image tokens vary by provider
  ToolUseBlock tu -> T.length tu.toolName + estimateValueChars tu.toolInput
  ToolResultBlock tr -> T.length tr.toolResultContent
  ThinkingBlock tc -> T.length tc.thinkingText
  RedactedThinkingBlock _ -> 100  -- Encrypted/hidden; arbitrary rough estimate
  JsonBlock v -> estimateValueChars v

-- | Estimate characters in a JSON value via its encoded length.
estimateValueChars :: Value -> Int
estimateValueChars = fromIntegral . LBS.length . encode

-- ══════════════════════════════════════════════════════════════
-- EMIT EFFECT
-- ══════════════════════════════════════════════════════════════

data Emit event r where
  EmitEvent :: event -> Emit event ()

emit :: Member (Emit event) effs => event -> Eff effs ()
emit = send . EmitEvent

runEmit :: LastMember IO effs => (event -> IO ()) -> Eff (Emit event ': effs) a -> Eff effs a
runEmit handler = interpret $ \case
  EmitEvent e -> sendM $ handler e

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
data RequestInput r where
  RequestChoice :: Text -> [(Text, a)] -> RequestInput a
  RequestText :: Text -> RequestInput Text
  RequestTextWithPhoto :: Text -> RequestInput (Text, [(Text, Text)])
  RequestCustom :: Text -> Value -> RequestInput Value

-- | Ask user to choose from a list of options.
--
-- @
-- result <- requestChoice "Which option?" [("Option A", 1), ("Option B", 2)]
-- @
requestChoice :: Member RequestInput effs => Text -> [(Text, a)] -> Eff effs a
requestChoice prompt choices = send (RequestChoice prompt choices)

-- | Ask user for free text input.
requestText :: Member RequestInput effs => Text -> Eff effs Text
requestText = send . RequestText

-- | Ask user for text input with photo support.
requestTextWithPhoto :: Member RequestInput effs => Text -> Eff effs (Text, [(Text, Text)])
requestTextWithPhoto = send . RequestTextWithPhoto

-- | Send a custom request to the UI (extensibility point).
requestCustom :: Member RequestInput effs => Text -> Value -> Eff effs Value
requestCustom tag payload = send (RequestCustom tag payload)

data InputHandler = InputHandler
  { ihChoice :: forall a. Text -> [(Text, a)] -> IO a
  , ihText   :: Text -> IO Text
  , ihTextWithPhoto :: Text -> IO (Text, [(Text, Text)])
  , ihCustom :: Text -> Value -> IO Value
  }

runRequestInput :: LastMember IO effs => InputHandler -> Eff (RequestInput ': effs) a -> Eff effs a
runRequestInput handler = interpret $ \case
  RequestChoice prompt choices ->
    let InputHandler { ihChoice = choiceHandler } = handler
    in sendM $ choiceHandler prompt choices
  RequestText prompt -> sendM $ handler.ihText prompt
  RequestTextWithPhoto prompt -> sendM $ handler.ihTextWithPhoto prompt
  RequestCustom tag payload -> sendM $ handler.ihCustom tag payload

-- ══════════════════════════════════════════════════════════════
-- QUESTION UI EFFECT
-- ══════════════════════════════════════════════════════════════

data QuestionUI r where
  AskQuestion :: Question -> QuestionUI Answer

requestQuestion :: Member QuestionUI effs => Question -> Eff effs Answer
requestQuestion q = send (AskQuestion q)

type QuestionHandler = Question -> IO Answer

runQuestionUI :: LastMember IO effs => QuestionHandler -> Eff (QuestionUI ': effs) a -> Eff effs a
runQuestionUI handler = interpret $ \case
  AskQuestion q -> sendM $ handler q

-- ══════════════════════════════════════════════════════════════
-- LOG EFFECT
-- ══════════════════════════════════════════════════════════════

data LogLevel = Debug | Info | Warn
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Structured log fields: key-value pairs for queryable log data.
--
-- Example:
-- @
-- logInfoWith "Scoring daily"
--   [ ("taskId", toJSON taskId)
--   , ("direction", toJSON ("up" :: Text))
--   ]
-- @
type LogFields = [(Text, Value)]

data Log r where
  LogMsg :: LogLevel -> Text -> Maybe LogFields -> Log ()

-- | Log a message at the given level (no structured fields).
logMsg :: Member Log effs => LogLevel -> Text -> Eff effs ()
logMsg level msg = send (LogMsg level msg Nothing)

-- | Log a message at the given level with structured fields.
logMsgWith :: Member Log effs => LogLevel -> Text -> LogFields -> Eff effs ()
logMsgWith level msg fields = send (LogMsg level msg (Just fields))

-- | Log a debug message.
logDebug :: Member Log effs => Text -> Eff effs ()
logDebug = logMsg Debug

-- | Log a debug message with structured fields.
logDebugWith :: Member Log effs => Text -> LogFields -> Eff effs ()
logDebugWith = logMsgWith Debug

-- | Log an info message.
logInfo :: Member Log effs => Text -> Eff effs ()
logInfo = logMsg Info

-- | Log an info message with structured fields.
--
-- Example:
-- @
-- logInfoWith "LLM tool calls"
--   [ ("count", toJSON (length toolCalls))
--   , ("tools", toJSON (map tcName toolCalls))
--   ]
-- @
logInfoWith :: Member Log effs => Text -> LogFields -> Eff effs ()
logInfoWith = logMsgWith Info

-- | Log a warning message.
logWarn :: Member Log effs => Text -> Eff effs ()
logWarn = logMsg Warn

-- | Log a warning message with structured fields.
logWarnWith :: Member Log effs => Text -> LogFields -> Eff effs ()
logWarnWith = logMsgWith Warn

runLog :: LastMember IO effs => LogLevel -> Eff (Log ': effs) a -> Eff effs a
runLog minLevel = interpret $ \case
  LogMsg level msg maybeFields
    | level >= minLevel -> do
        let fieldStr = case maybeFields of
              Nothing -> ""
              Just fs -> " | " <> T.intercalate ", " (map fst fs)
        sendM $ TIO.hPutStrLn stderr ("[" <> T.pack (show level) <> "] " <> msg <> fieldStr)
    | otherwise -> pure ()

-- ══════════════════════════════════════════════════════════════
-- RETURN EFFECT (graph termination)
-- ══════════════════════════════════════════════════════════════

-- | Return effect - terminates graph execution with a value.
--
-- Replaces the pattern of @Goto Exit result@ with a semantically clearer
-- effect that explicitly terminates the current graph/node execution.
--
-- @
-- -- Instead of:
-- gRoute :: mode :- LogicNode :@ UsesEffects '[Goto Exit Response]
-- routeHandler result = pure $ gotoExit result
--
-- -- Use:
-- gRoute :: mode :- LogicNode :@ UsesEffects '[Return Response]
-- routeHandler result = returnValue result
-- @
--
-- The Return effect is particularly useful for single-node graphs (MCP tools)
-- where Entry/Exit ceremony is overhead. With Return, the node simply
-- returns its result directly.
--
-- Note: The effect result type is @a@ (not @()@), so computations using
-- Return have the value type as their overall result type.
data Return (a :: Type) r where
  ReturnValue :: a -> Return a a

-- | Terminate graph execution with a value.
--
-- This is the primary way to exit a graph when using the Return effect.
-- The interpreter will capture this value as the graph's result.
--
-- Note: Returns the value directly, so the computation's result type
-- must match the Return effect's type parameter.
returnValue :: Member (Return a) effs => a -> Eff effs a
returnValue = send . ReturnValue

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
runReturn :: forall a effs. Eff (Return a ': effs) a -> Eff effs a
runReturn = interpret $ \case
  ReturnValue a -> pure a

