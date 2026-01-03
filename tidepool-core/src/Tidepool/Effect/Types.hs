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
  , RequestInput(..)
  , Log(..)
  , LogLevel(..)
  , QuestionUI(..)

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
  , requestDice
  , requestCustom
  , requestQuestion
  , logMsg
  , logDebug
  , logInfo
  , logWarn
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
  , llmCallTry
  , llmCallWithTools
  , llmCallWithToolsTry
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

    -- * Simple Runners (pure/IO)
  , runState
  , runRandom
  , runTime
  , runEmit
  , runLog
  , runChatHistory
  , runRequestInput
  , runQuestionUI

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
import Data.Time (UTCTime)
import qualified Data.Time as Time
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (Value(..), FromJSON, ToJSON, fromJSON, toJSON, Result(..), encode)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS
import GHC.Generics (Generic)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)

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
    :: Text                          -- System prompt
    -> [ContentBlock]                -- User content
    -> Value                         -- Output schema
    -> [Value]                       -- Tool definitions
    -> LLM (TurnOutcome (TurnResult Value))

-- | Outcome of running a turn
data TurnOutcome a
  = TurnCompleted a
  | TurnBroken Text
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
data ToolResult
  = ToolSuccess Value
  | ToolBreak Text
  deriving (Show, Eq)

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
type ToolDispatcher event effs =
  Text -> Value -> Eff effs (Either Text ToolResult)

-- | Build content blocks from text + images
withImages :: Text -> [ImageSource] -> [ContentBlock]
withImages text images = TextBlock text : map ImageBlock images

runTurn
  :: forall output effs.
     (Member LLM effs, FromJSON output)
  => Text -> Text -> Value -> [Value]
  -> Eff effs (TurnOutcome (TurnParseResult output))
runTurn systemPrompt userAction =
  runTurnContent systemPrompt [TextBlock userAction]

runTurnContent
  :: forall output effs.
     (Member LLM effs, FromJSON output)
  => Text -> [ContentBlock] -> Value -> [Value]
  -> Eff effs (TurnOutcome (TurnParseResult output))
runTurnContent systemPrompt userContent schema tools = do
  rawResult <- send (RunTurnOp systemPrompt userContent schema tools)
  case rawResult of
    TurnBroken reason -> return (TurnBroken reason)
    TurnCompleted tr -> do
      let rawJson = tr.trOutput
      case fromJSON rawJson of
        Success parsed -> return $ TurnCompleted $ TurnParsed tr { trOutput = parsed }
        Error err -> return $ TurnCompleted $ TurnParseFailed
          { tpfRawJson = rawJson
          , tpfNarrative = tr.trNarrative
          , tpfError = err
          , tpfToolsInvoked = tr.trToolsInvoked
          }

llmCall
  :: forall output effs. (Member LLM effs, FromJSON output)
  => Text -> Text -> Value -> Eff effs output
llmCall systemPrompt userInput schema = do
  result <- llmCallEither @output systemPrompt userInput schema
  case result of
    Left err -> error $ "LLM call failed: " <> T.unpack err
    Right output -> pure output

llmCallEither
  :: forall output effs. (Member LLM effs, FromJSON output)
  => Text -> Text -> Value -> Eff effs (Either Text output)
llmCallEither systemPrompt userInput schema = do
  result <- runTurn @output systemPrompt userInput schema []
  case result of
    TurnBroken reason -> pure $ Left $ "Unexpected break: " <> reason
    TurnCompleted (TurnParsed tr) -> pure $ Right tr.trOutput
    TurnCompleted (TurnParseFailed {tpfError}) -> pure $ Left $ "Parse failed: " <> T.pack tpfError

llmCallWithTools
  :: forall output effs. (Member LLM effs, FromJSON output)
  => Text -> Text -> Value -> [Value] -> Eff effs (Either Text output)
llmCallWithTools systemPrompt userInput schema tools = do
  result <- runTurn @output systemPrompt userInput schema tools
  case result of
    TurnBroken reason -> pure $ Left $ "Turn broken: " <> reason
    TurnCompleted (TurnParsed tr) -> pure $ Right tr.trOutput
    TurnCompleted (TurnParseFailed {tpfError}) -> pure $ Left $ "Parse failed: " <> T.pack tpfError

-- | Structured error type for LLM call failures
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

-- | Try variant: structured error instead of Text
llmCallTry
  :: forall output effs. (Member LLM effs, FromJSON output)
  => Text -> Text -> Value -> Eff effs (Either LlmError output)
llmCallTry systemPrompt userInput schema = do
  result <- runTurn @output systemPrompt userInput schema []
  case result of
    TurnBroken reason -> pure $ Left $ LlmTurnBroken reason
    TurnCompleted (TurnParsed tr) -> pure $ Right tr.trOutput
    TurnCompleted (TurnParseFailed {tpfError}) -> pure $ Left $ LlmParseFailed (T.pack tpfError)

-- | Try variant with tools: structured error instead of Text
llmCallWithToolsTry
  :: forall output effs. (Member LLM effs, FromJSON output)
  => Text -> Text -> Value -> [Value] -> Eff effs (Either LlmError output)
llmCallWithToolsTry systemPrompt userInput schema tools = do
  result <- runTurn @output systemPrompt userInput schema tools
  case result of
    TurnBroken reason -> pure $ Left $ LlmTurnBroken reason
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

data RequestInput r where
  RequestChoice :: Text -> [(Text, a)] -> RequestInput a
  RequestText :: Text -> RequestInput Text
  RequestDice :: Text -> [(Int, Int, Text)] -> RequestInput Int
  RequestTextWithPhoto :: Text -> RequestInput (Text, [(Text, Text)])
  RequestCustom :: Text -> Value -> RequestInput Value

requestChoice :: Member RequestInput effs => Text -> [(Text, a)] -> Eff effs a
requestChoice prompt choices = send (RequestChoice prompt choices)

requestText :: Member RequestInput effs => Text -> Eff effs Text
requestText = send . RequestText

requestDice :: Member RequestInput effs => Text -> [(Int, Int, Text)] -> Eff effs Int
requestDice prompt diceWithHints = send (RequestDice prompt diceWithHints)

requestTextWithPhoto :: Member RequestInput effs => Text -> Eff effs (Text, [(Text, Text)])
requestTextWithPhoto = send . RequestTextWithPhoto

requestCustom :: Member RequestInput effs => Text -> Value -> Eff effs Value
requestCustom tag payload = send (RequestCustom tag payload)

data InputHandler = InputHandler
  { ihChoice :: forall a. Text -> [(Text, a)] -> IO a
  , ihText   :: Text -> IO Text
  , ihTextWithPhoto :: Text -> IO (Text, [(Text, Text)])
  , ihDice   :: Text -> [(Int, Int, Text)] -> IO Int
  , ihCustom :: Text -> Value -> IO Value
  }

runRequestInput :: LastMember IO effs => InputHandler -> Eff (RequestInput ': effs) a -> Eff effs a
runRequestInput handler = interpret $ \case
  RequestChoice prompt choices ->
    let InputHandler { ihChoice = choiceHandler } = handler
    in sendM $ choiceHandler prompt choices
  RequestText prompt -> sendM $ handler.ihText prompt
  RequestTextWithPhoto prompt -> sendM $ handler.ihTextWithPhoto prompt
  RequestDice prompt diceWithHints -> sendM $ handler.ihDice prompt diceWithHints
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

data Log r where
  LogMsg :: LogLevel -> Text -> Log ()

logMsg :: Member Log effs => LogLevel -> Text -> Eff effs ()
logMsg level msg = send (LogMsg level msg)

logDebug :: Member Log effs => Text -> Eff effs ()
logDebug = logMsg Debug

logInfo :: Member Log effs => Text -> Eff effs ()
logInfo = logMsg Info

logWarn :: Member Log effs => Text -> Eff effs ()
logWarn = logMsg Warn

runLog :: LastMember IO effs => LogLevel -> Eff (Log ': effs) a -> Eff effs a
runLog minLevel = interpret $ \case
  LogMsg level _msg
    | level >= minLevel -> pure ()
    | otherwise -> pure ()

-- ══════════════════════════════════════════════════════════════
-- HELPERS
-- ══════════════════════════════════════════════════════════════

-- Internal helper: encode Value to Text
_encodeText :: Value -> Text
_encodeText v = case v of
  String t -> t
  _ -> TE.decodeUtf8 . LBS.toStrict . encode $ v
