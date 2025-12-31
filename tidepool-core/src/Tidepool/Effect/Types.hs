{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  , llmCallWithTools
  , withImages

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

    -- * Simple Runners (pure/IOE only)
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

import Effectful
import Effectful.Dispatch.Dynamic
import qualified Effectful.State.Static.Local as EState
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

data State s :: Effect where
  Get :: State s m s
  Put :: s -> State s m ()

type instance DispatchOf (State s) = 'Dynamic

get :: State s :> es => Eff es s
get = send Get

gets :: State s :> es => (s -> a) -> Eff es a
gets f = f <$> get

put :: State s :> es => s -> Eff es ()
put = send . Put

modify :: State s :> es => (s -> s) -> Eff es ()
modify f = get >>= put . f

runState :: s -> Eff (State s : es) a -> Eff es (a, s)
runState initial = reinterpret (EState.runState initial) $ \_ -> \case
  Get   -> EState.get
  Put s -> EState.put s

-- ══════════════════════════════════════════════════════════════
-- RANDOM EFFECT
-- ══════════════════════════════════════════════════════════════

data Random :: Effect where
  RandomInt :: Int -> Int -> Random m Int  -- lo hi inclusive
  RandomDouble :: Random m Double

type instance DispatchOf Random = 'Dynamic

randomInt :: Random :> es => Int -> Int -> Eff es Int
randomInt lo hi = send (RandomInt lo hi)

randomDouble :: Random :> es => Eff es Double
randomDouble = send RandomDouble

runRandom :: IOE :> es => Eff (Random : es) a -> Eff es a
runRandom = interpret $ \_ -> \case
  RandomInt lo hi -> liftIO $ randomRIO (lo, hi)
  RandomDouble    -> liftIO $ randomRIO (0.0, 1.0)

-- ══════════════════════════════════════════════════════════════
-- TIME EFFECT
-- ══════════════════════════════════════════════════════════════

data Time :: Effect where
  GetCurrentTime :: Time m UTCTime

type instance DispatchOf Time = 'Dynamic

getCurrentTime :: Time :> es => Eff es UTCTime
getCurrentTime = send GetCurrentTime

runTime :: IOE :> es => Eff (Time : es) a -> Eff es a
runTime = interpret $ \_ -> \case
  GetCurrentTime -> liftIO Time.getCurrentTime

-- ══════════════════════════════════════════════════════════════
-- LLM EFFECT
-- ══════════════════════════════════════════════════════════════

-- | The LLM effect runs a complete turn with template and tools
data LLM :: Effect where
  RunTurnOp
    :: Text                          -- System prompt
    -> [ContentBlock]                -- User content
    -> Value                         -- Output schema
    -> [Value]                       -- Tool definitions
    -> LLM m (TurnOutcome (TurnResult Value))

type instance DispatchOf LLM = 'Dynamic

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
type ToolDispatcher event es =
  Text -> Value -> Eff es (Either Text ToolResult)

-- | Build content blocks from text + images
withImages :: Text -> [ImageSource] -> [ContentBlock]
withImages text images = TextBlock text : map ImageBlock images

runTurn
  :: forall output es.
     (LLM :> es, FromJSON output)
  => Text -> Text -> Value -> [Value]
  -> Eff es (TurnOutcome (TurnParseResult output))
runTurn systemPrompt userAction =
  runTurnContent systemPrompt [TextBlock userAction]

runTurnContent
  :: forall output es.
     (LLM :> es, FromJSON output)
  => Text -> [ContentBlock] -> Value -> [Value]
  -> Eff es (TurnOutcome (TurnParseResult output))
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
  :: forall output es. (LLM :> es, FromJSON output)
  => Text -> Text -> Value -> Eff es output
llmCall systemPrompt userInput schema = do
  result <- llmCallEither @output systemPrompt userInput schema
  case result of
    Left err -> error $ "LLM call failed: " <> T.unpack err
    Right output -> pure output

llmCallEither
  :: forall output es. (LLM :> es, FromJSON output)
  => Text -> Text -> Value -> Eff es (Either Text output)
llmCallEither systemPrompt userInput schema = do
  result <- runTurn @output systemPrompt userInput schema []
  case result of
    TurnBroken reason -> pure $ Left $ "Unexpected break: " <> reason
    TurnCompleted (TurnParsed tr) -> pure $ Right tr.trOutput
    TurnCompleted (TurnParseFailed {tpfError}) -> pure $ Left $ "Parse failed: " <> T.pack tpfError

llmCallWithTools
  :: forall output es. (LLM :> es, FromJSON output)
  => Text -> Text -> Value -> [Value] -> Eff es (Either Text output)
llmCallWithTools systemPrompt userInput schema tools = do
  result <- runTurn @output systemPrompt userInput schema tools
  case result of
    TurnBroken reason -> pure $ Left $ "Turn broken: " <> reason
    TurnCompleted (TurnParsed tr) -> pure $ Right tr.trOutput
    TurnCompleted (TurnParseFailed {tpfError}) -> pure $ Left $ "Parse failed: " <> T.pack tpfError

-- ══════════════════════════════════════════════════════════════
-- CHAT HISTORY EFFECT
-- ══════════════════════════════════════════════════════════════

data ChatHistory :: Effect where
  GetHistory :: ChatHistory m [Message]
  AppendMessages :: [Message] -> ChatHistory m ()
  ClearHistory :: ChatHistory m ()

type instance DispatchOf ChatHistory = 'Dynamic

getHistory :: ChatHistory :> es => Eff es [Message]
getHistory = send GetHistory

appendMessages :: ChatHistory :> es => [Message] -> Eff es ()
appendMessages = send . AppendMessages

clearHistory :: ChatHistory :> es => Eff es ()
clearHistory = send ClearHistory

runChatHistory :: IOE :> es => Eff (ChatHistory : es) a -> Eff es a
runChatHistory action = do
  ref <- liftIO $ newIORef ([] :: [Message])
  runChatHistoryWith ref action

runChatHistoryWith :: IOE :> es => IORef [Message] -> Eff (ChatHistory : es) a -> Eff es a
runChatHistoryWith ref = interpret $ \_ -> \case
  GetHistory -> liftIO $ readIORef ref
  AppendMessages msgs -> liftIO $ modifyIORef ref (++ msgs)
  ClearHistory -> liftIO $ writeIORef ref []

-- | Estimate token count for a list of messages
-- Uses character_count / 4 approximation
estimateTokens :: [Message] -> Int
estimateTokens msgs = sum (map estimateMessageChars msgs) `div` 4

-- | Estimate character count for a single message
estimateMessageChars :: Message -> Int
estimateMessageChars msg = sum (map estimateBlockChars msg.content)

-- | Estimate character count for a content block
estimateBlockChars :: ContentBlock -> Int
estimateBlockChars = \case
  TextBlock t -> T.length t
  ImageBlock _ -> 1000  -- Images have separate token accounting; rough placeholder
  ToolUseBlock tu -> T.length tu.toolName + estimateValueChars tu.toolInput
  ToolResultBlock tr -> T.length tr.toolResultContent
  ThinkingBlock tc -> T.length tc.thinkingText
  RedactedThinkingBlock _ -> 100  -- Encrypted, unknown size
  JsonBlock v -> estimateValueChars v

-- | Estimate characters in a JSON value
estimateValueChars :: Value -> Int
estimateValueChars = fromIntegral . LBS.length . encode

-- ══════════════════════════════════════════════════════════════
-- EMIT EFFECT
-- ══════════════════════════════════════════════════════════════

data Emit event :: Effect where
  EmitEvent :: event -> Emit event m ()

type instance DispatchOf (Emit event) = 'Dynamic

emit :: Emit event :> es => event -> Eff es ()
emit = send . EmitEvent

runEmit :: IOE :> es => (event -> IO ()) -> Eff (Emit event : es) a -> Eff es a
runEmit handler = interpret $ \_ -> \case
  EmitEvent e -> liftIO $ handler e

-- ══════════════════════════════════════════════════════════════
-- REQUEST INPUT EFFECT
-- ══════════════════════════════════════════════════════════════

data RequestInput :: Effect where
  RequestChoice :: Text -> [(Text, a)] -> RequestInput m a
  RequestText :: Text -> RequestInput m Text
  RequestDice :: Text -> [(Int, Int, Text)] -> RequestInput m Int
  RequestTextWithPhoto :: Text -> RequestInput m (Text, [(Text, Text)])
  RequestCustom :: Text -> Value -> RequestInput m Value

type instance DispatchOf RequestInput = 'Dynamic

requestChoice :: RequestInput :> es => Text -> [(Text, a)] -> Eff es a
requestChoice prompt choices = send (RequestChoice prompt choices)

requestText :: RequestInput :> es => Text -> Eff es Text
requestText = send . RequestText

requestDice :: RequestInput :> es => Text -> [(Int, Int, Text)] -> Eff es Int
requestDice prompt diceWithHints = send (RequestDice prompt diceWithHints)

requestTextWithPhoto :: RequestInput :> es => Text -> Eff es (Text, [(Text, Text)])
requestTextWithPhoto = send . RequestTextWithPhoto

requestCustom :: RequestInput :> es => Text -> Value -> Eff es Value
requestCustom tag payload = send (RequestCustom tag payload)

data InputHandler = InputHandler
  { ihChoice :: forall a. Text -> [(Text, a)] -> IO a
  , ihText   :: Text -> IO Text
  , ihTextWithPhoto :: Text -> IO (Text, [(Text, Text)])
  , ihDice   :: Text -> [(Int, Int, Text)] -> IO Int
  , ihCustom :: Text -> Value -> IO Value
  }

runRequestInput :: IOE :> es => InputHandler -> Eff (RequestInput : es) a -> Eff es a
runRequestInput handler = interpret $ \_ -> \case
  RequestChoice prompt choices ->
    let InputHandler { ihChoice = choiceHandler } = handler
    in liftIO $ choiceHandler prompt choices
  RequestText prompt -> liftIO $ handler.ihText prompt
  RequestTextWithPhoto prompt -> liftIO $ handler.ihTextWithPhoto prompt
  RequestDice prompt diceWithHints -> liftIO $ handler.ihDice prompt diceWithHints
  RequestCustom tag payload -> liftIO $ handler.ihCustom tag payload

-- ══════════════════════════════════════════════════════════════
-- QUESTION UI EFFECT
-- ══════════════════════════════════════════════════════════════

data QuestionUI :: Effect where
  AskQuestion :: Question -> QuestionUI m Answer

type instance DispatchOf QuestionUI = 'Dynamic

requestQuestion :: QuestionUI :> es => Question -> Eff es Answer
requestQuestion q = send (AskQuestion q)

type QuestionHandler = Question -> IO Answer

runQuestionUI :: IOE :> es => QuestionHandler -> Eff (QuestionUI : es) a -> Eff es a
runQuestionUI handler = interpret $ \_ -> \case
  AskQuestion q -> liftIO $ handler q

-- ══════════════════════════════════════════════════════════════
-- LOG EFFECT
-- ══════════════════════════════════════════════════════════════

data LogLevel = Debug | Info | Warn
  deriving (Show, Eq, Ord, Enum, Bounded)

data Log :: Effect where
  LogMsg :: LogLevel -> Text -> Log m ()

type instance DispatchOf Log = 'Dynamic

logMsg :: Log :> es => LogLevel -> Text -> Eff es ()
logMsg level msg = send (LogMsg level msg)

logDebug :: Log :> es => Text -> Eff es ()
logDebug = logMsg Debug

logInfo :: Log :> es => Text -> Eff es ()
logInfo = logMsg Info

logWarn :: Log :> es => Text -> Eff es ()
logWarn = logMsg Warn

runLog :: IOE :> es => LogLevel -> Eff (Log : es) a -> Eff es a
runLog minLevel = interpret $ \_ -> \case
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
