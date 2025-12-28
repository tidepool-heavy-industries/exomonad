{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Core effect types for Tidepool game loops
module Tidepool.Effect
  ( -- * Core Effects
    GameEffects
  , State(..)
  , Random(..)
  , LLM(..)
  , ChatHistory(..)
  , Emit(..)
  , RequestInput(..)
  , Log(..)
  , LogLevel(..)

    -- * Effect Operations
  , get
  , gets
  , put
  , modify
  , randomInt
  , randomDouble
  , runTurn
  , emit
  , requestChoice
  , requestText
  , requestDice
  , logMsg
  , logDebug
  , logInfo
  , logWarn
  , getHistory
  , appendMessages
  , clearHistory

    -- * Result Types
  , TurnResult(..)
  , TurnParseResult(..)
  , ToolInvocation(..)
  , LLMConfig(..)
  , InputHandler(..)

    -- * Running Effects
  , runGame
  , runState
  , runRandom
  , runLLM
  , runLLMWithTools
  , runEmit
  , runRequestInput
  , runLog
  , runLogWithBridge
  , runChatHistory
  , runChatHistoryWithDB

    -- * Tool Execution Types
  , ToolDispatcher
  , ToolResult(..)

    -- * Turn Outcome
  , TurnOutcome(..)
  ) where

import Effectful
import Effectful.Dispatch.Dynamic
import qualified Effectful.State.Static.Local as EState
import System.Random (randomRIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Aeson (Value(..), FromJSON, ToJSON, fromJSON, toJSON, Result(..), encode, decode)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS
import GHC.Generics (Generic)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Data.Maybe (mapMaybe)
import qualified Tidepool.Anthropic.Client as Client
import Tidepool.Anthropic.Client
  ( SingleCallRequest(..), SingleCallResponse(..)
  , StopReason(..), ToolUse(..), Message(..), Role(..), ContentBlock(..)
  )
-- Note: ToolResult is imported qualified as Client.ToolResult to avoid
-- collision with Tidepool.Effect.ToolResult
import Tidepool.Anthropic.Http (ThinkingContent(..))
import qualified Tidepool.GUI.Core as GUICore
import Tidepool.GUI.Core (GUIBridge)
import qualified Tidepool.Storage as Storage
import Database.SQLite.Simple (Connection)

-- | The effect stack for game loops
-- IOE is at the base for IO operations (random, emit, input, log)
type GameEffects s event =
  '[ LLM
   , RequestInput
   , Log
   , ChatHistory
   , State s
   , Emit event
   , Random
   , IOE
   ]

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
-- LLM EFFECT
-- ══════════════════════════════════════════════════════════════

-- | The LLM effect runs a complete turn with template and tools
-- Interpreter handles: API calls, tool execution loop, retries, parsing
-- Returns TurnOutcome to signal whether turn completed or was broken by a tool
data LLM :: Effect where
  RunTurnOp
    :: Text                          -- System prompt (rules + mood guidance + world state)
    -> Text                          -- User message (player action ONLY)
    -> Value                         -- Output schema (JSON)
    -> [Value]                       -- Tool definitions (JSON)
    -> LLM m (TurnOutcome (TurnResult Value))  -- May complete or break

type instance DispatchOf LLM = 'Dynamic

-- | Run a turn with system prompt and user action
-- Returns TurnOutcome to allow caller to handle breaks
-- | Run a turn, returning either the parsed output or a parse failure with raw data
-- Callers should handle ParseFailure gracefully (e.g., with fallback output)
data TurnParseResult output
  = TurnParsed (TurnResult output)
  | TurnParseFailed
      { tpfRawJson :: Value
      , tpfNarrative :: Text
      , tpfError :: String
      , tpfToolsInvoked :: [ToolInvocation]
      }
  deriving (Show, Eq, Functor)

runTurn
  :: forall output es.
     (LLM :> es, FromJSON output)
  => Text                 -- system prompt (rules + world state)
  -> Text                 -- user action (player input only)
  -> Value                -- output schema
  -> [Value]              -- tool definitions
  -> Eff es (TurnOutcome (TurnParseResult output))
runTurn systemPrompt userAction schema tools = do
  rawResult <- send (RunTurnOp systemPrompt userAction schema tools)
  -- Parse the raw JSON output if turn completed
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

-- | Result of running an LLM turn
data TurnResult output = TurnResult
  { trOutput :: output                 -- Parsed structured output
  , trToolsInvoked :: [ToolInvocation] -- What tools were called (logging)
  , trNarrative :: Text                -- Any text blocks from response
  , trThinking :: Text                 -- Extended thinking content (if enabled)
  }
  deriving (Show, Eq, Generic, Functor)

-- | Record of a tool invocation for logging/debugging
data ToolInvocation = ToolInvocation
  { tiName :: Text
  , tiInput :: Value
  , tiOutput :: Value
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data LLMConfig = LLMConfig
  { llmApiKey :: Text
  , llmModel :: Text
  , llmMaxTokens :: Int
  , llmThinkingBudget :: Maybe Int  -- Token budget for extended thinking
  }
  deriving (Show, Eq, Generic)

-- | Run the LLM effect by calling the Anthropic API
-- Note: Tool execution uses a stub - for full tool support, use runLLMWithTools
-- Uses ChatHistory effect for conversation persistence and Log for debugging.
-- Always returns TurnCompleted since stub tools never break.
runLLM :: (IOE :> es, ChatHistory :> es, Log :> es) => LLMConfig -> Eff (LLM : es) a -> Eff es a
runLLM config = interpret $ \_ -> \case
  RunTurnOp systemPrompt userAction schema tools -> do
    -- Get prior conversation history (just action/response pairs)
    priorHistory <- getHistory

    logDebug $ "[LLM] Prior history: " <> T.pack (show (length priorHistory)) <> " messages"
    logDebug $ "[LLM] User action: " <> userAction

    let clientConfig = Client.ClientConfig
          { Client.apiKey = config.llmApiKey
          , Client.defaultModel = config.llmModel
          , Client.defaultMaxTokens = config.llmMaxTokens
          }
        outputSchema = if schema == toJSON () then Nothing else Just schema
        -- User message is JUST the action
        userMsg = Message User [TextBlock userAction]
        turnReq = Client.TurnRequest
          { Client.prompt = userAction  -- Just the action for the prompt field
          , Client.priorMessages = priorHistory
          , Client.systemPrompt = Just systemPrompt  -- Dynamic system prompt!
          , Client.outputSchema = outputSchema
          , Client.tools = tools
          , Client.toolExecutor = stubToolExecutor
          , Client.thinkingBudget = config.llmThinkingBudget
          }

    result <- liftIO $ Client.runTurnRequest clientConfig turnReq
    case result of
      Left err -> do
        logWarn $ "[LLM] API error: " <> T.pack (show err)
        -- Return a fallback result instead of crashing
        pure $ TurnCompleted TurnResult
          { trOutput = toJSON ()
          , trToolsInvoked = []
          , trNarrative = "*The spirits of Doskvol are silent. The world waits.*"
          , trThinking = ""
          }
      Right resp -> do
        -- Append just action + response to history (not the system prompt)
        let assistantMsg = Message Assistant [TextBlock resp.narrative]
        appendMessages [userMsg, assistantMsg]

        logDebug $ "[LLM] Assistant response:\n" <> resp.narrative
        logDebug $ "[LLM] Tools invoked: " <> T.pack (show (length resp.toolsInvoked))

        -- Stub tools never break, so always return TurnCompleted
        pure $ TurnCompleted TurnResult
          { trOutput = resp.output
          , trToolsInvoked = map convertInvocation resp.toolsInvoked
          , trNarrative = resp.narrative
          , trThinking = resp.thinking
          }
  where
    stubToolExecutor :: Text -> Value -> IO (Either Text Value)
    stubToolExecutor _name _input = pure $ Right (toJSON ())

    convertInvocation :: Client.ToolInvocation -> ToolInvocation
    convertInvocation inv = ToolInvocation
      { tiName = inv.invocationName
      , tiInput = inv.invocationInput
      , tiOutput = inv.invocationOutput
      }

-- | A tool dispatcher executes tools by name, returning results
-- This is used by runLLMWithTools to execute tools in the effect context
-- Tools can return:
--   Left err -> Tool error (reported back to LLM)
--   Right (ToolSuccess val) -> Tool succeeded (value returned to LLM)
--   Right (ToolBreak reason) -> Tool signals turn break (loop exits early)
type ToolDispatcher event es =
  Text    -- Tool name
  -> Value  -- Tool input (JSON)
  -> Eff es (Either Text ToolResult)  -- Error or result

-- | Run the LLM effect with full tool execution support
-- Tools are executed in the effect context, allowing them to use Emit, RequestInput, etc.
-- Maintains conversation history across turns via ChatHistory effect.
-- System prompt is provided per-turn (dynamic, includes world state).
-- Returns TurnBroken if any tool signals a break (for state machine transitions).
runLLMWithTools
  :: forall es event a.
     (IOE :> es, Emit event :> es, RequestInput :> es, Random :> es, ChatHistory :> es, Log :> es)
  => LLMConfig
  -> ToolDispatcher event es
  -> Eff (LLM : es) a
  -> Eff es a
runLLMWithTools config dispatcher = interpret $ \_ -> \case
  RunTurnOp systemPrompt userAction schema tools -> do
    -- Get prior conversation history (just action/response pairs)
    priorHistory <- getHistory

    logDebug $ "[LLM] Prior history: " <> T.pack (show (length priorHistory)) <> " messages"
    logDebug $ "[LLM] User action: " <> userAction

    let clientConfig = Client.ClientConfig
          { Client.apiKey = config.llmApiKey
          , Client.defaultModel = config.llmModel
          , Client.defaultMaxTokens = config.llmMaxTokens
          }
        outputSchema = if schema == toJSON () then Nothing else Just schema
        -- User message is JUST the action
        actionMsg = Message User [TextBlock userAction]
        initialMessages = priorHistory ++ [actionMsg]

    -- Run the tool loop (may complete or break)
    -- Pass systemPrompt to toolLoop so it can use it for API calls
    loopResult <- toolLoop clientConfig systemPrompt outputSchema tools initialMessages [] [] []

    case loopResult of
      -- Tool broke the turn - don't append to history, return break signal
      Left breakReason -> do
        logInfo $ "[LLM] Turn broken by tool: " <> breakReason
        return (TurnBroken breakReason)

      -- Turn completed normally
      Right (result, finalContent) -> do
        -- Append just action + response to history (not the system prompt)
        let assistantMsg = Message Assistant finalContent
        appendMessages [actionMsg, assistantMsg]

        logDebug $ "[LLM] Assistant response:\n" <> result.trNarrative
        logDebug $ "[LLM] Tools invoked: " <> T.pack (show (length result.trToolsInvoked))

        return (TurnCompleted result)
  where
    -- The tool loop: call API, handle tools, repeat until done or broken
    -- Returns Left breakReason if a tool breaks, Right (result, content) otherwise
    toolLoop
      :: Client.ClientConfig
      -> Text              -- System prompt (dynamic per turn)
      -> Maybe Value
      -> [Value]
      -> [Message]
      -> [ToolInvocation]
      -> [Text]
      -> [Text]
      -> Eff es (Either Text (TurnResult Value, [ContentBlock]))
    toolLoop cConfig sysPrompt outSchema tls msgs invs narrs thinks = do
      -- Make API call
      let req = SingleCallRequest
            { scrMessages = msgs
            , scrSystemPrompt = Just sysPrompt  -- Dynamic system prompt!
            , scrOutputSchema = outSchema
            , scrTools = tls
            , scrThinkingBudget = config.llmThinkingBudget
            }
      apiResult <- liftIO $ Client.callMessagesOnce cConfig req

      case apiResult of
        Left err -> error $ "LLM API error: " <> show err
        Right resp -> processResponse cConfig sysPrompt outSchema tls msgs resp invs narrs thinks

    -- Process a response: either done, broken, or execute tools and continue
    -- Returns Left breakReason if broken, Right (result, content) otherwise
    processResponse
      :: Client.ClientConfig
      -> Text              -- System prompt (for recursive calls)
      -> Maybe Value
      -> [Value]
      -> [Message]
      -> SingleCallResponse
      -> [ToolInvocation]
      -> [Text]
      -> [Text]
      -> Eff es (Either Text (TurnResult Value, [ContentBlock]))
    processResponse cConfig sysPrompt outSchema tls msgs resp invs narrs thinks = do
      let content = resp.scrContent

          -- Extract text and thinking blocks
          newNarratives = [t | TextBlock t <- content]
          newThinkings = [tc.thinkingText | ThinkingBlock tc <- content]

          allNarratives = narrs ++ newNarratives
          allThinkings = thinks ++ newThinkings

      case resp.scrStopReason of
        EndTurn -> do
          -- Try final response first, then accumulated narratives
          let finalOutput = extractFinalOutputWithFallback content allNarratives
              turnResult = TurnResult
                { trOutput = finalOutput
                , trToolsInvoked = invs
                , trNarrative = T.intercalate "\n\n" allNarratives
                , trThinking = T.intercalate "\n\n" allThinkings
                }
          pure $ Right (turnResult, content)

        ToolUseStop -> do
          -- Execute tools using the dispatcher (in effects!)
          toolExecResult <- executeToolsWithDispatcher resp.scrToolUses
          case toolExecResult of
            -- A tool broke the turn - propagate up
            Left breakReason -> pure $ Left breakReason

            -- All tools completed - continue loop
            Right (newInvocations, toolResults) -> do
              let allInvocations = invs ++ newInvocations

              -- Build continuation messages
              let assistantMsg = Message Assistant content
                  userMsg = Message User (map ToolResultBlock toolResults)
                  newMessages = msgs ++ [assistantMsg, userMsg]

              -- Continue the loop
              toolLoop cConfig sysPrompt outSchema tls newMessages allInvocations allNarratives allThinkings

        MaxTokens ->
          error "Response hit max tokens limit"

        StopSequence -> do
          let finalOutput = extractFinalOutputWithFallback content allNarratives
              turnResult = TurnResult
                { trOutput = finalOutput
                , trToolsInvoked = invs
                , trNarrative = T.intercalate "\n\n" allNarratives
                , trThinking = T.intercalate "\n\n" allThinkings
                }
          pure $ Right (turnResult, content)

        Refusal ->
          error "Model refused to respond (safety)"

        PauseTurn ->
          error "Server tool pause not supported"

    -- Execute tools using the dispatcher
    -- Returns: Left breakReason if any tool breaks, Right (invocations, results) otherwise
    executeToolsWithDispatcher :: [ToolUse] -> Eff es (Either Text ([ToolInvocation], [Client.ToolResult]))
    executeToolsWithDispatcher uses = go uses [] []
      where
        go [] invs results = pure $ Right (reverse invs, reverse results)
        go (use:rest) invs results = do
          toolResult <- dispatcher use.toolName use.toolInput
          case toolResult of
            -- Tool error - report back to LLM and continue
            Left err ->
              let inv = ToolInvocation
                    { tiName = use.toolName
                    , tiInput = use.toolInput
                    , tiOutput = toJSON err
                    }
                  res = Client.ToolResult
                    { Client.toolResultId = use.toolUseId
                    , Client.toolResultContent = err
                    , Client.toolResultIsError = True
                    }
              in go rest (inv:invs) (res:results)

            -- Tool broke the turn - stop immediately
            Right (ToolBreak reason) ->
              pure $ Left reason

            -- Tool succeeded - continue
            Right (ToolSuccess val) ->
              let inv = ToolInvocation
                    { tiName = use.toolName
                    , tiInput = use.toolInput
                    , tiOutput = val
                    }
                  res = Client.ToolResult
                    { Client.toolResultId = use.toolUseId
                    , Client.toolResultContent = encodeText val
                    , Client.toolResultIsError = False
                    }
              in go rest (inv:invs) (res:results)

    encodeText :: Value -> Text
    encodeText v = case v of
      String t -> t
      _ -> TE.decodeUtf8 . LBS.toStrict . encode $ v

    -- Extract structured output, with fallback to accumulated narratives
    -- This handles the case where JSON is in an earlier response (before tool use)
    extractFinalOutputWithFallback :: [ContentBlock] -> [Text] -> Value
    extractFinalOutputWithFallback blocks allNarrs =
      case extractFromBlocks blocks of
        Just v -> v
        Nothing ->
          -- Fallback: try to parse any accumulated narrative as JSON
          case mapMaybe tryParseJson allNarrs of
            (parsed:_) -> parsed
            [] -> toJSON ()

    extractFromBlocks :: [ContentBlock] -> Maybe Value
    extractFromBlocks blocks =
      -- First, look for a non-empty JsonBlock (from output_format structured output)
      case [v | JsonBlock v <- blocks, not (isEmptyJson v)] of
        (jsonVal:_) -> Just jsonVal
        [] ->
          -- Fall back to parsing text blocks as JSON (try each one)
          case mapMaybe tryParseJson [t | TextBlock t <- blocks] of
            (parsed:_) -> Just parsed
            [] -> Nothing
      where
        isEmptyJson (Array arr) = V.null arr
        isEmptyJson (Object obj) = KM.null obj
        isEmptyJson Null = True
        isEmptyJson _ = False

    tryParseJson :: Text -> Maybe Value
    tryParseJson t = decode (LBS.fromStrict $ TE.encodeUtf8 t)

-- ══════════════════════════════════════════════════════════════
-- CHAT HISTORY EFFECT
-- ══════════════════════════════════════════════════════════════

-- | Effect for storing and retrieving chat history
-- Abstracts the storage mechanism (IORef, database, etc.)
data ChatHistory :: Effect where
  GetHistory :: ChatHistory m [Message]
  AppendMessages :: [Message] -> ChatHistory m ()
  ClearHistory :: ChatHistory m ()

type instance DispatchOf ChatHistory = 'Dynamic

-- | Get the current conversation history
getHistory :: ChatHistory :> es => Eff es [Message]
getHistory = send GetHistory

-- | Append messages to the conversation history
appendMessages :: ChatHistory :> es => [Message] -> Eff es ()
appendMessages = send . AppendMessages

-- | Clear all conversation history
clearHistory :: ChatHistory :> es => Eff es ()
clearHistory = send ClearHistory

-- | Run the ChatHistory effect using an IORef for storage
runChatHistory :: IOE :> es => Eff (ChatHistory : es) a -> Eff es a
runChatHistory action = do
  ref <- liftIO $ newIORef ([] :: [Message])
  runChatHistoryWith ref action

-- | Run ChatHistory with an explicit IORef (for sharing across effects)
runChatHistoryWith :: IOE :> es => IORef [Message] -> Eff (ChatHistory : es) a -> Eff es a
runChatHistoryWith ref = interpret $ \_ -> \case
  GetHistory -> liftIO $ readIORef ref
  AppendMessages msgs -> liftIO $ modifyIORef ref (++ msgs)
  ClearHistory -> liftIO $ writeIORef ref []

-- | Run the ChatHistory effect backed by SQLite database
-- Loads initial history from DB on start (respecting compression cursor),
-- caches in memory for fast reads during turn, and persists new messages to DB.
runChatHistoryWithDB
  :: IOE :> es
  => Connection
  -> Storage.GameId
  -> Maybe Int         -- ^ Compression cursor (load only messages after this sequence)
  -> Eff (ChatHistory : es) a
  -> Eff es a
runChatHistoryWithDB conn gameId mCursor action = do
  -- Load initial history from database (respecting compression cursor)
  initialHistory <- liftIO $ case mCursor of
    Nothing     -> Storage.loadMessages conn gameId
    Just cursor -> Storage.loadMessagesAfter conn gameId cursor

  -- Use an IORef for in-memory cache (fast reads during turn)
  ref <- liftIO $ newIORef initialHistory

  interpret (\_ -> \case
    GetHistory -> liftIO $ readIORef ref

    AppendMessages msgs -> liftIO $ do
      -- Update in-memory cache
      modifyIORef ref (++ msgs)
      -- Persist to database
      Storage.appendMessages conn gameId msgs

    ClearHistory -> liftIO $ writeIORef ref []
    ) action

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

-- | Effect for tools that need external input from the player
-- Interpreter handles: UI presentation, input collection, validation
data RequestInput :: Effect where
  -- | Present choices to player, get their selection
  RequestChoice
    :: Text              -- Prompt to display
    -> [(Text, a)]       -- (label, value) pairs
    -> RequestInput m a

  -- | Request free-form text input
  RequestText
    :: Text              -- Prompt to display
    -> RequestInput m Text

  -- | Request dice selection from player
  -- Returns the index of the selected die
  RequestDice
    :: Text              -- Prompt to display
    -> [(Int, Int)]      -- (die value, index) pairs
    -> RequestInput m Int

type instance DispatchOf RequestInput = 'Dynamic

-- | Present a choice to the player and get their selection
requestChoice :: RequestInput :> es => Text -> [(Text, a)] -> Eff es a
requestChoice prompt choices = send (RequestChoice prompt choices)

-- | Request free-form text input from the player
requestText :: RequestInput :> es => Text -> Eff es Text
requestText = send . RequestText

-- | Request dice selection from player
-- Takes a prompt and (die value, index) pairs, returns the selected index
requestDice :: RequestInput :> es => Text -> [(Int, Int)] -> Eff es Int
requestDice prompt diceWithIndices = send (RequestDice prompt diceWithIndices)

-- | Handler for input requests (terminal, UI, etc.)
data InputHandler = InputHandler
  { ihChoice :: forall a. Text -> [(Text, a)] -> IO a
  , ihText   :: Text -> IO Text
  , ihDice   :: Text -> [(Int, Int)] -> IO Int
    -- ^ Dice selection: prompt, (die value, index) pairs -> selected index
  }

runRequestInput :: IOE :> es => InputHandler -> Eff (RequestInput : es) a -> Eff es a
runRequestInput (InputHandler choice txtInput dice) = interpret $ \_ -> \case
  RequestChoice prompt choices -> liftIO $ choice prompt choices
  RequestText prompt           -> liftIO $ txtInput prompt
  RequestDice prompt diceWithIndices -> liftIO $ dice prompt diceWithIndices

-- ══════════════════════════════════════════════════════════════
-- LOG EFFECT
-- ══════════════════════════════════════════════════════════════

-- | Log levels for the logging effect
data LogLevel = Debug | Info | Warn
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Logging effect for debug/info/warn output
data Log :: Effect where
  LogMsg :: LogLevel -> Text -> Log m ()

type instance DispatchOf Log = 'Dynamic

-- | Log a message at the given level
logMsg :: Log :> es => LogLevel -> Text -> Eff es ()
logMsg level msg = send (LogMsg level msg)

-- | Log a debug message
logDebug :: Log :> es => Text -> Eff es ()
logDebug = logMsg Debug

-- | Log an info message
logInfo :: Log :> es => Text -> Eff es ()
logInfo = logMsg Info

-- | Log a warning message
logWarn :: Log :> es => Text -> Eff es ()
logWarn = logMsg Warn

-- | Run the Log effect, outputting to IO with level filtering
-- Only logs messages at or above the given minimum level
runLog :: IOE :> es => LogLevel -> Eff (Log : es) a -> Eff es a
runLog minLevel = interpret $ \_ -> \case
  LogMsg level msg
    | level >= minLevel -> liftIO $ do
        let prefix = case level of
              Debug -> "[DEBUG] "
              Info  -> "[INFO]  "
              Warn  -> "[WARN]  "
        -- TIO.putStrLn (prefix <> msg)
        return ()
    | otherwise -> pure ()

-- | Run the Log effect, logging to GUI bridge debug panel
-- This makes effect-based logs appear in the GUI debug panel
runLogWithBridge
  :: IOE :> es
  => GUIBridge state
  -> LogLevel
  -> Eff (Log : es) a
  -> Eff es a
runLogWithBridge bridge minLevel = interpret $ \_ -> \case
  LogMsg level msg
    | level >= minLevel -> liftIO $ do
        let guiLevel = case level of
              Debug -> GUICore.Debug
              Info  -> GUICore.Info
              Warn  -> GUICore.Warn
        GUICore.addDebugEntry bridge guiLevel msg Nothing
    | otherwise -> pure ()

-- | Outcome of running a turn that may be interrupted
data TurnOutcome a
  = TurnCompleted a           -- Turn finished normally
  | TurnBroken Text           -- Turn was interrupted (reason)
  deriving (Show, Eq, Functor)

-- Note: runBreakTurn is not needed because breaking is handled via ToolResult
-- in the LLM tool loop. The BreakTurn effect is provided for tools that need
-- to signal a break - they should return ToolBreak from their dispatcher.

-- | Result of executing a tool during an LLM turn
data ToolResult
  = ToolSuccess Value   -- Tool succeeded, return value to LLM
  | ToolBreak Text      -- Tool signals to break turn (reason)
  deriving (Show, Eq)

-- ══════════════════════════════════════════════════════════════
-- COMBINED RUNNER
-- ══════════════════════════════════════════════════════════════

-- | Run the full game effect stack
runGame
  :: s
  -> LLMConfig
  -> (event -> IO ())
  -> InputHandler
  -> LogLevel              -- ^ Minimum log level to display
  -> Eff (GameEffects s event) a
  -> IO (a, s)
runGame initialState llmConfig eventHandler inputHandler minLogLevel computation =
  runEff
    . runRandom
    . runEmit eventHandler
    . runState initialState
    . runChatHistory
    . runLog minLogLevel
    . runRequestInput inputHandler
    . runLLM llmConfig
    $ computation

-- NOTE: runGameWithTools is commented out due to effect stack type issues
-- Users should compose the runners manually when using runLLMWithTools:
--
-- Example usage with DM tools:
-- @
-- let dispatcher = makeDispatcher dmToolList
-- runEff
--   . runRandom
--   . runEmit eventHandler
--   . runState initialState
--   . runRequestInput inputHandler
--   . runLLMWithTools llmConfig dispatcher
--   $ computation
-- @
