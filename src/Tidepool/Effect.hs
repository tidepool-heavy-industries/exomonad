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
  , logMsg
  , logDebug
  , logInfo
  , logWarn
  , getHistory
  , appendMessages
  , clearHistory

    -- * Result Types
  , TurnResult(..)
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
  , runChatHistory

    -- * Tool Execution Types
  , ToolDispatcher
  , ToolResult(..)

    -- * Break Turn Types (for state machine transitions)
  , BreakTurn(..)
  , breakTurn
  , TurnOutcome(..)
  ) where

import Effectful
import Effectful.Dispatch.Dynamic
import qualified Effectful.State.Static.Local as EState
import System.Random (randomRIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Aeson (Value(..), FromJSON, ToJSON, fromJSON, toJSON, Result(..), encode)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS
import GHC.Generics (Generic)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import qualified Tidepool.Anthropic.Client as Client
import Tidepool.Anthropic.Client
  ( SingleCallRequest(..), SingleCallResponse(..)
  , StopReason(..), ToolUse(..), Message(..), Role(..), ContentBlock(..)
  )
-- Note: ToolResult is imported qualified as Client.ToolResult to avoid
-- collision with Tidepool.Effect.ToolResult
import Tidepool.Anthropic.Http (ThinkingContent(..))

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
    :: Text                          -- Rendered prompt
    -> Value                         -- Output schema (JSON)
    -> [Value]                       -- Tool definitions (JSON)
    -> LLM m (TurnOutcome (TurnResult Value))  -- May complete or break

type instance DispatchOf LLM = 'Dynamic

-- | Run a turn with a template and context
-- Returns TurnOutcome to allow caller to handle breaks
runTurn
  :: forall context output es.
     (LLM :> es, ToJSON context, FromJSON output)
  => (context -> Text)    -- render function
  -> Value                -- output schema
  -> [Value]              -- tool definitions
  -> context              -- context to render
  -> Eff es (TurnOutcome (TurnResult output))
runTurn render schema tools context = do
  let prompt = render context
  rawResult <- send (RunTurnOp prompt schema tools)
  -- Parse the raw JSON output if turn completed
  case rawResult of
    TurnBroken reason -> return (TurnBroken reason)
    TurnCompleted tr -> case fromJSON tr.trOutput of
      Success parsed -> return $ TurnCompleted tr { trOutput = parsed }
      Error err -> error $ "Failed to parse LLM output: " <> err

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
  , llmSystemPrompt :: Text         -- System prompt for all turns
  }
  deriving (Show, Eq, Generic)

-- | Run the LLM effect by calling the Anthropic API
-- Note: Tool execution uses a stub - for full tool support, use runLLMWithTools
-- Uses ChatHistory effect for conversation persistence and Log for debugging.
-- Always returns TurnCompleted since stub tools never break.
runLLM :: (IOE :> es, ChatHistory :> es, Log :> es) => LLMConfig -> Eff (LLM : es) a -> Eff es a
runLLM config = interpret $ \_ -> \case
  RunTurnOp prompt schema tools -> do
    -- Get prior conversation history
    priorHistory <- getHistory

    logDebug $ "[LLM] Prior history: " <> T.pack (show (length priorHistory)) <> " messages"
    logDebug $ "[LLM] User prompt:\n" <> prompt

    let clientConfig = Client.ClientConfig
          { Client.apiKey = config.llmApiKey
          , Client.defaultModel = config.llmModel
          , Client.defaultMaxTokens = config.llmMaxTokens
          }
        outputSchema = if schema == toJSON () then Nothing else Just schema
        turnReq = Client.TurnRequest
          { Client.prompt = prompt
          , Client.priorMessages = priorHistory
          , Client.systemPrompt = Just config.llmSystemPrompt
          , Client.outputSchema = outputSchema
          , Client.tools = tools
          , Client.toolExecutor = stubToolExecutor
          , Client.thinkingBudget = config.llmThinkingBudget
          }

    result <- liftIO $ Client.runTurnRequest clientConfig turnReq
    case result of
      Left err -> error $ "LLM API error: " <> show err
      Right resp -> do
        -- Build messages and append to history
        let userMsg = Message User [TextBlock prompt]
            assistantMsg = Message Assistant [TextBlock resp.narrative]
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
-- Uses config.llmSystemPrompt as the system prompt for all turns.
-- Returns TurnBroken if any tool signals a break (for state machine transitions).
runLLMWithTools
  :: forall es event a.
     (IOE :> es, Emit event :> es, RequestInput :> es, Random :> es, ChatHistory :> es, Log :> es)
  => LLMConfig
  -> ToolDispatcher event es
  -> Eff (LLM : es) a
  -> Eff es a
runLLMWithTools config dispatcher = interpret $ \_ -> \case
  RunTurnOp prompt schema tools -> do
    -- Get prior conversation history
    priorHistory <- getHistory

    logDebug $ "[LLM] Prior history: " <> T.pack (show (length priorHistory)) <> " messages"
    logDebug $ "[LLM] User prompt:\n" <> prompt

    let clientConfig = Client.ClientConfig
          { Client.apiKey = config.llmApiKey
          , Client.defaultModel = config.llmModel
          , Client.defaultMaxTokens = config.llmMaxTokens
          }
        outputSchema = if schema == toJSON () then Nothing else Just schema
        -- Build messages with prior history + new user message
        initialMessages = priorHistory ++ [Message User [TextBlock prompt]]

    -- Run the tool loop (may complete or break)
    loopResult <- toolLoop clientConfig outputSchema tools initialMessages [] [] []

    case loopResult of
      -- Tool broke the turn - don't append to history, return break signal
      Left breakReason -> do
        logInfo $ "[LLM] Turn broken by tool: " <> breakReason
        return (TurnBroken breakReason)

      -- Turn completed normally
      Right (result, finalContent) -> do
        -- Append the full conversation to history
        let userMsg = Message User [TextBlock prompt]
            assistantMsg = Message Assistant finalContent
        appendMessages [userMsg, assistantMsg]

        logDebug $ "[LLM] Assistant response:\n" <> result.trNarrative
        logDebug $ "[LLM] Tools invoked: " <> T.pack (show (length result.trToolsInvoked))

        return (TurnCompleted result)
  where
    -- The tool loop: call API, handle tools, repeat until done or broken
    -- Returns Left breakReason if a tool breaks, Right (result, content) otherwise
    toolLoop
      :: Client.ClientConfig
      -> Maybe Value
      -> [Value]
      -> [Message]
      -> [ToolInvocation]
      -> [Text]
      -> [Text]
      -> Eff es (Either Text (TurnResult Value, [ContentBlock]))
    toolLoop cConfig outSchema tls msgs invs narrs thinks = do
      -- Make API call
      let req = SingleCallRequest
            { scrMessages = msgs
            , scrSystemPrompt = Just config.llmSystemPrompt
            , scrOutputSchema = outSchema
            , scrTools = tls
            , scrThinkingBudget = config.llmThinkingBudget
            }
      apiResult <- liftIO $ Client.callMessagesOnce cConfig req

      case apiResult of
        Left err -> error $ "LLM API error: " <> show err
        Right resp -> processResponse cConfig outSchema tls msgs resp invs narrs thinks

    -- Process a response: either done, broken, or execute tools and continue
    -- Returns Left breakReason if broken, Right (result, content) otherwise
    processResponse
      :: Client.ClientConfig
      -> Maybe Value
      -> [Value]
      -> [Message]
      -> SingleCallResponse
      -> [ToolInvocation]
      -> [Text]
      -> [Text]
      -> Eff es (Either Text (TurnResult Value, [ContentBlock]))
    processResponse cConfig outSchema tls msgs resp invs narrs thinks = do
      let content = resp.scrContent

          -- Extract text and thinking blocks
          newNarratives = [t | TextBlock t <- content]
          newThinkings = [tc.thinkingText | ThinkingBlock tc <- content]

          allNarratives = narrs ++ newNarratives
          allThinkings = thinks ++ newThinkings

      case resp.scrStopReason of
        EndTurn -> do
          let finalOutput = extractFinalOutput content
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
              toolLoop cConfig outSchema tls newMessages allInvocations allNarratives allThinkings

        MaxTokens ->
          error "Response hit max tokens limit"

        StopSequence -> do
          let finalOutput = extractFinalOutput content
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

    extractFinalOutput :: [ContentBlock] -> Value
    extractFinalOutput blocks =
      case [t | TextBlock t <- reverse blocks] of
        [] -> toJSON ()
        (lastText:_) ->
          case fromJSON (toJSON lastText) of
            Success v -> v
            Error _ -> toJSON lastText

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

type instance DispatchOf RequestInput = 'Dynamic

-- | Present a choice to the player and get their selection
requestChoice :: RequestInput :> es => Text -> [(Text, a)] -> Eff es a
requestChoice prompt choices = send (RequestChoice prompt choices)

-- | Request free-form text input from the player
requestText :: RequestInput :> es => Text -> Eff es Text
requestText = send . RequestText

-- | Handler for input requests (terminal, UI, etc.)
data InputHandler = InputHandler
  { ihChoice :: forall a. Text -> [(Text, a)] -> IO a
  , ihText   :: Text -> IO Text
  }

runRequestInput :: IOE :> es => InputHandler -> Eff (RequestInput : es) a -> Eff es a
runRequestInput (InputHandler choiceHandler textHandler) = interpret $ \_ -> \case
  RequestChoice prompt choices -> liftIO $ choiceHandler prompt choices
  RequestText prompt           -> liftIO $ textHandler prompt

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
        TIO.putStrLn (prefix <> msg)
    | otherwise -> pure ()

-- ══════════════════════════════════════════════════════════════
-- BREAK TURN EFFECT (for state machine transitions)
-- ══════════════════════════════════════════════════════════════

-- | Effect for tools that need to break out of the current LLM turn.
-- Used by state transition tools (Engage, Resolve, Accept, etc.) to signal
-- that the mood has changed and the turn should restart with a new template.
data BreakTurn :: Effect where
  -- | Break out of the current turn with a reason
  -- This is a non-returning operation - it terminates the current turn
  BreakTurnOp :: Text -> BreakTurn m a

type instance DispatchOf BreakTurn = 'Dynamic

-- | Break out of the current LLM turn (for state machine transitions)
-- Call this from a transition tool after modifying the mood state.
-- The turn will restart with the new mood's template.
breakTurn :: BreakTurn :> es => Text -> Eff es a
breakTurn = send . BreakTurnOp

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
