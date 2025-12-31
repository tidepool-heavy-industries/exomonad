{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | IO-based effect runners that need platform dependencies
--
-- These runners require HTTP, SQLite, or GUI - not WASM-compatible.
-- Use 'Tidepool.Effect.Types' in tidepool-core for pure effect definitions.
module Tidepool.Effect.Runners
  ( -- * Effect Stack
    RunnerEffects
    -- * LLM Runners
  , runLLM
  , runLLMWithTools
  , runLLMWithToolsHooked
  , ToolDispatcher
    -- * Chat History with DB
  , runChatHistoryWithDB
    -- * Log with GUI Bridge
  , runLogWithBridge
    -- * Combined Runner
  , runGame
  ) where

import Effectful
import Effectful.Dispatch.Dynamic
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (Value(..), ToJSON, toJSON, encode, decode)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Data.Maybe (mapMaybe)
import Database.SQLite.Simple (Connection)

import qualified Tidepool.Anthropic.Client as Client
import Tidepool.Anthropic.Client
  ( SingleCallRequest(..), SingleCallResponse(..)
  , StopReason(..), ToolUse(..), Message(..), ContentBlock(..)
  )
import Tidepool.Anthropic.Types (ThinkingContent(..))
import qualified Tidepool.GUI.Core as GUICore
import Tidepool.GUI.Core (GUIBridge)
import qualified Tidepool.Storage as Storage

import Tidepool.Effect.Types

-- | The effect stack for runners (interpreters).
type RunnerEffects s event =
  '[ LLM
   , RequestInput
   , Log
   , ChatHistory
   , State s
   , Emit event
   , Random
   , Time
   , IOE
   ]

-- ToolDispatcher is re-exported from Tidepool.Effect.Types

-- | Run the LLM effect by calling the Anthropic API
runLLM :: (IOE :> es, ChatHistory :> es, Log :> es) => LLMConfig -> Eff (LLM : es) a -> Eff es a
runLLM config = interpret $ \_ -> \case
  RunTurnOp systemPrompt userContent schema tools -> do
    priorHistory <- getHistory

    let userText = extractText userContent
    logDebug $ "[LLM] Prior history: " <> T.pack (show (length priorHistory)) <> " messages"
    logDebug $ "[LLM] User action: " <> userText

    let clientConfig = Client.ClientConfig
          { Client.apiKey = config.llmApiKey
          , Client.defaultModel = config.llmModel
          , Client.defaultMaxTokens = config.llmMaxTokens
          }
        outputSchema = if schema == toJSON () then Nothing else Just schema
        userMsg = Message User userContent
        turnReq = Client.TurnRequest
          { Client.prompt = userText
          , Client.priorMessages = priorHistory
          , Client.systemPrompt = Just systemPrompt
          , Client.outputSchema = outputSchema
          , Client.tools = tools
          , Client.toolExecutor = stubToolExecutor
          , Client.thinkingBudget = config.llmThinkingBudget
          }

    result <- liftIO $ Client.runTurnRequest clientConfig turnReq
    case result of
      Left err -> do
        logWarn $ "[LLM] API error: " <> T.pack (show err)
        pure $ TurnCompleted TurnResult
          { trOutput = toJSON ()
          , trToolsInvoked = []
          , trNarrative = "*The spirits of Doskvol are silent. The world waits.*"
          , trThinking = ""
          }
      Right resp -> do
        let assistantMsg = Message Assistant [TextBlock resp.narrative]
        appendMessages [userMsg, assistantMsg]

        logDebug $ "[LLM] Assistant response:\n" <> resp.narrative

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

-- | Run the LLM effect with full tool execution support
runLLMWithTools
  :: forall es event a.
     (IOE :> es, Emit event :> es, RequestInput :> es, Random :> es, ChatHistory :> es, Log :> es)
  => LLMConfig
  -> ToolDispatcher event es
  -> Eff (LLM : es) a
  -> Eff es a
runLLMWithTools = runLLMWithToolsHooked @es @event noHooks

-- | Run the LLM effect with hooks for lifecycle events
runLLMWithToolsHooked
  :: forall es event a.
     (IOE :> es, Emit event :> es, RequestInput :> es, Random :> es, ChatHistory :> es, Log :> es)
  => LLMHooks
  -> LLMConfig
  -> ToolDispatcher event es
  -> Eff (LLM : es) a
  -> Eff es a
runLLMWithToolsHooked hooks config dispatcher = interpret $ \_ -> \case
  RunTurnOp systemPrompt userContent schema tools -> do
    liftIO hooks.onTurnStart

    priorHistory <- getHistory

    let userText = extractText userContent
    logDebug $ "[LLM] Prior history: " <> T.pack (show (length priorHistory)) <> " messages"
    logDebug $ "[LLM] User action: " <> userText

    let clientConfig = Client.ClientConfig
          { Client.apiKey = config.llmApiKey
          , Client.defaultModel = config.llmModel
          , Client.defaultMaxTokens = config.llmMaxTokens
          }
        outputSchema = if schema == toJSON () then Nothing else Just schema
        actionMsg = Message User userContent
        initialMessages = priorHistory ++ [actionMsg]

    loopResult <- toolLoop clientConfig systemPrompt outputSchema tools initialMessages [] [] []

    case loopResult of
      Left breakReason -> do
        logInfo $ "[LLM] Turn broken by tool: " <> breakReason
        liftIO hooks.onTurnEnd
        return (TurnBroken breakReason)

      Right (result, finalContent) -> do
        let assistantMsg = Message Assistant finalContent
        appendMessages [actionMsg, assistantMsg]

        logDebug $ "[LLM] Assistant response:\n" <> result.trNarrative

        liftIO hooks.onTurnEnd
        return (TurnCompleted result)
  where
    toolLoop
      :: Client.ClientConfig
      -> Text
      -> Maybe Value
      -> [Value]
      -> [Message]
      -> [ToolInvocation]
      -> [Text]
      -> [Text]
      -> Eff es (Either Text (TurnResult Value, [ContentBlock]))
    toolLoop cConfig sysPrompt outSchema tls msgs invs narrs thinks = do
      let req = SingleCallRequest
            { scrMessages = msgs
            , scrSystemPrompt = Just sysPrompt
            , scrOutputSchema = outSchema
            , scrTools = tls
            , scrThinkingBudget = config.llmThinkingBudget
            }
      apiResult <- liftIO $ Client.callMessagesOnce cConfig req

      case apiResult of
        Left err -> error $ "LLM API error: " <> show err
        Right resp -> processResponse cConfig sysPrompt outSchema tls msgs resp invs narrs thinks

    processResponse
      :: Client.ClientConfig
      -> Text
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
          newNarratives = [t | TextBlock t <- content]
          newThinkings = [tc.thinkingText | ThinkingBlock tc <- content]
          allNarratives = narrs ++ newNarratives
          allThinkings = thinks ++ newThinkings

      case resp.scrStopReason of
        EndTurn -> do
          let finalOutput = extractFinalOutputWithFallback content allNarratives
              turnResult = TurnResult
                { trOutput = finalOutput
                , trToolsInvoked = invs
                , trNarrative = T.intercalate "\n\n" allNarratives
                , trThinking = T.intercalate "\n\n" allThinkings
                }
          pure $ Right (turnResult, content)

        ToolUseStop -> do
          toolExecResult <- executeToolsWithDispatcher resp.scrToolUses
          case toolExecResult of
            Left breakReason -> pure $ Left breakReason
            Right (newInvocations, toolResults) -> do
              let allInvocations = invs ++ newInvocations
                  assistantMsg = Message Assistant content
                  userMsg = Message User (map ToolResultBlock toolResults)
                  newMessages = msgs ++ [assistantMsg, userMsg]
              toolLoop cConfig sysPrompt outSchema tls newMessages allInvocations allNarratives allThinkings

        MaxTokens -> error "Response hit max tokens limit"
        StopSequence -> do
          let finalOutput = extractFinalOutputWithFallback content allNarratives
              turnResult = TurnResult
                { trOutput = finalOutput
                , trToolsInvoked = invs
                , trNarrative = T.intercalate "\n\n" allNarratives
                , trThinking = T.intercalate "\n\n" allThinkings
                }
          pure $ Right (turnResult, content)
        Refusal -> error "Model refused to respond (safety)"
        PauseTurn -> error "Server tool pause not supported"

    executeToolsWithDispatcher :: [ToolUse] -> Eff es (Either Text ([ToolInvocation], [Client.ToolResult]))
    executeToolsWithDispatcher uses = go uses [] []
      where
        go [] invs results = pure $ Right (reverse invs, reverse results)
        go (use:rest) invs results = do
          toolResult <- dispatcher use.toolName use.toolInput
          case toolResult of
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
            Right (ToolBreak reason) -> pure $ Left reason
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

    extractFinalOutputWithFallback :: [ContentBlock] -> [Text] -> Value
    extractFinalOutputWithFallback blocks allNarrs =
      case extractFromBlocks blocks of
        Just v -> v
        Nothing ->
          case mapMaybe tryParseJson allNarrs of
            (parsed:_) -> parsed
            [] -> toJSON ()

    extractFromBlocks :: [ContentBlock] -> Maybe Value
    extractFromBlocks blocks =
      case [v | JsonBlock v <- blocks, not (isEmptyJson v)] of
        (jsonVal:_) -> Just jsonVal
        [] ->
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

-- | Run the ChatHistory effect backed by SQLite database
runChatHistoryWithDB
  :: IOE :> es
  => Connection
  -> Storage.GameId
  -> Maybe Int
  -> Eff (ChatHistory : es) a
  -> Eff es a
runChatHistoryWithDB conn gameId mCursor action = do
  initialHistory <- liftIO $ case mCursor of
    Nothing     -> Storage.loadMessages conn gameId
    Just cursor -> Storage.loadMessagesAfter conn gameId cursor

  ref <- liftIO $ newIORef initialHistory

  interpret (\_ -> \case
    GetHistory -> liftIO $ readIORef ref
    AppendMessages msgs -> liftIO $ do
      modifyIORef ref (++ msgs)
      Storage.appendMessages conn gameId msgs
    ClearHistory -> liftIO $ writeIORef ref []
    ) action

-- | Run the Log effect, logging to GUI bridge debug panel
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

-- | Run the full game effect stack
runGame
  :: s
  -> LLMConfig
  -> (event -> IO ())
  -> InputHandler
  -> LogLevel
  -> Eff (RunnerEffects s event) a
  -> IO (a, s)
runGame initialState llmConfig eventHandler inputHandler minLogLevel =
  runEff
    . runTime
    . runRandom
    . runEmit eventHandler
    . runState initialState
    . runChatHistory
    . runLog minLogLevel
    . runRequestInput inputHandler
    . runLLM llmConfig

-- Helper: Extract text content from content blocks
extractText :: [ContentBlock] -> Text
extractText = T.intercalate " " . mapMaybe getText
  where
    getText (TextBlock t) = Just t
    getText _ = Nothing
