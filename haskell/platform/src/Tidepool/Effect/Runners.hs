{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
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
  , runLLMForCompression
  , ToolDispatcher
    -- * Compression Types
  , NotMember
  , CompressionEffects
  , CompressionDispatcher
  , CompressionConfig(..)
  , ChatHistoryConfig(..)
  , defaultCompressionPrompt
  , defaultCompressionSchema
  , defaultCompressionConfig
  , defaultChatHistoryConfig
    -- * Chat History with DB
  , runChatHistoryWithDB
    -- * Chat History with Compression
  , runChatHistoryWithCompression
    -- * Combined Runner
  , runGame
  ) where

import Control.Monad.Freer (Eff, Member, interpret, sendM, LastMember, runM)
import Data.Kind (Constraint, Type)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (Value(..), toJSON, encode, decode)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef)
import Data.Maybe (mapMaybe)
import Database.SQLite.Simple (Connection)
import GHC.TypeLits (TypeError, ErrorMessage(..))

import qualified Tidepool.Anthropic.Client as Client
import Tidepool.Anthropic.Client
  ( SingleCallRequest(..), SingleCallResponse(..)
  , StopReason(..), ToolUse(..)
  )
import Tidepool.Anthropic.Types (toolUseToResultId)
import qualified Tidepool.Storage as Storage

import Tidepool.Effect.Types

-- | The effect stack for runners (interpreters).
-- IO must be the last member for freer-simple's 'LastMember' constraint.
type RunnerEffects s event =
  '[ LLM
   , RequestInput
   , Log
   , ChatHistory
   , State s
   , Emit event
   , Random
   , Time
   , IO
   ]

-- ToolDispatcher is re-exported from Tidepool.Effect.Types

-- ══════════════════════════════════════════════════════════════
-- TYPE-LEVEL EFFECT MEMBERSHIP CHECK
-- ══════════════════════════════════════════════════════════════

-- | Type family that produces a compile error if an effect is present in the stack.
-- Used to enforce that compression LLM cannot access ChatHistory (prevents recursion).
-- In freer-simple, effects have kind (Type -> Type).
type family NotMember (e :: Type -> Type) (es :: [Type -> Type]) :: Constraint where
  NotMember e '[] = ()
  NotMember e (e : _) = TypeError
    ('Text "Effect " ':<>: 'ShowType e ':<>: 'Text " must not be in effect stack for compression")
  NotMember e (_ : es) = NotMember e es

-- ══════════════════════════════════════════════════════════════
-- TOOL EXECUTION OUTCOMES
-- ══════════════════════════════════════════════════════════════

-- | Result of executing tools (untyped for internal use)
data ToolExecOutcome
  = ToolExecContinue [ToolInvocation] [Client.ToolResult]
  | ToolExecBreak Text
  | ToolExecTransition Text Value  -- target name + payload (untyped)

-- | Result of tool loop execution (may break or transition)
data ToolLoopResult
  = ToolLoopSuccess (TurnResult Value) [ContentBlock]
  | ToolLoopBroke Text
  | ToolLoopTransitioned Text Value  -- target name + payload

-- ══════════════════════════════════════════════════════════════
-- COMPRESSION TYPES
-- ══════════════════════════════════════════════════════════════

-- | Effects available to compression tools (NO ChatHistory, NO Goto).
-- This restricted stack prevents recursion in the compression LLM.
-- IO is accessed via 'LastMember IO effs', not included in the list.
type CompressionEffects state event =
  '[ State state, Emit event, RequestInput, Random, Log ]

-- | Tool dispatcher for compression - polymorphic in effect stack.
-- The actual effect constraints are enforced at call site via 'runLLMForCompression'.
-- Uses empty target list since compression doesn't support transitions.
type CompressionDispatcher es =
  Text -> Value -> Eff es (Either Text (ToolResult '[]))

-- | Configuration for the compression LLM
-- The 'es' type parameter is the effect stack available to tools.
data CompressionConfig es = CompressionConfig
  { ccLLMConfig   :: LLMConfig            -- ^ LLM configuration (can use cheaper model)
  , ccPrompt      :: Text                 -- ^ System prompt for compression
  , ccSchema      :: Value                -- ^ Output schema for compression
  , ccTools       :: [Value]              -- ^ Tool definitions (JSON)
  , ccDispatcher  :: CompressionDispatcher es  -- ^ Tool dispatcher
  }

-- | Configuration for automatic chat history compression
-- The 'es' type parameter is the effect stack available to compression tools.
data ChatHistoryConfig es = ChatHistoryConfig
  { chcTokenThreshold :: Int                 -- ^ Trigger compression when estimated tokens exceed this
  , chcRecentToKeep   :: Int                 -- ^ Number of recent messages to preserve verbatim
  , chcCompression    :: CompressionConfig es -- ^ Compression LLM configuration
  }

-- | Default system prompt for compression
defaultCompressionPrompt :: Text
defaultCompressionPrompt = T.unlines
  [ "You are a conversation summarizer. Given a conversation history,"
  , "produce a compressed version that preserves:"
  , "1. Key facts and decisions made"
  , "2. Important context about characters/entities mentioned"
  , "3. The emotional tone and relationship dynamics"
  , "4. Any unresolved threads or pending actions"
  , ""
  , "Output a single summary that can replace the original conversation"
  , "while maintaining context for future turns."
  ]

-- | Default schema for compression output.
-- Expects a JSON object with a 'summary' field containing the compressed text.
defaultCompressionSchema :: Value
defaultCompressionSchema = Object $ KM.fromList
  [ ("type", String "object")
  , ("properties", Object $ KM.fromList
      [ ("summary", Object $ KM.fromList
          [ ("type", String "string")
          , ("description", String "A concise summary of the conversation that preserves key context")
          ])
      ])
  , ("required", Array $ V.fromList [String "summary"])
  ]

-- | Create a default compression config with no tools.
-- Uses a simple summarization prompt and schema.
defaultCompressionConfig :: LLMConfig -> CompressionConfig es
defaultCompressionConfig llmConfig = CompressionConfig
  { ccLLMConfig   = llmConfig
  , ccPrompt      = defaultCompressionPrompt
  , ccSchema      = defaultCompressionSchema
  , ccTools       = []  -- No tools by default
  , ccDispatcher  = \_ _ -> pure $ Right (ToolSuccess Null)  -- Stub dispatcher
  }

-- | Create a default chat history config.
-- Uses 8000 token threshold and keeps 8 recent messages.
defaultChatHistoryConfig :: LLMConfig -> ChatHistoryConfig es
defaultChatHistoryConfig llmConfig = ChatHistoryConfig
  { chcTokenThreshold = 8000
  , chcRecentToKeep   = 8
  , chcCompression    = defaultCompressionConfig llmConfig
  }

-- ══════════════════════════════════════════════════════════════
-- SHARED LLM CORE
-- ══════════════════════════════════════════════════════════════

-- | Operations for reading/writing chat history (optional for compression)
data ChatHistoryOps es = ChatHistoryOps
  { choGetHistory     :: Eff es [Message]
  , choAppendMessages :: [Message] -> Eff es ()
  }

-- | Shared LLM implementation - ChatHistory operations are optional.
-- This allows both normal LLM runners (with ChatHistory) and compression
-- LLM runners (without ChatHistory) to share the same core logic.
runLLMCore
  :: forall targets effs a.
     (LastMember IO effs, Member Log effs)
  => LLMHooks
  -> LLMConfig
  -> Maybe (ChatHistoryOps effs)  -- Nothing for compression, Just for normal
  -> (Text -> Value -> Eff effs (Either Text (ToolResult targets)))  -- Tool dispatcher
  -> Eff (LLM ': effs) a
  -> Eff effs a
runLLMCore hooks config mChatOps dispatcher = interpret $ \case
  RunTurnOp systemPrompt userContent schema tools -> do
    sendM hooks.onTurnStart

    priorHistory <- case mChatOps of
      Nothing -> pure []  -- Compression: no prior history
      Just ops -> ops.choGetHistory

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
      ToolLoopBroke breakReason -> do
        logInfo $ "[LLM] Turn broken by tool: " <> breakReason
        sendM hooks.onTurnEnd
        return (TurnBroken breakReason)

      ToolLoopTransitioned target payload -> do
        logInfo $ "[LLM] Tool initiated transition to " <> target
        sendM hooks.onTurnEnd
        return (TurnTransitionHint target payload)

      ToolLoopSuccess result finalContent -> do
        -- Only append to history if we have ChatHistory operations
        case mChatOps of
          Nothing -> pure ()
          Just ops -> do
            let assistantMsg = Message Assistant finalContent
            ops.choAppendMessages [actionMsg, assistantMsg]

        logDebug $ "[LLM] Assistant response:\n" <> result.trNarrative

        sendM hooks.onTurnEnd
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
      -> Eff effs ToolLoopResult
    toolLoop cConfig sysPrompt outSchema tls msgs invs narrs thinks = do
      let req = SingleCallRequest
            { scrMessages = msgs
            , scrSystemPrompt = Just sysPrompt
            , scrOutputSchema = outSchema
            , scrTools = tls
            , scrThinkingBudget = config.llmThinkingBudget
            }
      apiResult <- sendM $ Client.callMessagesOnce cConfig req

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
      -> Eff effs ToolLoopResult
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
          pure $ ToolLoopSuccess turnResult content

        ToolUseStop -> do
          toolExecResult <- executeToolsWithDispatcher resp.scrToolUses
          case toolExecResult of
            ToolExecBreak breakReason -> pure $ ToolLoopBroke breakReason
            ToolExecTransition target payload -> pure $ ToolLoopTransitioned target payload
            ToolExecContinue newInvocations toolResults -> do
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
          pure $ ToolLoopSuccess turnResult content
        Refusal -> error "Model refused to respond (safety)"
        PauseTurn -> error "Server tool pause not supported"

    executeToolsWithDispatcher :: [ToolUse] -> Eff effs ToolExecOutcome
    executeToolsWithDispatcher uses = go uses [] []
      where
        go [] invs results = pure $ ToolExecContinue (reverse invs) (reverse results)
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
                    { Client.toolResultId = toolUseToResultId use.toolUseId
                    , Client.toolResultContent = err
                    , Client.toolResultIsError = True
                    }
              in go rest (inv:invs) (res:results)
            Right (ToolBreak reason) -> pure $ ToolExecBreak reason
            Right (ToolSuccess val) ->
              let inv = ToolInvocation
                    { tiName = use.toolName
                    , tiInput = use.toolInput
                    , tiOutput = val
                    }
                  res = Client.ToolResult
                    { Client.toolResultId = toolUseToResultId use.toolUseId
                    , Client.toolResultContent = encodeText val
                    , Client.toolResultIsError = False
                    }
              in go rest (inv:invs) (res:results)
            Right (ToolTransition target payload) ->
              pure $ ToolExecTransition target payload

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

-- ══════════════════════════════════════════════════════════════
-- LLM RUNNERS
-- ══════════════════════════════════════════════════════════════

-- | Run the LLM effect by calling the Anthropic API
runLLM :: (LastMember IO effs, Member ChatHistory effs, Member Log effs) => LLMConfig -> Eff (LLM ': effs) a -> Eff effs a
runLLM config = interpret $ \case
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
          , Client.toolInterpreter = stubToolInterpreter
          , Client.thinkingBudget = config.llmThinkingBudget
          }

    result <- sendM $ Client.runTurnRequest clientConfig turnReq
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
    stubToolInterpreter :: Text -> Value -> IO (Either Text Value)
    stubToolInterpreter _name _input = pure $ Right (toJSON ())

    convertInvocation :: Client.ToolInvocation -> ToolInvocation
    convertInvocation inv = ToolInvocation
      { tiName = inv.invocationName
      , tiInput = inv.invocationInput
      , tiOutput = inv.invocationOutput
      }

-- | Run the LLM effect with full tool execution support
runLLMWithTools
  :: forall targets effs event a.
     (LastMember IO effs, Member (Emit event) effs, Member RequestInput effs, Member Random effs, Member ChatHistory effs, Member Log effs)
  => LLMConfig
  -> ToolDispatcher targets event effs
  -> Eff (LLM ': effs) a
  -> Eff effs a
runLLMWithTools config = runLLMWithToolsHooked @targets @effs @event @a noHooks config

-- | Run the LLM effect with hooks for lifecycle events
-- Uses shared 'runLLMCore' with ChatHistory operations enabled.
runLLMWithToolsHooked
  :: forall targets effs event a.
     (LastMember IO effs, Member (Emit event) effs, Member RequestInput effs, Member Random effs, Member ChatHistory effs, Member Log effs)
  => LLMHooks
  -> LLMConfig
  -> ToolDispatcher targets event effs
  -> Eff (LLM ': effs) a
  -> Eff effs a
runLLMWithToolsHooked hooks config dispatcher =
  let chatOps = ChatHistoryOps
        { choGetHistory = getHistory
        , choAppendMessages = appendMessages
        }
  in runLLMCore hooks config (Just chatOps) dispatcher

-- | Run the LLM effect for compression - NO ChatHistory access.
-- This is used during history compression to prevent infinite recursion.
-- The 'NotMember ChatHistory effs' constraint is a compile-time guarantee
-- that ChatHistory cannot be accessed by the compression LLM or its tools.
runLLMForCompression
  :: forall effs a.
     ( NotMember ChatHistory effs  -- Compile error if ChatHistory in scope
     , Member Log effs
     , LastMember IO effs
     )
  => LLMConfig
  -> (Text -> Value -> Eff effs (Either Text (ToolResult '[])))  -- Tool dispatcher
  -> Eff (LLM ': effs) a
  -> Eff effs a
runLLMForCompression config =
  runLLMCore noHooks config Nothing  -- Nothing = no ChatHistory ops

-- ══════════════════════════════════════════════════════════════
-- CHAT HISTORY RUNNERS
-- ══════════════════════════════════════════════════════════════

-- | Run the ChatHistory effect backed by SQLite database
runChatHistoryWithDB
  :: LastMember IO effs
  => Connection
  -> Storage.GameId
  -> Maybe Int
  -> Eff (ChatHistory ': effs) a
  -> Eff effs a
runChatHistoryWithDB conn gameId mCursor action = do
  initialHistory <- sendM $ case mCursor of
    Nothing     -> Storage.loadMessages conn gameId
    Just cursor -> Storage.loadMessagesAfter conn gameId cursor

  ref <- sendM $ newIORef initialHistory

  interpret (\case
    GetHistory -> sendM $ readIORef ref
    AppendMessages msgs -> sendM $ do
      modifyIORef ref (++ msgs)
      Storage.appendMessages conn gameId msgs
    ClearHistory -> sendM $ writeIORef ref []
    ) action

-- | Run the ChatHistory effect with automatic compression.
-- When the estimated token count exceeds the threshold, older messages
-- are compressed using an LLM call (with restricted effect stack).
--
-- The compression LLM has access to tools but NOT to ChatHistory,
-- which prevents infinite recursion.
--
-- __Note__: Compression is performed synchronously within 'AppendMessages'.
-- If the compression LLM call takes significant time (likely for API calls),
-- this will block the caller until compression completes.
runChatHistoryWithCompression
  :: forall effs a.
     ( Member LLM effs  -- LLM effect for compression (caller provides interpreter)
     , Member RequestInput effs
     , Member Random effs
     , Member Log effs
     , LastMember IO effs
     , NotMember ChatHistory effs  -- Ensure effs doesn't have ChatHistory
     )
  => ChatHistoryConfig effs
  -> Eff (ChatHistory ': effs) a
  -> Eff effs a
runChatHistoryWithCompression config action = do
  historyRef <- sendM $ newIORef ([] :: [Message])

  interpret (\case
    GetHistory -> sendM $ readIORef historyRef

    ClearHistory -> sendM $ writeIORef historyRef []

    AppendMessages newMsgs -> do
      currentHistory <- sendM $ readIORef historyRef
      let updatedHistory = currentHistory ++ newMsgs
          tokenCount = estimateTokens updatedHistory

      if tokenCount > config.chcTokenThreshold
        then do
          logInfo $ "[ChatHistory] Compressing: " <> T.pack (show tokenCount)
                 <> " tokens > " <> T.pack (show config.chcTokenThreshold) <> " threshold"
          compressed <- compressHistory config updatedHistory
          sendM $ writeIORef historyRef compressed
        else
          sendM $ writeIORef historyRef updatedHistory
    ) action

-- | Compress old messages using the compression LLM
compressHistory
  :: forall effs.
     ( Member LLM effs
     , Member RequestInput effs
     , Member Random effs
     , Member Log effs
     , LastMember IO effs
     , NotMember ChatHistory effs
     )
  => ChatHistoryConfig effs
  -> [Message]
  -> Eff effs [Message]
compressHistory config history = do
  let recentCount = config.chcRecentToKeep
      (toCompress, toKeep) = splitAt (length history - recentCount) history

  -- If nothing to compress, return as-is
  if null toCompress
    then pure history
    else do
      logDebug $ "[ChatHistory] Compressing " <> T.pack (show (length toCompress))
              <> " messages, keeping " <> T.pack (show (length toKeep)) <> " recent"

      -- Format messages for compression prompt
      let formattedHistory = formatMessagesForCompression toCompress
          userPrompt = "Compress this conversation:\n\n" <> formattedHistory

      -- Run compression LLM in restricted effect stack
      -- CRITICAL: This runs WITHOUT ChatHistory, preventing recursion
      compressionResult <- runCompressionLLM config userPrompt

      pure $ compressionResult ++ toKeep

-- | Run the compression LLM call.
-- Uses the LLM effect from the stack (caller provides interpreter).
-- The NotMember ChatHistory constraint ensures the compression LLM
-- cannot access chat history, preventing infinite recursion.
runCompressionLLM
  :: forall effs.
     ( Member LLM effs
     , Member Log effs
     , NotMember ChatHistory effs
     )
  => ChatHistoryConfig effs
  -> Text  -- User prompt with formatted messages
  -> Eff effs [Message]
runCompressionLLM config userPrompt = do
  let cc = config.chcCompression

  -- Use the LLM effect from the stack
  -- The caller provides the interpreter (real API or mock for tests)
  outcome <- runTurnContent cc.ccPrompt [TextBlock userPrompt] cc.ccSchema cc.ccTools
  pure $ case outcome of
    TurnBroken reason ->
      -- Fallback: just use a placeholder
      [Message Assistant [TextBlock $ "[Compressed history - interrupted: " <> reason <> "]"]]

    TurnTransitionHint target _payload ->
      -- Fallback: compression shouldn't trigger transitions
      [Message Assistant [TextBlock $ "[Compressed history - unexpected transition to " <> target <> "]"]]

    TurnCompleted parseResult -> case parseResult of
      TurnParsed tr ->
        -- Extract summary from the structured output
        case extractSummary tr.trOutput of
          Just summary -> [Message Assistant [TextBlock $ "[Compressed history]\n" <> summary]]
          Nothing -> [Message Assistant [TextBlock $ "[Compressed history]\n" <> tr.trNarrative]]
      TurnParseFailed{tpfNarrative = narr} ->
        -- Fallback on parse failure
        [Message Assistant [TextBlock $ "[Compressed history]\n" <> narr]]
  where
    -- Extract summary from structured output.
    -- Tries common field names: "summary", "coSummary", "text", "content"
    extractSummary :: Value -> Maybe Text
    extractSummary (Object obj) =
      let candidates = ["summary", "coSummary", "text", "content"]
      in foldr (\key acc -> case KM.lookup key obj of
          Just (String s) -> Just s
          _ -> acc) Nothing candidates
    extractSummary _ = Nothing

-- | Format messages for compression prompt.
-- Includes all content types to preserve context for the compressor.
formatMessagesForCompression :: [Message] -> Text
formatMessagesForCompression = T.intercalate "\n\n" . map formatMessage
  where
    formatMessage msg =
      let roleLabel = case msg.role of
            User -> "USER"
            Assistant -> "ASSISTANT"
          contentText = T.intercalate " " (map formatBlock msg.content)
      in roleLabel <> ": " <> contentText

    formatBlock :: ContentBlock -> Text
    formatBlock (TextBlock t) = t
    formatBlock (ImageBlock _) = "[IMAGE]"
    formatBlock (ToolUseBlock tu) = "[TOOL:" <> tu.toolName <> "]"
    formatBlock (ToolResultBlock tr) = "[TOOL_RESULT:" <> tr.toolResultContent <> "]"
    formatBlock (ThinkingBlock tc) = "[THINKING:" <> tc.thinkingText <> "]"
    formatBlock (RedactedThinkingBlock _) = "[REDACTED_THINKING]"
    formatBlock (JsonBlock _) = "[JSON]"

-- ══════════════════════════════════════════════════════════════
-- LOG AND GAME RUNNERS
-- ══════════════════════════════════════════════════════════════

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
  runM
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
