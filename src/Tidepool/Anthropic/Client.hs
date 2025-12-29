-- | Higher-level Anthropic client with tool loop abstraction
-- Wraps the core HTTP layer with tool execution and response processing
module Tidepool.Anthropic.Client
  ( -- * Configuration
    ClientConfig(..)

    -- * Request/Response Types
  , TurnRequest(..)
  , TurnResponse(..)
  , ToolInvocation(..)

    -- * Single Call (for effect-layer tool loop)
  , SingleCallRequest(..)
  , SingleCallResponse(..)
  , callMessagesOnce

    -- * Re-exports from Http layer
  , StopReason(..)
  , ToolUse(..)
  , Message(..)
  , Role(..)
  , ContentBlock(..)
  , ImageSource(..)
  , ToolResult(..)
  , ThinkingContent(..)
  , RedactedThinking(..)

    -- * Operations (with IO tool executor)
  , runTurnRequest
  ) where

import Tidepool.Anthropic.Http
  ( MessagesRequest(..), MessagesResponse(..), Message(..), Role(..)
  , ContentBlock(..), ImageSource(..), ToolUse(..), ToolResult(..), ToolChoice(..)
  , OutputFormat(..), ThinkingConfig(..), ThinkingContent(..), RedactedThinking(..)
  , StopReason(..), ApiError(..)
  , callMessages
  )

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS
import GHC.Generics (Generic)

-- ══════════════════════════════════════════════════════════════
-- CONFIGURATION
-- ══════════════════════════════════════════════════════════════

-- | Client configuration
data ClientConfig = ClientConfig
  { apiKey :: Text
  , defaultModel :: Text
  , defaultMaxTokens :: Int
  }
  deriving (Show, Eq, Generic)

-- ══════════════════════════════════════════════════════════════
-- REQUEST/RESPONSE TYPES
-- ══════════════════════════════════════════════════════════════

-- | A complete turn request
-- Abstracts over the raw API to provide a simpler interface
data TurnRequest = TurnRequest
  { prompt :: Text                           -- Rendered template (becomes user message)
  , priorMessages :: [Message]               -- Prior conversation history to prepend
  , systemPrompt :: Maybe Text               -- System message
  , outputSchema :: Maybe Value              -- JSON Schema for structured output
  , tools :: [Value]                         -- Tool definitions
  , toolExecutor :: Text -> Value -> IO (Either Text Value)  -- Execute tool by name
  , thinkingBudget :: Maybe Int              -- Token budget for extended thinking
  }

-- | A complete turn response
data TurnResponse = TurnResponse
  { output :: Value                          -- The final JSON response (after tools)
  , narrative :: Text                        -- Concatenated text blocks
  , thinking :: Text                         -- Concatenated thinking blocks (if enabled)
  , toolsInvoked :: [ToolInvocation]         -- Record of tool calls made
  }
  deriving (Show, Eq, Generic)

-- | Record of a single tool invocation
data ToolInvocation = ToolInvocation
  { invocationName :: Text
  , invocationInput :: Value
  , invocationOutput :: Value
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- ══════════════════════════════════════════════════════════════
-- SINGLE CALL TYPES (for effect-layer tool loop)
-- ══════════════════════════════════════════════════════════════

-- | Request for a single API call (no tool loop)
-- Used when the tool loop is handled at the effect layer
data SingleCallRequest = SingleCallRequest
  { scrMessages :: [Message]              -- Full conversation history
  , scrSystemPrompt :: Maybe Text         -- System message
  , scrOutputSchema :: Maybe Value        -- JSON Schema for structured output
  , scrTools :: [Value]                   -- Tool definitions
  , scrThinkingBudget :: Maybe Int        -- Token budget for extended thinking
  }

-- | Response from a single API call
data SingleCallResponse = SingleCallResponse
  { scrContent :: [ContentBlock]          -- All content blocks
  , scrStopReason :: StopReason           -- Why the model stopped
  , scrToolUses :: [ToolUse]              -- Tool use requests (if any)
  }

-- | Make a single API call (no tool loop)
-- Returns the response for the effect layer to handle
callMessagesOnce
  :: ClientConfig
  -> SingleCallRequest
  -> IO (Either ApiError SingleCallResponse)
callMessagesOnce config req = do
  -- Build output format if schema provided
  let outputFmt = case req.scrOutputSchema of
        Nothing -> Nothing
        Just schema -> Just OutputFormat
          { outputType = "json_schema"
          , outputSchema = schema
          }

  -- Build thinking config if budget provided
  let thinkingCfg = case req.scrThinkingBudget of
        Nothing -> Nothing
        Just budget -> Just ThinkingConfig
          { thinkingType = "enabled"
          , budgetTokens = budget
          }

  -- Build request
  let httpReq = MessagesRequest
        { model = config.defaultModel
        , messages = req.scrMessages
        , maxTokens = config.defaultMaxTokens
        , system = req.scrSystemPrompt
        , tools = if null req.scrTools then Nothing else Just req.scrTools
        , toolChoice = if null req.scrTools then Nothing else Just ToolChoiceAuto
        , outputFormat = outputFmt
        , thinking = thinkingCfg
        }

  result <- callMessages config.apiKey httpReq
  pure $ case result of
    Left err -> Left err
    Right response -> Right SingleCallResponse
      { scrContent = response.responseContent
      , scrStopReason = response.responseStopReason
      , scrToolUses = [tu | ToolUseBlock tu <- response.responseContent]
      }

-- ══════════════════════════════════════════════════════════════
-- OPERATIONS (with IO tool executor)
-- ══════════════════════════════════════════════════════════════

-- | Run a complete turn with tool loop
-- Handles: initial request → tool calls → continue → ... → final response
-- NOTE: For full effect support, use callMessagesOnce and implement the loop in the effect layer
runTurnRequest
  :: ClientConfig
  -> TurnRequest
  -> IO (Either ApiError TurnResponse)
runTurnRequest config turnReq = do
  -- Build output format if schema provided
  let outputFmt = case turnReq.outputSchema of
        Nothing -> Nothing
        Just schema -> Just OutputFormat
          { outputType = "json_schema"
          , outputSchema = schema
          }

  -- Build thinking config if budget provided
  let thinkingCfg = case turnReq.thinkingBudget of
        Nothing -> Nothing
        Just budget -> Just ThinkingConfig
          { thinkingType = "enabled"
          , budgetTokens = budget
          }

  -- Build initial request
  -- Note: when thinking is enabled, tool_choice must be "auto" or "none"
  -- Build messages with prior history + new user message
  let allMessages = turnReq.priorMessages ++ [Message User [TextBlock turnReq.prompt]]
      initialRequest = MessagesRequest
        { model = config.defaultModel
        , messages = allMessages
        , maxTokens = config.defaultMaxTokens
        , system = turnReq.systemPrompt
        , tools = if null turnReq.tools then Nothing else Just turnReq.tools
        , toolChoice = if null turnReq.tools then Nothing else Just ToolChoiceAuto
        , outputFormat = outputFmt
        , thinking = thinkingCfg
        }

  -- Run the conversation loop
  loop initialRequest [] [] []

  where
    -- The tool loop: call API, handle tools, repeat until done
    -- Tracks: invocations, narratives (text), and thinking content
    loop :: MessagesRequest -> [ToolInvocation] -> [Text] -> [Text] -> IO (Either ApiError TurnResponse)
    loop request invocations narratives thinkings = do
      result <- callMessages config.apiKey request
      case result of
        Left err -> pure $ Left err
        Right response -> processResponse request response invocations narratives thinkings

    -- Process a response: either done, or need to execute tools and continue
    processResponse
      :: MessagesRequest
      -> MessagesResponse
      -> [ToolInvocation]
      -> [Text]
      -> [Text]
      -> IO (Either ApiError TurnResponse)
    processResponse request response invocations narratives thinkings = do
      let content = response.responseContent

          -- Extract text blocks for narrative
          newNarratives = [t | TextBlock t <- content]

          -- Extract thinking blocks (extended thinking content)
          newThinkings = [tc.thinkingText | ThinkingBlock tc <- content]

          -- Extract tool use requests
          toolUses = [tu | ToolUseBlock tu <- content]

          -- Accumulate
          allNarratives = narratives ++ newNarratives
          allThinkings = thinkings ++ newThinkings

      case response.responseStopReason of
        EndTurn -> do
          -- Done - extract the final output
          -- Look for the last JSON block in the response
          let finalOutput = extractFinalOutput content
          pure $ Right TurnResponse
            { output = finalOutput
            , narrative = T.intercalate "\n\n" allNarratives
            , thinking = T.intercalate "\n\n" allThinkings
            , toolsInvoked = invocations
            }

        ToolUseStop -> do
          -- Need to execute tools and continue
          (newInvocations, toolResults) <- executeTools toolUses
          let allInvocations = invocations ++ newInvocations

          -- Build continuation request with tool results
          -- IMPORTANT: Preserve full content including thinking blocks
          let assistantMsg = Message Assistant content
              userMsg = Message User (map ToolResultBlock toolResults)
              newRequest = request
                { messages = request.messages ++ [assistantMsg, userMsg]
                }

          loop newRequest allInvocations allNarratives allThinkings

        MaxTokens ->
          pure $ Left $ HttpError "Response hit max tokens limit"

        StopSequence ->
          -- Treat as end of turn
          pure $ Right TurnResponse
            { output = extractFinalOutput content
            , narrative = T.intercalate "\n\n" allNarratives
            , thinking = T.intercalate "\n\n" allThinkings
            , toolsInvoked = invocations
            }

        Refusal ->
          -- Model refused for safety reasons
          pure $ Left $ HttpError "Model refused to respond (safety)"

        PauseTurn ->
          -- Server tool pause - treat as error for now
          -- (would need to handle server tools to support this)
          pure $ Left $ HttpError "Server tool pause not supported"

    -- Execute a list of tool uses and return invocations + results
    executeTools :: [ToolUse] -> IO ([ToolInvocation], [ToolResult])
    executeTools uses = do
      results <- mapM executeSingle uses
      pure (map fst results, map snd results)
      where
        executeSingle use = do
          result <- turnReq.toolExecutor use.toolName use.toolInput
          let (outputVal, resultText, isError) = case result of
                Right v -> (v, encodeText v, False)
                Left err -> (Null, err, True)
              invocation = ToolInvocation
                { invocationName = use.toolName
                , invocationInput = use.toolInput
                , invocationOutput = outputVal
                }
              toolResult = ToolResult
                { toolResultId = use.toolUseId
                , toolResultContent = resultText
                , toolResultIsError = isError
                }
          pure (invocation, toolResult)

    -- Encode a Value to Text for tool result content
    encodeText :: Value -> Text
    encodeText v = case v of
      String t -> t
      _ -> TE.decodeUtf8 . LBS.toStrict . encode $ v

-- | Extract the final output from response content
-- First looks for JsonBlock (structured output), then falls back to parsing text
extractFinalOutput :: [ContentBlock] -> Value
extractFinalOutput blocks =
  -- First, look for a JsonBlock (from output_format structured output)
  case [v | JsonBlock v <- blocks] of
    (jsonVal:_) -> jsonVal
    [] ->
      -- Fall back to parsing the last text block as JSON
      case [t | TextBlock t <- reverse blocks] of
        [] -> Null
        (lastText:_) ->
          case decode (LBS.fromStrict $ TE.encodeUtf8 lastText) of
            Just v -> v
            Nothing -> String lastText  -- If not valid JSON, return as string
