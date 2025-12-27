-- | Higher-level Anthropic client with tool loop abstraction
-- Wraps the core HTTP layer with tool execution and response processing
module Tidepool.Anthropic.Client
  ( -- * Configuration
    ClientConfig(..)

    -- * Request/Response Types
  , TurnRequest(..)
  , TurnResponse(..)
  , ToolInvocation(..)

    -- * Operations
  , runTurnRequest
  ) where

import Tidepool.Anthropic.Http
  ( MessagesRequest(..), MessagesResponse(..), Message(..), Role(..)
  , ContentBlock(..), ToolUse(..), ToolResult(..), ToolChoice(..)
  , OutputFormat(..), StopReason(..), ApiError(..)
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
  , systemPrompt :: Maybe Text               -- System message
  , outputSchema :: Maybe Value              -- JSON Schema for structured output
  , tools :: [Value]                         -- Tool definitions
  , toolExecutor :: Text -> Value -> IO (Either Text Value)  -- Execute tool by name
  }

-- | A complete turn response
data TurnResponse = TurnResponse
  { output :: Value                          -- The final JSON response (after tools)
  , narrative :: Text                        -- Concatenated text blocks
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
-- OPERATIONS
-- ══════════════════════════════════════════════════════════════

-- | Run a complete turn with tool loop
-- Handles: initial request → tool calls → continue → ... → final response
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

  -- Build initial request
  let initialRequest = MessagesRequest
        { model = config.defaultModel
        , messages = [Message User [TextBlock turnReq.prompt]]
        , maxTokens = config.defaultMaxTokens
        , system = turnReq.systemPrompt
        , tools = if null turnReq.tools then Nothing else Just turnReq.tools
        , toolChoice = if null turnReq.tools then Nothing else Just ToolChoiceAuto
        , outputFormat = outputFmt
        }

  -- Run the conversation loop
  loop initialRequest [] []

  where
    -- The tool loop: call API, handle tools, repeat until done
    loop :: MessagesRequest -> [ToolInvocation] -> [Text] -> IO (Either ApiError TurnResponse)
    loop request invocations narratives = do
      result <- callMessages config.apiKey request
      case result of
        Left err -> pure $ Left err
        Right response -> processResponse request response invocations narratives

    -- Process a response: either done, or need to execute tools and continue
    processResponse
      :: MessagesRequest
      -> MessagesResponse
      -> [ToolInvocation]
      -> [Text]
      -> IO (Either ApiError TurnResponse)
    processResponse request response invocations narratives = do
      let content = response.responseContent

          -- Extract text blocks for narrative
          newNarratives = [t | TextBlock t <- content]

          -- Extract tool use requests
          toolUses = [tu | ToolUseBlock tu <- content]

          -- Accumulate narratives
          allNarratives = narratives ++ newNarratives

      case response.responseStopReason of
        EndTurn -> do
          -- Done - extract the final output
          -- Look for the last JSON block in the response
          let finalOutput = extractFinalOutput content
          pure $ Right TurnResponse
            { output = finalOutput
            , narrative = T.intercalate "\n\n" allNarratives
            , toolsInvoked = invocations
            }

        ToolUseStop -> do
          -- Need to execute tools and continue
          (newInvocations, toolResults) <- executeTools toolUses
          let allInvocations = invocations ++ newInvocations

          -- Build continuation request with tool results
          let assistantMsg = Message Assistant content
              userMsg = Message User (map ToolResultBlock toolResults)
              newRequest = request
                { messages = request.messages ++ [assistantMsg, userMsg]
                }

          loop newRequest allInvocations allNarratives

        MaxTokens ->
          pure $ Left $ HttpError "Response hit max tokens limit"

        StopSequence ->
          -- Treat as end of turn
          pure $ Right TurnResponse
            { output = extractFinalOutput content
            , narrative = T.intercalate "\n\n" allNarratives
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
-- Looks for structured JSON in the response
extractFinalOutput :: [ContentBlock] -> Value
extractFinalOutput blocks =
  -- For now, just take the last text block and try to parse as JSON
  -- In practice, the model should return valid JSON in the text block
  case [t | TextBlock t <- reverse blocks] of
    [] -> Null
    (lastText:_) ->
      case decode (LBS.fromStrict $ TE.encodeUtf8 lastText) of
        Just v -> v
        Nothing -> String lastText  -- If not valid JSON, return as string
