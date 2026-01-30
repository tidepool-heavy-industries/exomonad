{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Conversion utilities between native types and wire types.
--
-- This module provides the bridge between:
-- - Native types ('ContentBlock', 'TurnResult') used in 'Member LLM effs' handlers
-- - Wire types ('WireMessage', 'WireContentBlock') used for WASM FFI
--
-- This enables handlers to use 'Member LLM effs' and work in both WASM and native
-- contexts, with the WASM interpreter converting to/from wire format.
module ExoMonad.Wasm.Conversion
  ( -- * Native → Wire (for sending to TypeScript)
    contentBlockToWire,
    contentBlocksToWireMessages,
    messageToWire,
    toToolResultOutcome,

    -- * Wire → Native (for receiving from TypeScript)
    wireContentBlockToNative,
    wireMessageToNative,
    wireMessagesToNative,
    fromToolResultOutcome,

    -- * TurnResult parsing (from TypeScript response)
    parseWireTurnResult,
    WireTurnResult (..),
  )
where

import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    Value (..),
    object,
    withObject,
    (.:),
    (.:?),
    (.=),
  )
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
-- Native types from exomonad-core

import ExoMonad.Anthropic.Types
  ( ContentBlock (..),
    Message (..),
    Role (..),
    ToolResultId (..),
    ToolUse (..),
    ToolUseId (..),
  )
import ExoMonad.Anthropic.Types qualified as Anthropic
import ExoMonad.Effect.Types
  ( ToolInvocation (..),
    TurnResult (..),
  )
import ExoMonad.Effect.Types qualified as Effect
-- Wire types
import ExoMonad.Wasm.WireTypes
  ( ToolResultOutcome (..),
    WireContentBlock (..),
    WireMessage (..),
  )
import GHC.Generics (Generic)

-- ════════════════════════════════════════════════════════════════════════════
-- NATIVE → WIRE (for sending to TypeScript)
-- ════════════════════════════════════════════════════════════════════════════

-- | Convert a native ContentBlock to wire format.
--
-- Returns 'Nothing' for blocks that shouldn't be sent over the wire:
-- - Thinking (model-internal)
-- - RedactedThinking (model-internal)
-- - Json (converted to text representation)
contentBlockToWire :: ContentBlock -> Maybe WireContentBlock
contentBlockToWire = \case
  Text {text = txt} ->
    Just $ WCBText txt
  Image {source = src} ->
    Just $ WCBImage src
  ToolUse {id = ToolUseId tid, name = toolName, input = toolInput} ->
    Just $
      WCBToolUse
        { wcbToolId = tid,
          wcbToolName = toolName,
          wcbToolInput = toolInput
        }
  ToolResult {toolUseId = ToolResultId tid, content = resultContent, isError = isErr} ->
    Just $
      WCBToolResult
        { wcbToolUseId = tid,
          wcbResultContent = resultContent,
          wcbIsError = isErr
        }
  -- Thinking blocks are internal to the model and not sent to TypeScript
  Thinking {} ->
    Nothing
  RedactedThinking {} ->
    Nothing
  -- JSON blocks get stringified as text
  Json {json = val} ->
    Just $ WCBText $ TL.toStrict $ TLE.decodeUtf8 $ Aeson.encode val

-- | Convert a native Message to wire format.
messageToWire :: Message -> WireMessage
messageToWire msg =
  WireMessage
    { wmRole = roleToText msg.role,
      wmContent = mapMaybe contentBlockToWire (NE.toList msg.content)
    }
  where
    roleToText User = "user"
    roleToText Assistant = "assistant"

-- | Convert system prompt + user content blocks to wire messages.
--
-- Creates:
-- 1. A "system" message if system prompt is non-empty
-- 2. A "user" message with converted content blocks
--
-- This is the primary entry point for converting LLM effect inputs to wire format.
contentBlocksToWireMessages :: Text -> [ContentBlock] -> [WireMessage]
contentBlocksToWireMessages systemPrompt contentBlocks =
  systemMsg ++ userMsgs
  where
    -- System message (if non-empty)
    systemMsg
      | T.null systemPrompt = []
      | otherwise = [WireMessage "system" [WCBText systemPrompt]]

    -- User message with converted content
    userMsgs =
      let wireBlocks = mapMaybe contentBlockToWire contentBlocks
       in if null wireBlocks
            then []
            else [WireMessage "user" wireBlocks]

-- ════════════════════════════════════════════════════════════════════════════
-- WIRE → NATIVE (for receiving from TypeScript)
-- ════════════════════════════════════════════════════════════════════════════

-- | Convert a wire ContentBlock to native format.
wireContentBlockToNative :: WireContentBlock -> ContentBlock
wireContentBlockToNative = \case
  WCBText txt ->
    Text {text = txt}
  WCBImage src ->
    Image {source = src}
  WCBToolUse tid toolName toolInput ->
    ToolUse
      { id = ToolUseId tid,
        name = toolName,
        input = toolInput
      }
  WCBToolResult tid resultContent isErr ->
    ToolResult
      { toolUseId = ToolResultId tid,
        content = resultContent,
        isError = isErr
      }

-- | Convert a wire Message to native format.
--
-- Returns 'Nothing' if the message has empty content (wire messages should have content).
wireMessageToNative :: WireMessage -> Maybe Message
wireMessageToNative msg =
  case NE.nonEmpty (map wireContentBlockToNative msg.wmContent) of
    Nothing -> Nothing -- Empty content, skip this message
    Just blocks ->
      Just
        Message
          { role = textToRole msg.wmRole,
            content = blocks
          }
  where
    textToRole "user" = User
    textToRole "assistant" = Assistant
    textToRole _ = User -- Default to user for unknown roles

-- | Convert a list of wire Messages to native format.
--
-- Skips messages with empty content.
wireMessagesToNative :: [WireMessage] -> [Message]
wireMessagesToNative = mapMaybe wireMessageToNative

-- ════════════════════════════════════════════════════════════════════════════
-- TURN RESULT PARSING (from TypeScript response)
-- ════════════════════════════════════════════════════════════════════════════

-- | Wire format for TurnResult, matching TypeScript's response structure.
--
-- TypeScript returns:
-- @
-- {
--   "content": [...],        // Parsed output (JSON)
--   "toolsInvoked": [...],   // Tool invocations during turn
--   "narrative": "...",      // Text narrative from response
--   "thinking": "..."        // Extended thinking content
-- }
-- @
data WireTurnResult = WireTurnResult
  { -- | The parsed output (JSON value)
    wtrContent :: Value,
    -- | Tools that were invoked during the turn
    wtrToolsInvoked :: [ToolInvocation],
    -- | Text narrative extracted from response
    wtrNarrative :: Text,
    -- | Extended thinking content (if any)
    wtrThinking :: Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON WireTurnResult where
  toJSON wtr =
    object
      [ "content" .= wtr.wtrContent,
        "toolsInvoked" .= wtr.wtrToolsInvoked,
        "narrative" .= wtr.wtrNarrative,
        "thinking" .= wtr.wtrThinking
      ]

instance FromJSON WireTurnResult where
  parseJSON = withObject "WireTurnResult" $ \o ->
    WireTurnResult
      <$> o .: "content"
      <*> (o .:? "toolsInvoked" >>= maybe (pure []) parseJSON)
      <*> (o .:? "narrative" >>= maybe (pure "") pure)
      <*> (o .:? "thinking" >>= maybe (pure "") pure)

-- | Parse a wire TurnResult from TypeScript's JSON response.
--
-- TypeScript handles the tool loop and returns the final TurnResult.
-- This function parses that response back to Haskell's 'TurnResult' type.
--
-- Returns 'Left' with error message if parsing fails.
parseWireTurnResult :: Value -> Either Text (TurnResult Value)
parseWireTurnResult val =
  case Aeson.parseEither parseJSON val of
    Left err -> Left $ "Failed to parse TurnResult: " <> T.pack err
    Right (WireTurnResult content tools narrative thinking) ->
      Right
        TurnResult
          { trOutput = content,
            trToolsInvoked = tools,
            trNarrative = narrative,
            trThinking = thinking
          }

-- ════════════════════════════════════════════════════════════════════════════
-- TOOL RESULT OUTCOME CONVERSIONS (dispatcher results)
-- ════════════════════════════════════════════════════════════════════════════

-- | Convert a parameterized ToolResult to wire ToolResultOutcome.
--
-- Note: This loses type information at the boundary (parameterized → untyped).
-- Type safety is restored at the interpreter level via ConvertTransitionHint typeclass.
toToolResultOutcome :: Effect.ToolResult targets -> ToolResultOutcome
toToolResultOutcome = \case
  Effect.ToolSuccess val ->
    TROSuccess val
  Effect.ToolBreak reason ->
    TROBreak reason
  Effect.ToolTransition target payload ->
    TROTransition target payload

-- | Convert a wire ToolResultOutcome back to untyped ToolResult.
--
-- Returns ToolResult with empty target list ('[]') since type information is lost at FFI boundary.
-- The interpreter's ConvertTransitionHint typeclass will convert to properly-typed GotoChoice.
fromToolResultOutcome :: ToolResultOutcome -> Effect.ToolResult '[]
fromToolResultOutcome = \case
  TROSuccess val ->
    Effect.ToolSuccess val
  TROBreak reason ->
    Effect.ToolBreak reason
  TROTransition target payload ->
    Effect.ToolTransition target payload
