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
module Tidepool.Wasm.Conversion
  ( -- * Native → Wire (for sending to TypeScript)
    contentBlockToWire
  , contentBlocksToWireMessages
  , messageToWire

    -- * Wire → Native (for receiving from TypeScript)
  , wireContentBlockToNative
  , wireMessageToNative
  , wireMessagesToNative

    -- * TurnResult parsing (from TypeScript response)
  , parseWireTurnResult
  , WireTurnResult(..)
  ) where

import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , Value(..)
  , (.:)
  , (.:?)
  , (.=)
  , object
  , withObject
  )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import GHC.Generics (Generic)

-- Native types from tidepool-core
import Tidepool.Anthropic.Types
  ( ContentBlock(..)
  , Message(..)
  , Role(..)
  , ToolUse(..)
  , ToolResult(..)
  )
import Tidepool.Effect.Types (TurnResult(..), ToolInvocation(..))

-- Wire types
import Tidepool.Wasm.WireTypes
  ( WireMessage(..)
  , WireContentBlock(..)
  )


-- ════════════════════════════════════════════════════════════════════════════
-- NATIVE → WIRE (for sending to TypeScript)
-- ════════════════════════════════════════════════════════════════════════════

-- | Convert a native ContentBlock to wire format.
--
-- Returns 'Nothing' for blocks that shouldn't be sent over the wire:
-- - ThinkingBlock (model-internal)
-- - RedactedThinkingBlock (model-internal)
-- - JsonBlock (converted to text representation)
contentBlockToWire :: ContentBlock -> Maybe WireContentBlock
contentBlockToWire = \case
  TextBlock txt ->
    Just $ WCBText txt

  ImageBlock source ->
    Just $ WCBImage source

  ToolUseBlock tu ->
    Just $ WCBToolUse
      { wcbToolId = tu.toolUseId
      , wcbToolName = tu.toolName
      , wcbToolInput = tu.toolInput
      }

  ToolResultBlock tr ->
    Just $ WCBToolResult
      { wcbToolUseId = tr.toolResultId
      , wcbResultContent = tr.toolResultContent
      , wcbIsError = tr.toolResultIsError
      }

  -- Thinking blocks are internal to the model and not sent to TypeScript
  ThinkingBlock _ ->
    Nothing

  RedactedThinkingBlock _ ->
    Nothing

  -- JSON blocks get stringified as text
  JsonBlock val ->
    Just $ WCBText $ TL.toStrict $ TLE.decodeUtf8 $ Aeson.encode val


-- | Convert a native Message to wire format.
messageToWire :: Message -> WireMessage
messageToWire msg = WireMessage
  { wmRole = roleToText msg.role
  , wmContent = mapMaybe contentBlockToWire msg.content
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
    TextBlock txt

  WCBImage source ->
    ImageBlock source

  WCBToolUse tid name input ->
    ToolUseBlock ToolUse
      { toolUseId = tid
      , toolName = name
      , toolInput = input
      }

  WCBToolResult tid content isErr ->
    ToolResultBlock ToolResult
      { toolResultId = tid
      , toolResultContent = content
      , toolResultIsError = isErr
      }


-- | Convert a wire Message to native format.
wireMessageToNative :: WireMessage -> Message
wireMessageToNative msg = Message
  { role = textToRole msg.wmRole
  , content = map wireContentBlockToNative msg.wmContent
  }
  where
    textToRole "user" = User
    textToRole "assistant" = Assistant
    textToRole _ = User  -- Default to user for unknown roles


-- | Convert a list of wire Messages to native format.
wireMessagesToNative :: [WireMessage] -> [Message]
wireMessagesToNative = map wireMessageToNative


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
  { wtrContent :: Value
    -- ^ The parsed output (JSON value)
  , wtrToolsInvoked :: [ToolInvocation]
    -- ^ Tools that were invoked during the turn
  , wtrNarrative :: Text
    -- ^ Text narrative extracted from response
  , wtrThinking :: Text
    -- ^ Extended thinking content (if any)
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON WireTurnResult where
  toJSON wtr = object
    [ "content" .= wtr.wtrContent
    , "toolsInvoked" .= wtr.wtrToolsInvoked
    , "narrative" .= wtr.wtrNarrative
    , "thinking" .= wtr.wtrThinking
    ]

instance FromJSON WireTurnResult where
  parseJSON = withObject "WireTurnResult" $ \o -> WireTurnResult
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
    Right (WireTurnResult content tools narrative thinking) -> Right TurnResult
      { trOutput = content
      , trToolsInvoked = tools
      , trNarrative = narrative
      , trThinking = thinking
      }

