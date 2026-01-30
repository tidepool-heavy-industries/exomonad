{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Wire types for WASM ↔ TypeScript communication.
--
-- These types cross the WASM/JSON boundary and must match the
-- TypeScript definitions in @deploy/src/protocol.ts@.
--
-- Design: Domain-specific effects only (LLM, Habitica, Log).
-- No general-purpose primitives like HTTP fetch.
module ExoMonad.Wasm.WireTypes
  ( -- * Effects (WASM → TypeScript)
    SerializableEffect (..),

    -- * Effect Metadata (for routing) - re-exported from exomonad-core
    EffectCategory (..),
    EffectSemantics (..),
    effectMetadata,

    -- * LLM Call Types (for tool-aware LLM calls)
    WireMessage (..),
    WireContentBlock (..),
    WireToolCall (..),
    LlmCallResult (..),

    -- * Tool Result Outcomes (tool dispatcher results)
    ToolResultOutcome (..),

    -- * Results (TypeScript → WASM)
    EffectResult (..),
    TelegramAskResult (..),

    -- * Graph State (for observability)
    ExecutionPhase (..),
    GraphState (..),

    -- * Step Output
    StepOutput (..),

    -- * Graph Info (compile-time metadata)
    TypeInfoWire (..),
    GotoTargetWire (..),
    NodeInfoWire (..),
    EdgeInfoWire (..),
    GraphInfoWire (..),
  )
where

import Data.Aeson
  ( FromJSON (..),
    Object,
    ToJSON (..),
    Value (..),
    object,
    withObject,
    (.:),
    (.:?),
    (.=),
  )
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Text qualified as T
-- Re-export effect metadata from the single source of truth

-- Import ImageSource for image content blocks
import ExoMonad.Anthropic.Types (ImageSource (..))
import ExoMonad.Effect.Metadata (EffectCategory (..), EffectSemantics (..))
import GHC.Generics (Generic)

-- ════════════════════════════════════════════════════════════════════════════
-- LLM CALL TYPES (for tool-aware LLM calls)
-- ════════════════════════════════════════════════════════════════════════════

-- | Wire-format message for LLM conversation history.
--
-- JSON encoding: @{role: "user", content: [...]}@
data WireMessage = WireMessage
  { -- | Role: "user" | "assistant" | "system"
    wmRole :: Text,
    -- | Content blocks
    wmContent :: [WireContentBlock]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON WireMessage where
  toJSON msg =
    object
      [ "role" .= msg.wmRole,
        "content" .= msg.wmContent
      ]

instance FromJSON WireMessage where
  parseJSON = withObject "WireMessage" $ \o ->
    WireMessage
      <$> o .: "role"
      <*> o .: "content"

-- | Wire-format content block for LLM messages.
--
-- JSON encoding uses "type" discriminator:
-- - @{type: "text", text: "hello"}@
-- - @{type: "image", source: {type: "base64", media_type: "...", data: "..."}}@
-- - @{type: "tool_use", id: "...", name: "ask_user", input: {...}}@
-- - @{type: "tool_result", tool_use_id: "...", content: "...", is_error: false}@
data WireContentBlock
  = -- | Plain text content
    WCBText {wcbText :: Text}
  | -- | Image content (base64 or URL)
    WCBImage {wcbImageSource :: ImageSource}
  | WCBToolUse
      { -- | Unique tool use ID
        wcbToolId :: Text,
        -- | Tool name
        wcbToolName :: Text,
        -- | Tool input (JSON)
        wcbToolInput :: Value
      }
  | -- \^ LLM is calling a tool
    WCBToolResult
      { -- | ID of the tool_use this is responding to
        wcbToolUseId :: Text,
        -- | Result content (stringified)
        wcbResultContent :: Text,
        -- | Whether this is an error result
        wcbIsError :: Bool
      }
  -- \^ Result of a tool execution
  deriving stock (Show, Eq, Generic)

instance ToJSON WireContentBlock where
  toJSON (WCBText txt) =
    object
      [ "type" .= ("text" :: Text),
        "text" .= txt
      ]
  toJSON (WCBImage source) =
    object
      [ "type" .= ("image" :: Text),
        "source" .= source
      ]
  toJSON (WCBToolUse tid name input) =
    object
      [ "type" .= ("tool_use" :: Text),
        "id" .= tid,
        "name" .= name,
        "input" .= input
      ]
  toJSON (WCBToolResult tid content isErr) =
    object
      [ "type" .= ("tool_result" :: Text),
        "tool_use_id" .= tid,
        "content" .= content,
        "is_error" .= isErr
      ]

instance FromJSON WireContentBlock where
  parseJSON = withObject "WireContentBlock" $ \o -> do
    (typ :: Text) <- o .: "type"
    case typ of
      "text" -> WCBText <$> o .: "text"
      "image" -> WCBImage <$> o .: "source"
      "tool_use" ->
        WCBToolUse
          <$> o .: "id"
          <*> o .: "name"
          <*> o .: "input"
      "tool_result" ->
        WCBToolResult
          <$> o .: "tool_use_id"
          <*> o .: "content"
          <*> o .: "is_error"
      _ -> fail $ "Unknown WireContentBlock type: " ++ T.unpack typ

-- | Tool call request from LLM.
--
-- JSON encoding: @{id: "...", name: "ask_user", input: {...}}@
data WireToolCall = WireToolCall
  { -- | Unique ID for this tool call (used in tool_result)
    wtcId :: Text,
    -- | Tool name (e.g., "ask_user")
    wtcName :: Text,
    -- | Tool arguments (JSON)
    wtcInput :: Value
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON WireToolCall where
  toJSON tc =
    object
      [ "id" .= tc.wtcId,
        "name" .= tc.wtcName,
        "input" .= tc.wtcInput
      ]

instance FromJSON WireToolCall where
  parseJSON = withObject "WireToolCall" $ \o ->
    WireToolCall
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "input"

-- | Result from LLM API call - either done or needs tools.
--
-- JSON encoding uses "type" discriminator:
-- - @{type: "done", content: [...]}@
-- - @{type: "needs_tools", tool_calls: [...], content: [...]}@
data LlmCallResult
  = LlmDone
      { -- | Final response content
        lcrContent :: [WireContentBlock]
      }
  | LlmNeedsTools
      { -- | Tools the LLM wants to call
        lcrToolCalls :: [WireToolCall],
        -- | Any text content before tools (for logging)
        lcrTextContent :: [WireContentBlock]
      }
  deriving stock (Show, Eq, Generic)

instance ToJSON LlmCallResult where
  toJSON (LlmDone content) =
    object
      [ "type" .= ("done" :: Text),
        "content" .= content
      ]
  toJSON (LlmNeedsTools calls content) =
    object
      [ "type" .= ("needs_tools" :: Text),
        "tool_calls" .= calls,
        "content" .= content
      ]

instance FromJSON LlmCallResult where
  parseJSON = withObject "LlmCallResult" $ \o -> do
    (typ :: Text) <- o .: "type"
    case typ of
      "done" -> LlmDone <$> o .: "content"
      "needs_tools" ->
        LlmNeedsTools
          <$> o .: "tool_calls"
          <*> o .: "content"
      _ -> fail $ "Unknown LlmCallResult type: " ++ T.unpack typ

-- | Tool dispatcher result outcome - what a tool execution produces.
--
-- This is separate from WCBToolResult (which is for Anthropic message blocks).
-- ToolResultOutcome represents the result of executing a tool via the dispatcher.
--
-- JSON encoding uses "tag" discriminator:
-- - @{tag: "success", value: {...}}@
-- - @{tag: "break", reason: "..."}@
-- - @{tag: "transition", target: "nodeA", payload: {...}}@  (NEW: tool-initiated transitions)
data ToolResultOutcome
  = -- | Tool succeeded with output value
    TROSuccess Value
  | -- | Tool requested turn break
    TROBreak Text
  | -- | Tool-initiated graph transition (target node name + payload)
    TROTransition Text Value
  deriving stock (Show, Eq, Generic)

instance ToJSON ToolResultOutcome where
  toJSON (TROSuccess val) =
    object
      [ "tag" .= ("success" :: Text),
        "value" .= val
      ]
  toJSON (TROBreak reason) =
    object
      [ "tag" .= ("break" :: Text),
        "reason" .= reason
      ]
  toJSON (TROTransition target payload) =
    object
      [ "tag" .= ("transition" :: Text),
        "target" .= target,
        "payload" .= payload
      ]

instance FromJSON ToolResultOutcome where
  parseJSON = withObject "ToolResultOutcome" $ \o -> do
    (tag :: Text) <- o .: "tag"
    case tag of
      "success" -> TROSuccess <$> o .: "value"
      "break" -> TROBreak <$> o .: "reason"
      "transition" -> TROTransition <$> o .: "target" <*> o .: "payload"
      _ -> fail $ "Unknown ToolResultOutcome tag: " ++ T.unpack tag

-- ════════════════════════════════════════════════════════════════════════════
-- EFFECTS (WASM → TypeScript)
-- ════════════════════════════════════════════════════════════════════════════

-- | Effects that Haskell yields for TypeScript to execute.
--
-- JSON encoding matches protocol.ts: @{type: "LogInfo", eff_message: "...", eff_fields: {...}}@
data SerializableEffect
  = EffLlmComplete
      { -- | Which node is making this call
        effNode :: Text,
        -- | System prompt
        effSystemPrompt :: Text,
        -- | User content
        effUserContent :: Text,
        -- | JSON schema for structured output
        effSchema :: Maybe Value,
        -- | Model to use (e.g., "@cf/meta/llama-3.3-70b-instruct-fp8-fast")
        -- If Nothing, TypeScript uses its default model
        effModel :: Maybe Text
      }
  | EffLogInfo
      { -- | Log message
        effMessage :: Text,
        -- | Optional structured fields for queryable log data
        effFields :: Maybe (Map Text Value)
      }
  | EffLogError
      { -- | Log message
        effMessage :: Text,
        -- | Optional structured fields for queryable log data
        effFields :: Maybe (Map Text Value)
      }
  | EffHabitica
      { -- | Operation name: "GetUser", "ScoreTask", "GetTasks", etc.
        effHabOp :: Text,
        -- | Operation-specific payload (taskId, direction, etc.)
        effHabPayload :: Value
      }
  | EffTelegramSend
      { -- | Message text to send
        effTgText :: Text,
        -- | Parse mode: "PlainText" | "Markdown" | "HTML"
        effTgParseMode :: Text,
        -- | Optional thread/topic ID for group forums or private chat topics
        effTgThreadId :: Maybe Int
      }
  | EffTelegramAsk
      { -- | Message text to display
        effTgAskText :: Text,
        -- | Parse mode: "PlainText" | "Markdown" | "HTML"
        effTgAskParseMode :: Text,
        -- | Button options: [(label, callback)]
        effTgButtons :: [(Text, Text)],
        -- | Optional thread/topic ID for group forums or private chat topics
        effTgAskThreadId :: Maybe Int
      }
  | EffLlmCall
      { -- | Which node is making this call
        effLlmNode :: Text,
        -- | Full conversation history
        effLlmMessages :: [WireMessage],
        -- | JSON schema for structured output
        effLlmSchema :: Maybe Value,
        -- | Tool definitions (Anthropic format)
        effLlmTools :: [Value],
        -- | Model to use (e.g., "@cf/meta/llama-3.3-70b-instruct-fp8-fast")
        -- If Nothing, TypeScript uses its default model
        effLlmModel :: Maybe Text
      }
  | -- ══════════════════════════════════════════════════════════════════════════
    -- State Effects (for DM and other stateful graphs)
    -- ══════════════════════════════════════════════════════════════════════════
    EffGetState
      { -- | State key (e.g., "worldState", "sessionState")
        effStateKey :: Text
      }
  | EffSetState
      { -- | State key
        effStateKey :: Text,
        -- | New state value (full replacement)
        effStateValue :: Value
      }
  | -- ══════════════════════════════════════════════════════════════════════════
    -- Event Emission (for observability/GUI)
    -- ══════════════════════════════════════════════════════════════════════════
    EffEmitEvent
      { -- | Event name (e.g., "StressChanged", "ClockAdvanced")
        effEventName :: Text,
        -- | Event-specific payload
        effEventPayload :: Value
      }
  | -- ══════════════════════════════════════════════════════════════════════════
    -- Random Number Generation
    -- ══════════════════════════════════════════════════════════════════════════
    EffRandomInt
      { -- | Minimum value (inclusive)
        effRandomMin :: Int,
        -- | Maximum value (inclusive)
        effRandomMax :: Int
      }
  | -- ══════════════════════════════════════════════════════════════════════════
    -- Time
    -- ══════════════════════════════════════════════════════════════════════════

    -- | Get current UTC time (returns ISO8601 string)
    EffGetTime
  deriving stock (Show, Eq, Generic)

instance ToJSON SerializableEffect where
  toJSON (EffLlmComplete node sys user schema model) =
    object $
      [ "type" .= ("LlmComplete" :: Text),
        "eff_node" .= node,
        "eff_system_prompt" .= sys,
        "eff_user_content" .= user
      ]
        ++ maybe [] (\s -> ["eff_schema" .= s]) schema
        ++ maybe [] (\m -> ["eff_model" .= m]) model
  toJSON (EffLogInfo msg fields) =
    object $
      [ "type" .= ("LogInfo" :: Text),
        "eff_message" .= msg
      ]
        ++ maybe [] (\f -> ["eff_fields" .= f]) fields
  toJSON (EffLogError msg fields) =
    object $
      [ "type" .= ("LogError" :: Text),
        "eff_message" .= msg
      ]
        ++ maybe [] (\f -> ["eff_fields" .= f]) fields
  toJSON (EffHabitica op payload) =
    object
      [ "type" .= ("Habitica" :: Text),
        "eff_hab_op" .= op,
        "eff_hab_payload" .= payload
      ]
  toJSON (EffTelegramSend txt parseMode threadId) =
    object $
      [ "type" .= ("TelegramSend" :: Text),
        "eff_tg_text" .= txt,
        "eff_tg_parse_mode" .= parseMode
      ]
        ++ maybe [] (\tid -> ["eff_tg_thread_id" .= tid]) threadId
  toJSON (EffTelegramAsk txt parseMode buttons threadId) =
    object $
      [ "type" .= ("TelegramAsk" :: Text),
        "eff_tg_text" .= txt,
        "eff_tg_parse_mode" .= parseMode,
        "eff_buttons" .= [[label, val] | (label, val) <- buttons]
      ]
        ++ maybe [] (\tid -> ["eff_tg_thread_id" .= tid]) threadId
  toJSON (EffLlmCall node msgs schema tools model) =
    object $
      [ "type" .= ("LlmCall" :: Text),
        "eff_node" .= node,
        "eff_messages" .= msgs,
        "eff_tools" .= tools
      ]
        ++ maybe [] (\s -> ["eff_schema" .= s]) schema
        ++ maybe [] (\m -> ["eff_model" .= m]) model
  toJSON (EffGetState key) =
    object
      [ "type" .= ("GetState" :: Text),
        "eff_state_key" .= key
      ]
  toJSON (EffSetState key value) =
    object
      [ "type" .= ("SetState" :: Text),
        "eff_state_key" .= key,
        "eff_state_value" .= value
      ]
  toJSON (EffEmitEvent name payload) =
    object
      [ "type" .= ("EmitEvent" :: Text),
        "eff_event_name" .= name,
        "eff_event_payload" .= payload
      ]
  toJSON (EffRandomInt minVal maxVal) =
    object
      [ "type" .= ("RandomInt" :: Text),
        "eff_min" .= minVal,
        "eff_max" .= maxVal
      ]
  toJSON EffGetTime =
    object
      [ "type" .= ("GetTime" :: Text)
      ]

instance FromJSON SerializableEffect where
  parseJSON = withObject "SerializableEffect" $ \o -> do
    (typ :: Text) <- o .: "type"
    case typ of
      "LlmComplete" ->
        EffLlmComplete
          <$> o .: "eff_node"
          <*> o .: "eff_system_prompt"
          <*> o .: "eff_user_content"
          <*> o .:? "eff_schema"
          <*> o .:? "eff_model"
      "LogInfo" ->
        EffLogInfo
          <$> o .: "eff_message"
          <*> o .:? "eff_fields"
      "LogError" ->
        EffLogError
          <$> o .: "eff_message"
          <*> o .:? "eff_fields"
      "Habitica" ->
        EffHabitica
          <$> o .: "eff_hab_op"
          <*> o .: "eff_hab_payload"
      "TelegramSend" ->
        EffTelegramSend
          <$> o .: "eff_tg_text"
          <*> o .: "eff_tg_parse_mode"
          <*> o .:? "eff_tg_thread_id"
      "TelegramAsk" -> do
        txt <- o .: "eff_tg_text"
        parseMode <- o .: "eff_tg_parse_mode"
        buttonArrays <- o .: "eff_buttons" :: Parser [[Text]]
        threadId <- o .:? "eff_tg_thread_id"
        let buttons = [(l, v) | [l, v] <- buttonArrays]
        pure $ EffTelegramAsk txt parseMode buttons threadId
      "LlmCall" ->
        EffLlmCall
          <$> o .: "eff_node"
          <*> o .: "eff_messages"
          <*> o .:? "eff_schema"
          <*> o .: "eff_tools"
          <*> o .:? "eff_model"
      "GetState" -> EffGetState <$> o .: "eff_state_key"
      "SetState" ->
        EffSetState
          <$> o .: "eff_state_key"
          <*> o .: "eff_state_value"
      "EmitEvent" ->
        EffEmitEvent
          <$> o .: "eff_event_name"
          <*> o .: "eff_event_payload"
      "RandomInt" ->
        EffRandomInt
          <$> o .: "eff_min"
          <*> o .: "eff_max"
      "GetTime" -> pure EffGetTime
      _ -> fail $ "Unknown effect type: " ++ show typ

-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT METADATA (for routing decisions)
-- ════════════════════════════════════════════════════════════════════════════

-- EffectCategory and EffectSemantics are imported from ExoMonad.Effect.Metadata
-- (the single source of truth for effect routing)

-- | Get routing metadata for an effect.
--
-- This determines:
-- 1. Whether StateMachineDO handles it internally or yields to caller
-- 2. Whether the graph waits for a response or continues immediately
--
-- Implementation note: Uses pattern matching on effect constructors and
-- looks up metadata from allEffectMeta. The type name must match the
-- emTypeName in the metadata.
effectMetadata :: SerializableEffect -> (EffectCategory, EffectSemantics)
effectMetadata = \case
  EffLogInfo {} -> (Internal, FireAndForget)
  EffLogError {} -> (Internal, FireAndForget)
  EffLlmComplete {} -> (Internal, Blocking)
  EffLlmCall {} -> (Internal, Blocking) -- Raw LLM API call
  EffHabitica {} -> (Internal, Blocking)
  EffTelegramSend {} -> (Yielded, FireAndForget) -- Send and continue
  EffTelegramAsk {} -> (Yielded, Blocking) -- Wait for button click
  -- State effects (for DM and other stateful graphs)
  EffGetState {} -> (Internal, Blocking) -- Read state from storage
  EffSetState {} -> (Internal, FireAndForget) -- Write state to storage
  -- Event emission (for observability/GUI updates)
  EffEmitEvent {} -> (Yielded, FireAndForget) -- Notify listeners
  -- Random number generation
  EffRandomInt {} -> (Internal, Blocking) -- Get random int
  -- Time
  EffGetTime {} -> (Internal, Blocking) -- Get current time

-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT RESULTS (TypeScript → WASM)
-- ════════════════════════════════════════════════════════════════════════════

-- | Result of effect execution from TypeScript.
--
-- For Log effects, TypeScript just acknowledges (success with null value).
-- For LLM effects (future), success contains parsed output.
--
-- JSON encoding: @{type: "success", value: ...}@ or @{type: "error", message: "..."}@
data EffectResult
  = ResSuccess {resValue :: Maybe Value}
  | ResError {resMessage :: Text}
  deriving stock (Show, Eq, Generic)

instance ToJSON EffectResult where
  toJSON (ResSuccess val) =
    object $
      ("type" .= ("success" :: Text))
        : maybe [] (\v -> ["value" .= v]) val
  toJSON (ResError msg) =
    object
      [ "type" .= ("error" :: Text),
        "message" .= msg
      ]

instance FromJSON EffectResult where
  parseJSON = withObject "EffectResult" $ \o -> do
    (typ :: Text) <- o .: "type"
    case typ of
      "success" -> ResSuccess <$> lookupValue o
      "error" -> ResError <$> o .: "message"
      _ -> fail $ "Unknown result type: " ++ show typ

-- | Look up "value" field, distinguishing missing from null.
--
-- - Missing field → Nothing
-- - Field present (even if null) → Just value
--
-- This is needed because .:? treats null as missing, but for Maybe Value
-- we need to preserve Just Null when the field is explicitly null.
lookupValue :: Object -> Parser (Maybe Value)
lookupValue o = pure $ KM.lookup (fromText "value") o

-- ════════════════════════════════════════════════════════════════════════════
-- TELEGRAM ASK RESULT (TypeScript → WASM)
-- ════════════════════════════════════════════════════════════════════════════

-- | Result from TelegramAsk effect.
--
-- The user can respond to buttons in three ways:
-- 1. Click a valid button → 'TelegramButton' with the action
-- 2. Send text instead → 'TelegramText' with the message
-- 3. Click a stale button → 'TelegramStaleButton' (nonce mismatch)
--
-- JSON encoding matches TypeScript TelegramAskResult:
-- - @{type: "button", response: "approved"}@
-- - @{type: "text", text: "some message"}@
-- - @{type: "stale_button"}@
data TelegramAskResult
  = -- | User clicked a valid button
    TelegramButton {tarResponse :: Text}
  | -- | User sent text instead of clicking
    TelegramText {tarText :: Text}
  | -- | User clicked a button with expired/invalid nonce
    TelegramStaleButton
  deriving stock (Show, Eq, Generic)

instance ToJSON TelegramAskResult where
  toJSON (TelegramButton response) =
    object
      [ "type" .= ("button" :: Text),
        "response" .= response
      ]
  toJSON (TelegramText txt) =
    object
      [ "type" .= ("text" :: Text),
        "text" .= txt
      ]
  toJSON TelegramStaleButton =
    object
      [ "type" .= ("stale_button" :: Text)
      ]

instance FromJSON TelegramAskResult where
  parseJSON = withObject "TelegramAskResult" $ \o -> do
    (typ :: Text) <- o .: "type"
    case typ of
      "button" -> TelegramButton <$> o .: "response"
      "text" -> TelegramText <$> o .: "text"
      "stale_button" -> pure TelegramStaleButton
      _ -> fail $ "Unknown TelegramAskResult type: " ++ T.unpack typ

-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH STATE (for observability)
-- ════════════════════════════════════════════════════════════════════════════

-- | Execution phase - matches protocol.ts ExecutionPhase exactly.
--
-- JSON encoding uses flat object with "type" discriminator:
-- - @{type: "idle"}@
-- - @{type: "in_node", nodeName: "classify"}@
-- - @{type: "transitioning", fromNode: "a", toNode: "b"}@
-- - @{type: "completed", result: ...}@
-- - @{type: "failed", error: "..."}@
data ExecutionPhase
  = PhaseIdle
  | PhaseInNode {phaseName :: Text}
  | PhaseTransitioning {phaseFrom :: Text, phaseTo :: Text}
  | PhaseCompleted {phaseResult :: Value}
  | PhaseFailed {phaseError :: Text}
  deriving stock (Show, Eq, Generic)

instance ToJSON ExecutionPhase where
  toJSON PhaseIdle =
    object
      [ "type" .= ("idle" :: Text)
      ]
  toJSON (PhaseInNode name) =
    object
      [ "type" .= ("in_node" :: Text),
        "nodeName" .= name
      ]
  toJSON (PhaseTransitioning from to) =
    object
      [ "type" .= ("transitioning" :: Text),
        "fromNode" .= from,
        "toNode" .= to
      ]
  toJSON (PhaseCompleted result) =
    object
      [ "type" .= ("completed" :: Text),
        "result" .= result
      ]
  toJSON (PhaseFailed err) =
    object
      [ "type" .= ("failed" :: Text),
        "error" .= err
      ]

instance FromJSON ExecutionPhase where
  parseJSON = withObject "ExecutionPhase" $ \o -> do
    (typ :: Text) <- o .: "type"
    case typ of
      "idle" -> pure PhaseIdle
      "in_node" -> PhaseInNode <$> o .: "nodeName"
      "transitioning" -> PhaseTransitioning <$> o .: "fromNode" <*> o .: "toNode"
      "completed" -> PhaseCompleted <$> o .: "result"
      "failed" -> PhaseFailed <$> o .: "error"
      _ -> fail $ "Unknown execution phase type: " ++ show typ

-- | Runtime graph state - matches protocol.ts GraphState.
--
-- JSON encoding: @{phase: {...}, completedNodes: [...]}@
data GraphState = GraphState
  { -- | Current execution phase
    gsPhase :: ExecutionPhase,
    -- | Nodes that have completed
    gsCompletedNodes :: [Text]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON GraphState where
  toJSON gs =
    object
      [ "phase" .= gs.gsPhase,
        "completedNodes" .= gs.gsCompletedNodes
      ]

instance FromJSON GraphState where
  parseJSON = withObject "GraphState" $ \o ->
    GraphState
      <$> o .: "phase"
      <*> o .: "completedNodes"

-- ════════════════════════════════════════════════════════════════════════════
-- STEP OUTPUT (WASM → TypeScript per step)
-- ════════════════════════════════════════════════════════════════════════════

-- | Output from each step of graph execution.
--
-- This is a sum type that makes invalid states unrepresentable:
-- - 'StepYield': Not done, has an effect to execute
-- - 'StepDone': Completed successfully with a result
-- - 'StepFailed': Completed with an error
--
-- The execution loop:
-- 1. TypeScript calls @initialize(input)@ → gets StepOutput
-- 2. If StepYield: execute the effect, call @step(result)@ → gets StepOutput
-- 3. Repeat until StepDone or StepFailed
data StepOutput
  = StepYield
      { -- | Effect to execute
        soEffect :: SerializableEffect,
        -- | Current graph execution state (for observability)
        soGraphState :: GraphState
      }
  | StepDone
      { -- | Final result (matches Exit type)
        soResult :: Value,
        -- | Final graph execution state
        soGraphState :: GraphState
      }
  | StepFailed
      { -- | Error message
        soError :: Text,
        -- | Graph state at failure
        soGraphState :: GraphState
      }
  deriving stock (Show, Eq, Generic)

instance ToJSON StepOutput where
  toJSON (StepYield eff gs) =
    object
      [ "effect" .= Just eff,
        "done" .= False,
        "stepResult" .= Null,
        "graphState" .= gs
      ]
  toJSON (StepDone result gs) =
    object
      [ "effect" .= Null,
        "done" .= True,
        "stepResult" .= result,
        "graphState" .= gs
      ]
  toJSON (StepFailed err gs) =
    object
      [ "effect" .= Null,
        "done" .= True,
        "stepResult" .= Null,
        "error" .= err,
        "graphState" .= gs
      ]

instance FromJSON StepOutput where
  parseJSON = withObject "StepOutput" $ \o -> do
    done <- o .: "done"
    mEffect <- o .:? "effect"
    mError <- o .:? "error"
    gs <- o .: "graphState"
    case (done, mEffect, mError) of
      -- Valid: not done, has effect to execute
      (False, Just eff, _) -> pure $ StepYield eff gs
      -- Valid: done with error
      (True, Nothing, Just err) -> pure $ StepFailed err gs
      -- Valid: done with result
      (True, Nothing, Nothing) -> do
        result <- o .: "stepResult"
        pure $ StepDone result gs
      -- Invalid: done should not have an effect
      (True, Just _, _) ->
        fail "StepOutput: done=true should not have an effect"
      -- Invalid: not done requires an effect
      (False, Nothing, _) ->
        fail "StepOutput: done=false requires an effect"

-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH INFO (compile-time metadata - matches protocol.ts)
-- ════════════════════════════════════════════════════════════════════════════

-- | Type information for a Haskell type.
--
-- JSON encoding: @{typeName: "Intent", typeModule: "Echo"}@
data TypeInfoWire = TypeInfoWire
  { -- | Simple type name, e.g., "Intent"
    tiwTypeName :: Text,
    -- | Module path, e.g., "Echo"
    tiwTypeModule :: Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON TypeInfoWire where
  toJSON ti =
    object
      [ "typeName" .= ti.tiwTypeName,
        "typeModule" .= ti.tiwTypeModule
      ]

instance FromJSON TypeInfoWire where
  parseJSON = withObject "TypeInfoWire" $ \o ->
    TypeInfoWire
      <$> o .: "typeName"
      <*> o .: "typeModule"

-- | Goto target - control flow destination.
--
-- JSON encoding: @{gtTarget: "exit", gtPayloadType: {...}}@
data GotoTargetWire = GotoTargetWire
  { -- | Target node name, or "Exit"
    gtwTarget :: Text,
    -- | Type passed (for Exit transitions)
    gtwPayloadType :: TypeInfoWire
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON GotoTargetWire where
  toJSON gt =
    object
      [ "gtTarget" .= gt.gtwTarget,
        "gtPayloadType" .= gt.gtwPayloadType
      ]

instance FromJSON GotoTargetWire where
  parseJSON = withObject "GotoTargetWire" $ \o ->
    GotoTargetWire
      <$> o .: "gtTarget"
      <*> o .: "gtPayloadType"

-- | Node metadata - matches protocol.ts NodeInfo.
--
-- JSON encoding:
-- @{niName: "classify", niKind: "LLM", niInput: {...}, niSchema: null, niGotoTargets: [...]}@
data NodeInfoWire = NodeInfoWire
  { -- | Node name (record field name in Servant-style)
    niwName :: Text,
    -- | "LLM" or "Logic"
    niwKind :: Text,
    -- | Input type this node needs (single type)
    niwInput :: Maybe TypeInfoWire,
    -- | Output type (LLM nodes only)
    niwSchema :: Maybe TypeInfoWire,
    -- | Possible Goto targets (Logic nodes only)
    niwGotoTargets :: [GotoTargetWire]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON NodeInfoWire where
  toJSON ni =
    object
      [ "niName" .= ni.niwName,
        "niKind" .= ni.niwKind,
        "niInput" .= ni.niwInput,
        "niSchema" .= ni.niwSchema,
        "niGotoTargets" .= ni.niwGotoTargets
      ]

instance FromJSON NodeInfoWire where
  parseJSON = withObject "NodeInfoWire" $ \o ->
    NodeInfoWire
      <$> o .: "niName"
      <*> o .: "niKind"
      <*> o .: "niInput"
      <*> o .: "niSchema"
      <*> o .: "niGotoTargets"

-- | Edge for graph visualization.
--
-- JSON encoding: @{eiFrom: "entry", eiTo: "classify", eiPayloadType: {...}}@
data EdgeInfoWire = EdgeInfoWire
  { -- | Source: "Entry" or node name
    eiwFrom :: Text,
    -- | Target: "Exit" or node name
    eiwTo :: Text,
    -- | Type carried on this edge
    eiwPayloadType :: TypeInfoWire
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON EdgeInfoWire where
  toJSON ei =
    object
      [ "eiFrom" .= ei.eiwFrom,
        "eiTo" .= ei.eiwTo,
        "eiPayloadType" .= ei.eiwPayloadType
      ]

instance FromJSON EdgeInfoWire where
  parseJSON = withObject "EdgeInfoWire" $ \o ->
    EdgeInfoWire
      <$> o .: "eiFrom"
      <*> o .: "eiTo"
      <*> o .: "eiPayloadType"

-- | Static graph metadata - matches protocol.ts GraphInfo.
--
-- JSON encoding:
-- @{name: "ExampleGraph", entryType: {...}, exitType: {...}, nodes: [...], edges: [...]}@
data GraphInfoWire = GraphInfoWire
  { -- | Graph name
    giwName :: Text,
    -- | Type accepted at Entry
    giwEntryType :: TypeInfoWire,
    -- | Type produced at Exit
    giwExitType :: TypeInfoWire,
    -- | All nodes in the graph (excludes Entry/Exit)
    giwNodes :: [NodeInfoWire],
    -- | Edges for visualization (from Needs/Schema and Goto)
    giwEdges :: [EdgeInfoWire]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON GraphInfoWire where
  toJSON gi =
    object
      [ "name" .= gi.giwName,
        "entryType" .= gi.giwEntryType,
        "exitType" .= gi.giwExitType,
        "nodes" .= gi.giwNodes,
        "edges" .= gi.giwEdges
      ]

instance FromJSON GraphInfoWire where
  parseJSON = withObject "GraphInfoWire" $ \o ->
    GraphInfoWire
      <$> o .: "name"
      <*> o .: "entryType"
      <*> o .: "exitType"
      <*> o .: "nodes"
      <*> o .: "edges"
