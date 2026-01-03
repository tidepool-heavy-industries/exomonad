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
module Tidepool.Wasm.WireTypes
  ( -- * Effects (WASM → TypeScript)
    SerializableEffect(..)

    -- * Effect Metadata (for routing) - re-exported from tidepool-core
  , EffectCategory(..)
  , EffectSemantics(..)
  , effectMetadata

    -- * LLM Call Types (for tool-aware LLM calls)
  , WireMessage(..)
  , WireContentBlock(..)
  , WireToolCall(..)
  , LlmCallResult(..)

    -- * Results (TypeScript → WASM)
  , EffectResult(..)
  , TelegramAskResult(..)

    -- * Graph State (for observability)
  , ExecutionPhase(..)
  , GraphState(..)

    -- * Step Output
  , StepOutput(..)

    -- * Graph Info (compile-time metadata)
  , TypeInfoWire(..)
  , GotoTargetWire(..)
  , NodeInfoWire(..)
  , EdgeInfoWire(..)
  , GraphInfoWire(..)
  ) where

import Data.Aeson
  ( ToJSON(..)
  , FromJSON(..)
  , Value(..)
  , Object
  , object
  , (.=)
  , (.:)
  , (.:?)
  , withObject
  )
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Key (fromText)
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- Re-export effect metadata from the single source of truth
import Tidepool.Effect.Metadata (EffectCategory(..), EffectSemantics(..))


-- ════════════════════════════════════════════════════════════════════════════
-- LLM CALL TYPES (for tool-aware LLM calls)
-- ════════════════════════════════════════════════════════════════════════════

-- | Wire-format message for LLM conversation history.
--
-- JSON encoding: @{role: "user", content: [...]}@
data WireMessage = WireMessage
  { wmRole :: Text
  -- ^ Role: "user" | "assistant" | "system"
  , wmContent :: [WireContentBlock]
  -- ^ Content blocks
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON WireMessage where
  toJSON msg = object
    [ "role" .= msg.wmRole
    , "content" .= msg.wmContent
    ]

instance FromJSON WireMessage where
  parseJSON = withObject "WireMessage" $ \o -> WireMessage
    <$> o .: "role"
    <*> o .: "content"


-- | Wire-format content block for LLM messages.
--
-- JSON encoding uses "type" discriminator:
-- - @{type: "text", text: "hello"}@
-- - @{type: "tool_use", id: "...", name: "ask_user", input: {...}}@
-- - @{type: "tool_result", tool_use_id: "...", content: "...", is_error: false}@
data WireContentBlock
  = WCBText { wcbText :: Text }
    -- ^ Plain text content
  | WCBToolUse
      { wcbToolId :: Text
      -- ^ Unique tool use ID
      , wcbToolName :: Text
      -- ^ Tool name
      , wcbToolInput :: Value
      -- ^ Tool input (JSON)
      }
    -- ^ LLM is calling a tool
  | WCBToolResult
      { wcbToolUseId :: Text
      -- ^ ID of the tool_use this is responding to
      , wcbResultContent :: Text
      -- ^ Result content (stringified)
      , wcbIsError :: Bool
      -- ^ Whether this is an error result
      }
    -- ^ Result of a tool execution
  deriving stock (Show, Eq, Generic)

instance ToJSON WireContentBlock where
  toJSON (WCBText txt) = object
    [ "type" .= ("text" :: Text)
    , "text" .= txt
    ]
  toJSON (WCBToolUse tid name input) = object
    [ "type" .= ("tool_use" :: Text)
    , "id" .= tid
    , "name" .= name
    , "input" .= input
    ]
  toJSON (WCBToolResult tid content isErr) = object
    [ "type" .= ("tool_result" :: Text)
    , "tool_use_id" .= tid
    , "content" .= content
    , "is_error" .= isErr
    ]

instance FromJSON WireContentBlock where
  parseJSON = withObject "WireContentBlock" $ \o -> do
    (typ :: Text) <- o .: "type"
    case typ of
      "text" -> WCBText <$> o .: "text"
      "tool_use" -> WCBToolUse
        <$> o .: "id"
        <*> o .: "name"
        <*> o .: "input"
      "tool_result" -> WCBToolResult
        <$> o .: "tool_use_id"
        <*> o .: "content"
        <*> o .: "is_error"
      _ -> fail $ "Unknown WireContentBlock type: " ++ T.unpack typ


-- | Tool call request from LLM.
--
-- JSON encoding: @{id: "...", name: "ask_user", input: {...}}@
data WireToolCall = WireToolCall
  { wtcId :: Text
  -- ^ Unique ID for this tool call (used in tool_result)
  , wtcName :: Text
  -- ^ Tool name (e.g., "ask_user")
  , wtcInput :: Value
  -- ^ Tool arguments (JSON)
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON WireToolCall where
  toJSON tc = object
    [ "id" .= tc.wtcId
    , "name" .= tc.wtcName
    , "input" .= tc.wtcInput
    ]

instance FromJSON WireToolCall where
  parseJSON = withObject "WireToolCall" $ \o -> WireToolCall
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
      { lcrContent :: [WireContentBlock]
      -- ^ Final response content
      }
  | LlmNeedsTools
      { lcrToolCalls :: [WireToolCall]
      -- ^ Tools the LLM wants to call
      , lcrTextContent :: [WireContentBlock]
      -- ^ Any text content before tools (for logging)
      }
  deriving stock (Show, Eq, Generic)

instance ToJSON LlmCallResult where
  toJSON (LlmDone content) = object
    [ "type" .= ("done" :: Text)
    , "content" .= content
    ]
  toJSON (LlmNeedsTools calls content) = object
    [ "type" .= ("needs_tools" :: Text)
    , "tool_calls" .= calls
    , "content" .= content
    ]

instance FromJSON LlmCallResult where
  parseJSON = withObject "LlmCallResult" $ \o -> do
    (typ :: Text) <- o .: "type"
    case typ of
      "done" -> LlmDone <$> o .: "content"
      "needs_tools" -> LlmNeedsTools
        <$> o .: "tool_calls"
        <*> o .: "content"
      _ -> fail $ "Unknown LlmCallResult type: " ++ T.unpack typ


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECTS (WASM → TypeScript)
-- ════════════════════════════════════════════════════════════════════════════

-- | Effects that Haskell yields for TypeScript to execute.
--
-- JSON encoding matches protocol.ts: @{type: "LogInfo", eff_message: "..."}@
data SerializableEffect
  = EffLlmComplete
      { effNode :: Text
      -- ^ Which node is making this call
      , effSystemPrompt :: Text
      -- ^ System prompt
      , effUserContent :: Text
      -- ^ User content
      , effSchema :: Maybe Value
      -- ^ JSON schema for structured output
      }
  | EffLogInfo { effMessage :: Text }
  | EffLogError { effMessage :: Text }
  | EffHabitica
      { effHabOp :: Text
      -- ^ Operation name: "GetUser", "ScoreTask", "GetTasks", etc.
      , effHabPayload :: Value
      -- ^ Operation-specific payload (taskId, direction, etc.)
      }
  | EffTelegramSend
      { effTgText :: Text
      -- ^ Message text to send
      , effTgParseMode :: Text
      -- ^ Parse mode: "PlainText" | "Markdown" | "HTML"
      }
  | EffTelegramAsk
      { effTgAskText :: Text
      -- ^ Message text to display
      , effTgAskParseMode :: Text
      -- ^ Parse mode: "PlainText" | "Markdown" | "HTML"
      , effTgButtons :: [(Text, Text)]
      -- ^ Button options: [(label, callback)]
      }
  | EffLlmCall
      { effLlmNode :: Text
      -- ^ Which node is making this call
      , effLlmMessages :: [WireMessage]
      -- ^ Full conversation history
      , effLlmSchema :: Maybe Value
      -- ^ JSON schema for structured output
      , effLlmTools :: [Value]
      -- ^ Tool definitions (Anthropic format)
      }
  deriving stock (Show, Eq, Generic)

instance ToJSON SerializableEffect where
  toJSON (EffLlmComplete node sys user schema) = object $
    [ "type" .= ("LlmComplete" :: Text)
    , "eff_node" .= node
    , "eff_system_prompt" .= sys
    , "eff_user_content" .= user
    ] ++ maybe [] (\s -> ["eff_schema" .= s]) schema
  toJSON (EffLogInfo msg) = object
    [ "type" .= ("LogInfo" :: Text)
    , "eff_message" .= msg
    ]
  toJSON (EffLogError msg) = object
    [ "type" .= ("LogError" :: Text)
    , "eff_message" .= msg
    ]
  toJSON (EffHabitica op payload) = object
    [ "type" .= ("Habitica" :: Text)
    , "eff_hab_op" .= op
    , "eff_hab_payload" .= payload
    ]
  toJSON (EffTelegramSend txt parseMode) = object
    [ "type" .= ("TelegramSend" :: Text)
    , "eff_tg_text" .= txt
    , "eff_tg_parse_mode" .= parseMode
    ]
  toJSON (EffTelegramAsk txt parseMode buttons) = object
    [ "type" .= ("TelegramAsk" :: Text)
    , "eff_tg_text" .= txt
    , "eff_tg_parse_mode" .= parseMode
    , "eff_buttons" .= [[label, val] | (label, val) <- buttons]
    ]
  toJSON (EffLlmCall node msgs schema tools) = object $
    [ "type" .= ("LlmCall" :: Text)
    , "eff_node" .= node
    , "eff_messages" .= msgs
    , "eff_tools" .= tools
    ] ++ maybe [] (\s -> ["eff_schema" .= s]) schema

instance FromJSON SerializableEffect where
  parseJSON = withObject "SerializableEffect" $ \o -> do
    (typ :: Text) <- o .: "type"
    case typ of
      "LlmComplete" -> EffLlmComplete
        <$> o .: "eff_node"
        <*> o .: "eff_system_prompt"
        <*> o .: "eff_user_content"
        <*> o .:? "eff_schema"
      "LogInfo" -> EffLogInfo <$> o .: "eff_message"
      "LogError" -> EffLogError <$> o .: "eff_message"
      "Habitica" -> EffHabitica
        <$> o .: "eff_hab_op"
        <*> o .: "eff_hab_payload"
      "TelegramSend" -> EffTelegramSend
        <$> o .: "eff_tg_text"
        <*> o .: "eff_tg_parse_mode"
      "TelegramAsk" -> do
        txt <- o .: "eff_tg_text"
        parseMode <- o .: "eff_tg_parse_mode"
        buttonArrays <- o .: "eff_buttons" :: Parser [[Text]]
        let buttons = [(l, v) | [l, v] <- buttonArrays]
        pure $ EffTelegramAsk txt parseMode buttons
      "LlmCall" -> EffLlmCall
        <$> o .: "eff_node"
        <*> o .: "eff_messages"
        <*> o .:? "eff_schema"
        <*> o .: "eff_tools"
      _         -> fail $ "Unknown effect type: " ++ show typ


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT METADATA (for routing decisions)
-- ════════════════════════════════════════════════════════════════════════════

-- EffectCategory and EffectSemantics are imported from Tidepool.Effect.Metadata
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
  EffLogInfo{}       -> (Internal, FireAndForget)
  EffLogError{}      -> (Internal, FireAndForget)
  EffLlmComplete{}   -> (Internal, Blocking)
  EffLlmCall{}       -> (Internal, Blocking)      -- Raw LLM API call
  EffHabitica{}      -> (Internal, Blocking)
  EffTelegramSend{}  -> (Yielded, FireAndForget)  -- Send and continue
  EffTelegramAsk{}   -> (Yielded, Blocking)       -- Wait for button click


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
  = ResSuccess { resValue :: Maybe Value }
  | ResError { resMessage :: Text }
  deriving stock (Show, Eq, Generic)

instance ToJSON EffectResult where
  toJSON (ResSuccess val) = object $
    ("type" .= ("success" :: Text))
    : maybe [] (\v -> ["value" .= v]) val
  toJSON (ResError msg) = object
    [ "type" .= ("error" :: Text)
    , "message" .= msg
    ]

instance FromJSON EffectResult where
  parseJSON = withObject "EffectResult" $ \o -> do
    (typ :: Text) <- o .: "type"
    case typ of
      "success" -> ResSuccess <$> lookupValue o
      "error"   -> ResError <$> o .: "message"
      _         -> fail $ "Unknown result type: " ++ show typ

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
  = TelegramButton { tarResponse :: Text }
    -- ^ User clicked a valid button
  | TelegramText { tarText :: Text }
    -- ^ User sent text instead of clicking
  | TelegramStaleButton
    -- ^ User clicked a button with expired/invalid nonce
  deriving stock (Show, Eq, Generic)

instance ToJSON TelegramAskResult where
  toJSON (TelegramButton response) = object
    [ "type" .= ("button" :: Text)
    , "response" .= response
    ]
  toJSON (TelegramText txt) = object
    [ "type" .= ("text" :: Text)
    , "text" .= txt
    ]
  toJSON TelegramStaleButton = object
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
  | PhaseInNode { phaseName :: Text }
  | PhaseTransitioning { phaseFrom :: Text, phaseTo :: Text }
  | PhaseCompleted { phaseResult :: Value }
  | PhaseFailed { phaseError :: Text }
  deriving stock (Show, Eq, Generic)

instance ToJSON ExecutionPhase where
  toJSON PhaseIdle = object
    [ "type" .= ("idle" :: Text)
    ]
  toJSON (PhaseInNode name) = object
    [ "type" .= ("in_node" :: Text)
    , "nodeName" .= name
    ]
  toJSON (PhaseTransitioning from to) = object
    [ "type" .= ("transitioning" :: Text)
    , "fromNode" .= from
    , "toNode" .= to
    ]
  toJSON (PhaseCompleted result) = object
    [ "type" .= ("completed" :: Text)
    , "result" .= result
    ]
  toJSON (PhaseFailed err) = object
    [ "type" .= ("failed" :: Text)
    , "error" .= err
    ]

instance FromJSON ExecutionPhase where
  parseJSON = withObject "ExecutionPhase" $ \o -> do
    (typ :: Text) <- o .: "type"
    case typ of
      "idle"          -> pure PhaseIdle
      "in_node"       -> PhaseInNode <$> o .: "nodeName"
      "transitioning" -> PhaseTransitioning <$> o .: "fromNode" <*> o .: "toNode"
      "completed"     -> PhaseCompleted <$> o .: "result"
      "failed"        -> PhaseFailed <$> o .: "error"
      _               -> fail $ "Unknown execution phase type: " ++ show typ


-- | Runtime graph state - matches protocol.ts GraphState.
--
-- JSON encoding: @{phase: {...}, completedNodes: [...]}@
data GraphState = GraphState
  { gsPhase :: ExecutionPhase
  -- ^ Current execution phase
  , gsCompletedNodes :: [Text]
  -- ^ Nodes that have completed
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON GraphState where
  toJSON gs = object
    [ "phase" .= gs.gsPhase
    , "completedNodes" .= gs.gsCompletedNodes
    ]

instance FromJSON GraphState where
  parseJSON = withObject "GraphState" $ \o -> GraphState
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
      { soEffect :: SerializableEffect
      -- ^ Effect to execute
      , soGraphState :: GraphState
      -- ^ Current graph execution state (for observability)
      }
  | StepDone
      { soResult :: Value
      -- ^ Final result (matches Exit type)
      , soGraphState :: GraphState
      -- ^ Final graph execution state
      }
  | StepFailed
      { soError :: Text
      -- ^ Error message
      , soGraphState :: GraphState
      -- ^ Graph state at failure
      }
  deriving stock (Show, Eq, Generic)

instance ToJSON StepOutput where
  toJSON (StepYield eff gs) = object
    [ "effect" .= Just eff
    , "done" .= False
    , "stepResult" .= Null
    , "graphState" .= gs
    ]
  toJSON (StepDone result gs) = object
    [ "effect" .= Null
    , "done" .= True
    , "stepResult" .= result
    , "graphState" .= gs
    ]
  toJSON (StepFailed err gs) = object
    [ "effect" .= Null
    , "done" .= True
    , "stepResult" .= Null
    , "error" .= err
    , "graphState" .= gs
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
  { tiwTypeName :: Text
  -- ^ Simple type name, e.g., "Intent"
  , tiwTypeModule :: Text
  -- ^ Module path, e.g., "Echo"
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON TypeInfoWire where
  toJSON ti = object
    [ "typeName" .= ti.tiwTypeName
    , "typeModule" .= ti.tiwTypeModule
    ]

instance FromJSON TypeInfoWire where
  parseJSON = withObject "TypeInfoWire" $ \o -> TypeInfoWire
    <$> o .: "typeName"
    <*> o .: "typeModule"


-- | Goto target - control flow destination.
--
-- JSON encoding: @{gtTarget: "exit", gtPayloadType: {...}}@
data GotoTargetWire = GotoTargetWire
  { gtwTarget :: Text
  -- ^ Target node name, or "Exit"
  , gtwPayloadType :: TypeInfoWire
  -- ^ Type passed (for Exit transitions)
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON GotoTargetWire where
  toJSON gt = object
    [ "gtTarget" .= gt.gtwTarget
    , "gtPayloadType" .= gt.gtwPayloadType
    ]

instance FromJSON GotoTargetWire where
  parseJSON = withObject "GotoTargetWire" $ \o -> GotoTargetWire
    <$> o .: "gtTarget"
    <*> o .: "gtPayloadType"


-- | Node metadata - matches protocol.ts NodeInfo.
--
-- JSON encoding:
-- @{niName: "classify", niKind: "LLM", niNeeds: [...], niSchema: null, niGotoTargets: [...]}@
data NodeInfoWire = NodeInfoWire
  { niwName :: Text
  -- ^ Node name (record field name in Servant-style)
  , niwKind :: Text
  -- ^ "LLM" or "Logic"
  , niwNeeds :: [TypeInfoWire]
  -- ^ Types this node needs (from context)
  , niwSchema :: Maybe TypeInfoWire
  -- ^ Output type (LLM nodes only)
  , niwGotoTargets :: [GotoTargetWire]
  -- ^ Possible Goto targets (Logic nodes only)
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON NodeInfoWire where
  toJSON ni = object
    [ "niName" .= ni.niwName
    , "niKind" .= ni.niwKind
    , "niNeeds" .= ni.niwNeeds
    , "niSchema" .= ni.niwSchema
    , "niGotoTargets" .= ni.niwGotoTargets
    ]

instance FromJSON NodeInfoWire where
  parseJSON = withObject "NodeInfoWire" $ \o -> NodeInfoWire
    <$> o .: "niName"
    <*> o .: "niKind"
    <*> o .: "niNeeds"
    <*> o .: "niSchema"
    <*> o .: "niGotoTargets"


-- | Edge for graph visualization.
--
-- JSON encoding: @{eiFrom: "entry", eiTo: "classify", eiPayloadType: {...}}@
data EdgeInfoWire = EdgeInfoWire
  { eiwFrom :: Text
  -- ^ Source: "Entry" or node name
  , eiwTo :: Text
  -- ^ Target: "Exit" or node name
  , eiwPayloadType :: TypeInfoWire
  -- ^ Type carried on this edge
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON EdgeInfoWire where
  toJSON ei = object
    [ "eiFrom" .= ei.eiwFrom
    , "eiTo" .= ei.eiwTo
    , "eiPayloadType" .= ei.eiwPayloadType
    ]

instance FromJSON EdgeInfoWire where
  parseJSON = withObject "EdgeInfoWire" $ \o -> EdgeInfoWire
    <$> o .: "eiFrom"
    <*> o .: "eiTo"
    <*> o .: "eiPayloadType"


-- | Static graph metadata - matches protocol.ts GraphInfo.
--
-- JSON encoding:
-- @{name: "ExampleGraph", entryType: {...}, exitType: {...}, nodes: [...], edges: [...]}@
data GraphInfoWire = GraphInfoWire
  { giwName :: Text
  -- ^ Graph name
  , giwEntryType :: TypeInfoWire
  -- ^ Type accepted at Entry
  , giwExitType :: TypeInfoWire
  -- ^ Type produced at Exit
  , giwNodes :: [NodeInfoWire]
  -- ^ All nodes in the graph (excludes Entry/Exit)
  , giwEdges :: [EdgeInfoWire]
  -- ^ Edges for visualization (from Needs/Schema and Goto)
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON GraphInfoWire where
  toJSON gi = object
    [ "name" .= gi.giwName
    , "entryType" .= gi.giwEntryType
    , "exitType" .= gi.giwExitType
    , "nodes" .= gi.giwNodes
    , "edges" .= gi.giwEdges
    ]

instance FromJSON GraphInfoWire where
  parseJSON = withObject "GraphInfoWire" $ \o -> GraphInfoWire
    <$> o .: "name"
    <*> o .: "entryType"
    <*> o .: "exitType"
    <*> o .: "nodes"
    <*> o .: "edges"
