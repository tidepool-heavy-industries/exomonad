{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

-- | Observability effect for structured logging and tracing.
--
-- Effect type only - interpreters live in exomonad-observability-interpreter.
--
-- Supports two observability primitives:
-- 1. Events (logs) - Published to Loki for queryability
-- 2. Spans (traces) - Published to Tempo via OTLP for distributed tracing
module ExoMonad.Effects.Observability
  ( -- * Effect
    Observability (..),

    -- * Event Operations
    publishEvent,

    -- * Span Operations
    startSpan,
    endSpan,
    withSpan,
    addSpanAttribute,

    -- * Event Types
    ExoMonadEvent (..),

    -- * Span Types
    SpanKind (..),
    SpanAttribute (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withObject, withText, (.:), (.=))
import Data.Aeson.Types (Parser)
import Data.Kind (Type)
import Polysemy (Member, Sem, makeSem)

-- ════════════════════════════════════════════════════════════════════════════
-- EVENT TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Structured events for observability.
--
-- These events are published to Loki for queryability and dashboard visualization.
data ExoMonadEvent
  = LLMCallEvent
      { llmModel :: Text,
        llmPromptTokens :: Int,
        llmCompletionTokens :: Int,
        llmLatencyMs :: Int
      }
  | UserActionEvent
      { uaActionType :: Text,
        uaNodeContext :: Text
      }
  | EffectExecutionEvent
      { eeEffectType :: Text,
        eeSuccess :: Bool,
        eeLatencyMs :: Int
      }
  | ErrorEvent
      { errMessage :: Text,
        errContext :: Value
      }
  deriving (Show, Eq, Generic)

instance ToJSON ExoMonadEvent where
  toJSON (LLMCallEvent model_ prompt_ completion_ latency_) =
    object
      [ "type" .= ("llm_call" :: Text),
        "model" .= model_,
        "prompt_tokens" .= prompt_,
        "completion_tokens" .= completion_,
        "latency_ms" .= latency_
      ]
  toJSON (UserActionEvent action_ context_) =
    object
      [ "type" .= ("user_action" :: Text),
        "action_type" .= action_,
        "node_context" .= context_
      ]
  toJSON (EffectExecutionEvent effect_ success_ latency_) =
    object
      [ "type" .= ("effect_execution" :: Text),
        "effect_type" .= effect_,
        "success" .= success_,
        "latency_ms" .= latency_
      ]
  toJSON (ErrorEvent msg_ ctx_) =
    object
      [ "type" .= ("error" :: Text),
        "message" .= msg_,
        "context" .= ctx_
      ]

instance FromJSON ExoMonadEvent where
  parseJSON = withObject "ExoMonadEvent" $ \v -> do
    eventType <- v .: "type" :: Parser Text
    case eventType of
      "llm_call" ->
        LLMCallEvent
          <$> v .: "model"
          <*> v .: "prompt_tokens"
          <*> v .: "completion_tokens"
          <*> v .: "latency_ms"
      "user_action" ->
        UserActionEvent <$> v .: "action_type" <*> v .: "node_context"
      "effect_execution" ->
        EffectExecutionEvent <$> v .: "effect_type" <*> v .: "success" <*> v .: "latency_ms"
      "error" ->
        ErrorEvent <$> v .: "message" <*> v .: "context"
      _ -> fail $ "Unknown event type: " ++ show eventType

-- ════════════════════════════════════════════════════════════════════════════
-- SPAN TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Span kind for OpenTelemetry traces.
data SpanKind
  = -- | Internal operation within the application
    SpanInternal
  | -- | Outgoing request (LLM call, API call)
    SpanClient
  | -- | Incoming request (tool handler entry)
    SpanServer
  deriving (Show, Eq, Generic)

instance ToJSON SpanKind where
  toJSON SpanInternal = "internal"
  toJSON SpanClient = "client"
  toJSON SpanServer = "server"

instance FromJSON SpanKind where
  parseJSON = withText "SpanKind" $ \case
    "internal" -> pure SpanInternal
    "client" -> pure SpanClient
    "server" -> pure SpanServer
    other -> fail $ "Unknown SpanKind: " <> show other

-- | Span attributes for enriching traces.
data SpanAttribute
  = -- | Text attribute (key, value)
    AttrText Text Text
  | -- | Integer attribute (key, value)
    AttrInt Text Int
  | -- | Boolean attribute (key, value)
    AttrBool Text Bool
  deriving (Show, Eq, Generic)

instance ToJSON SpanAttribute where
  toJSON (AttrText k v) = object ["key" .= k, "value" .= object ["stringValue" .= v]]
  toJSON (AttrInt k v) = object ["key" .= k, "value" .= object ["intValue" .= v]]
  toJSON (AttrBool k v) = object ["key" .= k, "value" .= object ["boolValue" .= v]]

-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT
-- ════════════════════════════════════════════════════════════════════════════

-- | Observability effect for publishing structured events and traces.
--
-- Events are published to Loki for querying and dashboards.
-- Spans are published to Tempo via OTLP for distributed tracing.
--
-- Span tracking uses start/end operations. Use 'withSpan' for bracket-style
-- span management that ensures spans are properly closed.
data Observability m a where
  PublishEvent :: ExoMonadEvent -> Observability m ()
  -- | Start a new span, pushing onto the span stack.
  -- Returns a span ID for correlation.
  StartSpan :: Text -> SpanKind -> [SpanAttribute] -> Observability m Text
  -- | End the current span with optional status and additional attributes.
  -- If isError is True, the span is marked as failed.
  EndSpan :: Bool -> [SpanAttribute] -> Observability m ()
  -- | Add an attribute to the current span (no-op if no span active).
  AddSpanAttribute :: SpanAttribute -> Observability m ()

makeSem ''Observability

-- | Run an action within a span.
--
-- The span tracks the execution time and status of the action.
-- Span name should be descriptive (e.g., "tool:search", "llm:call").
--
-- @
-- withSpan "node:classify" SpanInternal [] $ do
--   result <- classifyInput input
--   addSpanAttribute (AttrText "result" (show result))
--   pure result
-- @
withSpan ::
  (Member Observability r) =>
  -- | Span name
  Text ->
  -- | Span kind
  SpanKind ->
  -- | Initial attributes
  [SpanAttribute] ->
  -- | Action to run
  Sem r a ->
  Sem r a
withSpan name kind attrs action = do
  _ <- startSpan name kind attrs
  result <- action
  endSpan False []
  pure result
