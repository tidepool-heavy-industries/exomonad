-- | Observability effect for structured logging and tracing.
--
-- Effect type only - interpreters live in tidepool-observability-interpreter.
--
-- Supports two observability primitives:
-- 1. Events (logs) - Published to Loki for queryability
-- 2. Spans (traces) - Published to Tempo via OTLP for distributed tracing
module Tidepool.Effects.Observability
  ( -- * Effect
    Observability(..)

    -- * Event Operations
  , publishEvent

    -- * Span Operations
  , startSpan
  , endSpan
  , withSpan
  , addSpanAttribute

    -- * Event Types
  , TidepoolEvent(..)

    -- * Span Types
  , SpanKind(..)
  , SpanAttribute(..)
  ) where

import Data.Text (Text)
import Data.Aeson (Value, ToJSON(..), FromJSON(..), object, (.=), withObject, (.:))
import qualified Data.Aeson.Types as Aeson
import GHC.Generics (Generic)
import Control.Monad.Freer (Eff, Member, send)


-- ════════════════════════════════════════════════════════════════════════════
-- EVENT TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Structured events for observability.
--
-- These events are published to Loki for queryability and dashboard visualization.
data TidepoolEvent
  = GraphTransition
      { getFrom :: Text
      , getTo :: Text
      , getTrigger :: Text
      }
  | LLMCallEvent
      { llmModel :: Text
      , llmPromptTokens :: Int
      , llmCompletionTokens :: Int
      , llmLatencyMs :: Int
      }
  | UserActionEvent
      { uaActionType :: Text
      , uaNodeContext :: Text
      }
  | EffectExecutionEvent
      { eeEffectType :: Text
      , eeSuccess :: Bool
      , eeLatencyMs :: Int
      }
  | ErrorEvent
      { errMessage :: Text
      , errContext :: Value
      }
  deriving (Show, Eq, Generic)

instance ToJSON TidepoolEvent where
  toJSON (GraphTransition from_ to_ trigger_) = object
    [ "type" .= ("graph_transition" :: Text)
    , "from" .= from_
    , "to" .= to_
    , "trigger" .= trigger_
    ]
  toJSON (LLMCallEvent model_ prompt_ completion_ latency_) = object
    [ "type" .= ("llm_call" :: Text)
    , "model" .= model_
    , "prompt_tokens" .= prompt_
    , "completion_tokens" .= completion_
    , "latency_ms" .= latency_
    ]
  toJSON (UserActionEvent action_ context_) = object
    [ "type" .= ("user_action" :: Text)
    , "action_type" .= action_
    , "node_context" .= context_
    ]
  toJSON (EffectExecutionEvent effect_ success_ latency_) = object
    [ "type" .= ("effect_execution" :: Text)
    , "effect_type" .= effect_
    , "success" .= success_
    , "latency_ms" .= latency_
    ]
  toJSON (ErrorEvent msg_ ctx_) = object
    [ "type" .= ("error" :: Text)
    , "message" .= msg_
    , "context" .= ctx_
    ]

instance FromJSON TidepoolEvent where
  parseJSON = withObject "TidepoolEvent" $ \v -> do
    eventType <- v .: "type" :: Aeson.Parser Text
    case eventType of
      "graph_transition" ->
        GraphTransition <$> v .: "from" <*> v .: "to" <*> v .: "trigger"
      "llm_call" ->
        LLMCallEvent <$> v .: "model" <*> v .: "prompt_tokens"
                     <*> v .: "completion_tokens" <*> v .: "latency_ms"
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
  = SpanInternal  -- ^ Internal operation within the application
  | SpanClient    -- ^ Outgoing request (LLM call, API call)
  | SpanServer    -- ^ Incoming request (graph entry)
  deriving (Show, Eq, Generic)

instance ToJSON SpanKind where
  toJSON SpanInternal = "internal"
  toJSON SpanClient   = "client"
  toJSON SpanServer   = "server"

instance FromJSON SpanKind where
  parseJSON = Aeson.withText "SpanKind" $ \case
    "internal" -> pure SpanInternal
    "client"   -> pure SpanClient
    "server"   -> pure SpanServer
    other      -> fail $ "Unknown SpanKind: " <> show other

-- | Span attributes for enriching traces.
data SpanAttribute
  = AttrText Text Text      -- ^ Text attribute (key, value)
  | AttrInt Text Int        -- ^ Integer attribute (key, value)
  | AttrBool Text Bool      -- ^ Boolean attribute (key, value)
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
data Observability r where
  PublishEvent :: TidepoolEvent -> Observability ()
  -- | Start a new span, pushing onto the span stack.
  -- Returns a span ID for correlation.
  StartSpan :: Text -> SpanKind -> [SpanAttribute] -> Observability Text
  -- | End the current span with optional status and additional attributes.
  -- If isError is True, the span is marked as failed.
  EndSpan :: Bool -> [SpanAttribute] -> Observability ()
  -- | Add an attribute to the current span (no-op if no span active).
  AddSpanAttribute :: SpanAttribute -> Observability ()


-- ════════════════════════════════════════════════════════════════════════════
-- SMART CONSTRUCTORS
-- ════════════════════════════════════════════════════════════════════════════

-- | Publish an observability event.
publishEvent :: Member Observability effs => TidepoolEvent -> Eff effs ()
publishEvent = send . PublishEvent

-- | Start a new span.
--
-- Returns a span ID. Must be paired with 'endSpan'.
-- Prefer 'withSpan' for automatic span management.
startSpan
  :: Member Observability effs
  => Text           -- ^ Span name
  -> SpanKind       -- ^ Span kind
  -> [SpanAttribute] -- ^ Initial attributes
  -> Eff effs Text  -- ^ Span ID
startSpan name kind attrs = send (StartSpan name kind attrs)

-- | End the current span.
--
-- @isError@ should be True if the span represents a failed operation.
endSpan
  :: Member Observability effs
  => Bool           -- ^ Is error?
  -> [SpanAttribute] -- ^ Final attributes to add
  -> Eff effs ()
endSpan isError attrs = send (EndSpan isError attrs)

-- | Run an action within a span.
--
-- The span tracks the execution time and status of the action.
-- Span name should be descriptive (e.g., "graph:classify", "llm:call").
--
-- @
-- withSpan "node:classify" SpanInternal [] $ do
--   result <- classifyInput input
--   addSpanAttribute (AttrText "result" (show result))
--   pure result
-- @
withSpan
  :: Member Observability effs
  => Text           -- ^ Span name
  -> SpanKind       -- ^ Span kind
  -> [SpanAttribute] -- ^ Initial attributes
  -> Eff effs a     -- ^ Action to run
  -> Eff effs a
withSpan name kind attrs action = do
  _ <- startSpan name kind attrs
  result <- action
  endSpan False []
  pure result

-- | Add an attribute to the current span.
--
-- No-op if no span is active.
addSpanAttribute :: Member Observability effs => SpanAttribute -> Eff effs ()
addSpanAttribute = send . AddSpanAttribute
