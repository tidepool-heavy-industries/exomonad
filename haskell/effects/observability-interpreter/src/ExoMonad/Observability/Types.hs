-- | Observability types for Loki (logs) and OTLP (traces).
--
-- Supports both:
-- - Loki push API: https://grafana.com/docs/loki/latest/reference/loki-http-api/
-- - OTLP HTTP: https://opentelemetry.io/docs/specs/otlp/
module ExoMonad.Observability.Types
  ( -- * Configuration
    ObservabilityConfig (..),
    LokiConfig (..),
    OTLPConfig (..),
    defaultLokiConfig,
    defaultOTLPConfig,
    grafanaCloudConfig,
    grafanaCloudOTLPConfig,

    -- * ID Newtypes (type-safe trace/span IDs)
    TraceId (..),
    SpanId (..),

    -- * Loki Push API Types
    LokiPushRequest (..),
    LokiStream (..),
    StreamLabels (..),

    -- * OTLP Types
    OTLPTraceRequest (..),
    OTLPResourceSpans (..),
    OTLPScopeSpans (..),
    OTLPSpan (..),
    OTLPStatus (..),
    OTLPStatusCode (..),

    -- * Trace Context
    TraceContext (..),
    newTraceContext,
    ActiveSpan (..),
    pushActiveSpan,
    popActiveSpan,
    currentActiveSpan,
    generateTraceId,
    generateSpanId,

    -- * Helpers
    eventToLabels,
    eventToLine,
    nowNanos,
    nowNanosInt,
    spanKindToInt,
  )
where

import Data.Aeson
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word64)
import ExoMonad.Effects.Observability (ExoMonadEvent (..), SpanAttribute (..), SpanKind (..))
import GHC.Generics (Generic)
import Numeric (showHex)
import System.Random (randomIO)

-- ════════════════════════════════════════════════════════════════════════════
-- ID NEWTYPES (Type-Safe Trace/Span IDs)
-- ════════════════════════════════════════════════════════════════════════════

-- | Type-safe wrapper for trace IDs (128-bit, 32 hex chars).
--
-- Prevents accidental confusion between trace IDs and span IDs.
newtype TraceId = TraceId {unTraceId :: Text}
  deriving (Show, Eq, Generic)

instance ToJSON TraceId where
  toJSON (TraceId t) = toJSON t

-- | Type-safe wrapper for span IDs (64-bit, 16 hex chars).
--
-- Prevents accidental confusion between span IDs and trace IDs.
newtype SpanId = SpanId {unSpanId :: Text}
  deriving (Show, Eq, Generic)

instance ToJSON SpanId where
  toJSON (SpanId t) = toJSON t

-- ════════════════════════════════════════════════════════════════════════════
-- CONFIGURATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Combined observability configuration.
data ObservabilityConfig
  = ObservabilityOtelConfig
      { -- | Loki config (for logs)
        loki :: Maybe LokiConfig,
        -- | OTLP config (for traces)
        otlp :: Maybe OTLPConfig,
        -- | Service name for resource attributes
        serviceName :: Text
      }
  | ObservabilitySocketConfig
      { socketPath :: FilePath
      }
  deriving (Show, Eq, Generic)

-- | Loki push endpoint configuration.
data LokiConfig = LokiConfig
  { -- | Base URL (e.g., "http://localhost:3100" or Grafana Cloud URL)
    baseUrl :: Text,
    -- | Username for basic auth (Grafana Cloud user ID)
    user :: Maybe Text,
    -- | API token for basic auth
    token :: Maybe Text,
    -- | Job label for all logs (e.g., "exomonad-native")
    jobLabel :: Text
  }
  deriving (Show, Eq, Generic)

-- | OTLP HTTP endpoint configuration.
data OTLPConfig = OTLPConfig
  { -- | OTLP endpoint (e.g., "https://otlp-gateway.../otlp/v1/traces")
    endpoint :: Text,
    -- | Username for basic auth
    user :: Maybe Text,
    -- | API token for basic auth
    token :: Maybe Text
  }
  deriving (Show, Eq, Generic)

-- | Default config for local Loki instance.
defaultLokiConfig :: LokiConfig
defaultLokiConfig =
  LokiConfig
    { baseUrl = "http://localhost:3100",
      user = Nothing,
      token = Nothing,
      jobLabel = "exomonad-native"
    }

-- | Default config for local OTLP collector.
defaultOTLPConfig :: OTLPConfig
defaultOTLPConfig =
  OTLPConfig
    { endpoint = "http://localhost:4318/v1/traces",
      user = Nothing,
      token = Nothing
    }

-- | Create Loki config for Grafana Cloud.
--
-- @
-- grafanaCloudConfig
--   "https://logs-prod-us-central1.grafana.net"
--   "123456"
--   "glc_your_token_here"
-- @
grafanaCloudConfig :: Text -> Text -> Text -> LokiConfig
grafanaCloudConfig url userId token =
  LokiConfig
    { baseUrl = url,
      user = Just userId,
      token = Just token,
      jobLabel = "exomonad-native"
    }

-- | Create OTLP config for Grafana Cloud Tempo.
--
-- @
-- grafanaCloudOTLPConfig
--   "https://otlp-gateway-prod-us-west-0.grafana.net/otlp/v1/traces"
--   "123456"
--   "glc_your_token_here"
-- @
grafanaCloudOTLPConfig :: Text -> Text -> Text -> OTLPConfig
grafanaCloudOTLPConfig endpoint userId token =
  OTLPConfig
    { endpoint = endpoint,
      user = Just userId,
      token = Just token
    }

-- ════════════════════════════════════════════════════════════════════════════
-- LOKI PUSH API TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Top-level Loki push request.
--
-- Format: @{"streams": [{"stream": {...}, "values": [...]}]}@
newtype LokiPushRequest = LokiPushRequest
  { streams :: [LokiStream]
  }
  deriving (Show, Eq, Generic)

instance ToJSON LokiPushRequest where
  toJSON req = object ["streams" .= req.streams]

-- | A single stream in a Loki push.
--
-- Each stream has labels (for indexing) and values (timestamp + log line pairs).
data LokiStream = LokiStream
  { labels :: StreamLabels,
    -- | (timestamp_nanos, log_line) pairs
    values :: [(Text, Text)]
  }
  deriving (Show, Eq, Generic)

instance ToJSON LokiStream where
  toJSON s =
    object
      [ "stream" .= s.labels,
        "values" .= map (\(ts, line) -> [ts, line]) s.values
      ]

-- | Stream labels for Loki indexing.
--
-- Only high-cardinality labels that are useful for filtering.
-- Event-specific details go in the log line JSON, not labels.
data StreamLabels = StreamLabels
  { -- | Job name (e.g., "exomonad-native")
    job :: Text,
    -- | Event type for filtering
    eventType :: Text,
    -- | Current span name (if any)
    span :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON StreamLabels where
  toJSON labels =
    object $
      catMaybes
        [ Just ("job" .= labels.job),
          Just ("event_type" .= labels.eventType),
          ("span" .=) <$> labels.span
        ]
    where
      catMaybes = foldr (\mx acc -> maybe acc (: acc) mx) []

-- ════════════════════════════════════════════════════════════════════════════
-- OTLP TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | OTLP trace request (JSON format).
--
-- See: https://opentelemetry.io/docs/specs/otlp/#otlphttp-request
newtype OTLPTraceRequest = OTLPTraceRequest
  { resourceSpans :: [OTLPResourceSpans]
  }
  deriving (Show, Eq, Generic)

instance ToJSON OTLPTraceRequest where
  toJSON req = object ["resourceSpans" .= req.resourceSpans]

-- | Resource spans group spans by resource (service, host, etc.).
data OTLPResourceSpans = OTLPResourceSpans
  { -- | Resource attributes (service.name, etc.)
    resource :: Value,
    scopeSpans :: [OTLPScopeSpans]
  }
  deriving (Show, Eq, Generic)

instance ToJSON OTLPResourceSpans where
  toJSON rs =
    object
      [ "resource" .= rs.resource,
        "scopeSpans" .= rs.scopeSpans
      ]

-- | Scope spans group spans by instrumentation scope.
data OTLPScopeSpans = OTLPScopeSpans
  { -- | Instrumentation scope (name, version)
    scope :: Value,
    spans :: [OTLPSpan]
  }
  deriving (Show, Eq, Generic)

instance ToJSON OTLPScopeSpans where
  toJSON ss =
    object
      [ "scope" .= ss.scope,
        "spans" .= ss.spans
      ]

-- | Individual OTLP span.
data OTLPSpan = OTLPSpan
  { -- | Type-safe 32 hex chars (16 bytes)
    traceId :: TraceId,
    -- | Type-safe 16 hex chars (8 bytes)
    spanId :: SpanId,
    -- | Parent span ID (if nested)
    parentSpanId :: Maybe SpanId,
    -- | Span name
    name :: Text,
    -- | Span kind (0=unspec, 1=internal, 2=server, 3=client)
    kind :: Int,
    startTimeUnixNano :: Integer,
    endTimeUnixNano :: Integer,
    -- | SpanAttribute as JSON
    attributes :: [Value],
    status :: OTLPStatus
  }
  deriving (Show, Eq, Generic)

instance ToJSON OTLPSpan where
  toJSON s =
    object $
      catMaybes
        [ Just ("traceId" .= s.traceId),
          Just ("spanId" .= s.spanId),
          ("parentSpanId" .=) <$> s.parentSpanId,
          Just ("name" .= s.name),
          Just ("kind" .= s.kind),
          Just ("startTimeUnixNano" .= show s.startTimeUnixNano),
          Just ("endTimeUnixNano" .= show s.endTimeUnixNano),
          Just ("attributes" .= s.attributes),
          Just ("status" .= s.status)
        ]
    where
      catMaybes = foldr (\mx acc -> maybe acc (: acc) mx) []

-- | Span status.
data OTLPStatus = OTLPStatus
  { code :: OTLPStatusCode,
    message :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON OTLPStatus where
  toJSON st =
    object $
      catMaybes
        [ Just ("code" .= statusCodeToInt st.code),
          ("message" .=) <$> st.message
        ]
    where
      catMaybes = foldr (\mx acc -> maybe acc (: acc) mx) []
      statusCodeToInt :: OTLPStatusCode -> Int
      statusCodeToInt StatusUnset = 0
      statusCodeToInt StatusOk = 1
      statusCodeToInt StatusError = 2

-- | OTLP status codes.
data OTLPStatusCode = StatusUnset | StatusOk | StatusError
  deriving (Show, Eq, Generic)

-- ════════════════════════════════════════════════════════════════════════════
-- TRACE CONTEXT
-- ════════════════════════════════════════════════════════════════════════════

-- | Active span information.
data ActiveSpan = ActiveSpan
  { -- | Type-safe span ID (16 hex chars)
    spanId :: SpanId,
    -- | Span name
    name :: Text,
    -- | Span kind
    kind :: SpanKind,
    -- | Start time in nanos
    startTime :: Integer,
    -- | Accumulated attributes
    attributes :: [SpanAttribute]
  }
  deriving (Show, Eq, Generic)

-- | Mutable trace context for tracking the current trace and span stack.
data TraceContext = TraceContext
  { -- | Type-safe current trace ID (32 hex chars)
    traceId :: IORef TraceId,
    -- | Stack of active spans
    spanStack :: IORef [ActiveSpan],
    -- | Spans ready to export
    completedSpans :: IORef [OTLPSpan]
  }

-- | Create a new trace context with a fresh trace ID.
newTraceContext :: IO TraceContext
newTraceContext = do
  traceId <- generateTraceId
  TraceContext
    <$> newIORef traceId
    <*> newIORef []
    <*> newIORef []

-- | Generate a random 128-bit trace ID (32 hex chars).
generateTraceId :: IO TraceId
generateTraceId = do
  w1 <- randomIO :: IO Word64
  w2 <- randomIO :: IO Word64
  pure $ TraceId $ T.pack $ padLeft 16 (showHex w1 "") <> padLeft 16 (showHex w2 "")
  where
    padLeft n s = replicate (n - length s) '0' <> s

-- | Generate a random 64-bit span ID (16 hex chars).
generateSpanId :: IO SpanId
generateSpanId = do
  w <- randomIO :: IO Word64
  pure $ SpanId $ T.pack $ padLeft 16 (showHex w "")
  where
    padLeft n s = replicate (n - length s) '0' <> s

-- | Push an active span onto the stack.
pushActiveSpan :: TraceContext -> ActiveSpan -> IO ()
pushActiveSpan ctx span_ = modifyIORef ctx.spanStack (span_ :)

-- | Pop an active span from the stack.
popActiveSpan :: TraceContext -> IO (Maybe ActiveSpan)
popActiveSpan ctx = do
  stack <- readIORef ctx.spanStack
  case stack of
    [] -> pure Nothing
    (s : rest) -> do
      writeIORef ctx.spanStack rest
      pure (Just s)

-- | Get the current (innermost) active span.
currentActiveSpan :: TraceContext -> IO (Maybe ActiveSpan)
currentActiveSpan ctx = do
  stack <- readIORef ctx.spanStack
  pure $ case stack of
    [] -> Nothing
    (s : _) -> Just s

-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Convert SpanKind to OTLP integer.
spanKindToInt :: SpanKind -> Int
spanKindToInt SpanInternal = 1
spanKindToInt SpanServer = 2
spanKindToInt SpanClient = 3

-- | Extract event type for label.
eventToLabels :: Text -> ExoMonadEvent -> StreamLabels
eventToLabels job event =
  StreamLabels
    { job = job,
      eventType = eventType event,
      span = Nothing -- Filled in by interpreter
    }
  where
    eventType (GraphTransition {}) = "graph_transition"
    eventType (LLMCallEvent {}) = "llm_call"
    eventType (UserActionEvent {}) = "user_action"
    eventType (EffectExecutionEvent {}) = "effect_execution"
    eventType (ErrorEvent {}) = "error"

-- | Convert event to JSON log line.
eventToLine :: ExoMonadEvent -> Text
eventToLine event = TL.toStrict . TLE.decodeUtf8 . encode $ toJSON event

-- | Get current time in nanoseconds as text (Loki timestamp format).
nowNanos :: IO Text
nowNanos = T.pack . show <$> nowNanosInt

-- | Get current time in nanoseconds as Integer.
nowNanosInt :: IO Integer
nowNanosInt = do
  t <- getPOSIXTime
  pure $ floor (t * 1e9)
