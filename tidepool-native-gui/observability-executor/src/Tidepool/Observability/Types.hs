-- | Observability types for Loki (logs) and OTLP (traces).
--
-- Supports both:
-- - Loki push API: https://grafana.com/docs/loki/latest/reference/loki-http-api/
-- - OTLP HTTP: https://opentelemetry.io/docs/specs/otlp/
module Tidepool.Observability.Types
  ( -- * Configuration
    ObservabilityConfig(..)
  , LokiConfig(..)
  , OTLPConfig(..)
  , defaultLokiConfig
  , defaultOTLPConfig
  , grafanaCloudConfig
  , grafanaCloudOTLPConfig

    -- * Loki Push API Types
  , LokiPushRequest(..)
  , LokiStream(..)
  , StreamLabels(..)

    -- * OTLP Types
  , OTLPTraceRequest(..)
  , OTLPResourceSpans(..)
  , OTLPScopeSpans(..)
  , OTLPSpan(..)
  , OTLPStatus(..)
  , OTLPStatusCode(..)

    -- * Trace Context
  , TraceContext(..)
  , newTraceContext
  , ActiveSpan(..)
  , pushActiveSpan
  , popActiveSpan
  , currentActiveSpan
  , generateTraceId
  , generateSpanId

    -- * Helpers
  , eventToLabels
  , eventToLine
  , nowNanos
  , nowNanosInt
  , spanKindToInt
  ) where

import Data.Aeson
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.ByteString.Lazy as LBS
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word64)
import GHC.Generics (Generic)
import System.Random (randomIO)
import Numeric (showHex)

import Tidepool.Effects.Observability (TidepoolEvent(..), SpanKind(..), SpanAttribute(..))


-- ════════════════════════════════════════════════════════════════════════════
-- CONFIGURATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Combined observability configuration.
data ObservabilityConfig = ObservabilityConfig
  { ocLoki :: Maybe LokiConfig   -- ^ Loki config (for logs)
  , ocOTLP :: Maybe OTLPConfig   -- ^ OTLP config (for traces)
  , ocServiceName :: Text        -- ^ Service name for resource attributes
  }
  deriving (Show, Eq, Generic)

-- | Loki push endpoint configuration.
data LokiConfig = LokiConfig
  { lcBaseUrl  :: Text      -- ^ Base URL (e.g., "http://localhost:3100" or Grafana Cloud URL)
  , lcUser     :: Maybe Text  -- ^ Username for basic auth (Grafana Cloud user ID)
  , lcToken    :: Maybe Text  -- ^ API token for basic auth
  , lcJobLabel :: Text      -- ^ Job label for all logs (e.g., "tidepool-native")
  }
  deriving (Show, Eq, Generic)

-- | OTLP HTTP endpoint configuration.
data OTLPConfig = OTLPConfig
  { otlpEndpoint :: Text       -- ^ OTLP endpoint (e.g., "https://otlp-gateway.../otlp/v1/traces")
  , otlpUser     :: Maybe Text -- ^ Username for basic auth
  , otlpToken    :: Maybe Text -- ^ API token for basic auth
  }
  deriving (Show, Eq, Generic)

-- | Default config for local Loki instance.
defaultLokiConfig :: LokiConfig
defaultLokiConfig = LokiConfig
  { lcBaseUrl  = "http://localhost:3100"
  , lcUser     = Nothing
  , lcToken    = Nothing
  , lcJobLabel = "tidepool-native"
  }

-- | Default config for local OTLP collector.
defaultOTLPConfig :: OTLPConfig
defaultOTLPConfig = OTLPConfig
  { otlpEndpoint = "http://localhost:4318/v1/traces"
  , otlpUser     = Nothing
  , otlpToken    = Nothing
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
grafanaCloudConfig url userId token = LokiConfig
  { lcBaseUrl  = url
  , lcUser     = Just userId
  , lcToken    = Just token
  , lcJobLabel = "tidepool-native"
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
grafanaCloudOTLPConfig endpoint userId token = OTLPConfig
  { otlpEndpoint = endpoint
  , otlpUser     = Just userId
  , otlpToken    = Just token
  }


-- ════════════════════════════════════════════════════════════════════════════
-- LOKI PUSH API TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Top-level Loki push request.
--
-- Format: @{"streams": [{"stream": {...}, "values": [...]}]}@
newtype LokiPushRequest = LokiPushRequest
  { lprStreams :: [LokiStream]
  }
  deriving (Show, Eq, Generic)

instance ToJSON LokiPushRequest where
  toJSON req = object ["streams" .= req.lprStreams]

-- | A single stream in a Loki push.
--
-- Each stream has labels (for indexing) and values (timestamp + log line pairs).
data LokiStream = LokiStream
  { lsLabels :: StreamLabels
  , lsValues :: [(Text, Text)]  -- ^ (timestamp_nanos, log_line) pairs
  }
  deriving (Show, Eq, Generic)

instance ToJSON LokiStream where
  toJSON s = object
    [ "stream" .= s.lsLabels
    , "values" .= map (\(ts, line) -> [ts, line]) s.lsValues
    ]

-- | Stream labels for Loki indexing.
--
-- Only high-cardinality labels that are useful for filtering.
-- Event-specific details go in the log line JSON, not labels.
data StreamLabels = StreamLabels
  { slJob       :: Text      -- ^ Job name (e.g., "tidepool-native")
  , slEventType :: Text      -- ^ Event type for filtering
  , slSpan      :: Maybe Text  -- ^ Current span name (if any)
  }
  deriving (Show, Eq, Generic)

instance ToJSON StreamLabels where
  toJSON labels = object $ catMaybes
    [ Just ("job" .= labels.slJob)
    , Just ("event_type" .= labels.slEventType)
    , ("span" .=) <$> labels.slSpan
    ]
    where
      catMaybes = foldr (\mx acc -> maybe acc (:acc) mx) []


-- ════════════════════════════════════════════════════════════════════════════
-- OTLP TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | OTLP trace request (JSON format).
--
-- See: https://opentelemetry.io/docs/specs/otlp/#otlphttp-request
newtype OTLPTraceRequest = OTLPTraceRequest
  { otrResourceSpans :: [OTLPResourceSpans]
  }
  deriving (Show, Eq, Generic)

instance ToJSON OTLPTraceRequest where
  toJSON req = object ["resourceSpans" .= req.otrResourceSpans]

-- | Resource spans group spans by resource (service, host, etc.).
data OTLPResourceSpans = OTLPResourceSpans
  { orsResource :: Value           -- ^ Resource attributes (service.name, etc.)
  , orsScopeSpans :: [OTLPScopeSpans]
  }
  deriving (Show, Eq, Generic)

instance ToJSON OTLPResourceSpans where
  toJSON rs = object
    [ "resource" .= rs.orsResource
    , "scopeSpans" .= rs.orsScopeSpans
    ]

-- | Scope spans group spans by instrumentation scope.
data OTLPScopeSpans = OTLPScopeSpans
  { ossScope :: Value        -- ^ Instrumentation scope (name, version)
  , ossSpans :: [OTLPSpan]
  }
  deriving (Show, Eq, Generic)

instance ToJSON OTLPScopeSpans where
  toJSON ss = object
    [ "scope" .= ss.ossScope
    , "spans" .= ss.ossSpans
    ]

-- | Individual OTLP span.
data OTLPSpan = OTLPSpan
  { ospTraceId :: Text           -- ^ 32 hex chars (16 bytes)
  , ospSpanId :: Text            -- ^ 16 hex chars (8 bytes)
  , ospParentSpanId :: Maybe Text -- ^ Parent span ID (if nested)
  , ospName :: Text              -- ^ Span name
  , ospKind :: Int               -- ^ Span kind (0=unspec, 1=internal, 2=server, 3=client)
  , ospStartTimeUnixNano :: Integer
  , ospEndTimeUnixNano :: Integer
  , ospAttributes :: [Value]     -- ^ SpanAttribute as JSON
  , ospStatus :: OTLPStatus
  }
  deriving (Show, Eq, Generic)

instance ToJSON OTLPSpan where
  toJSON s = object $ catMaybes
    [ Just ("traceId" .= s.ospTraceId)
    , Just ("spanId" .= s.ospSpanId)
    , ("parentSpanId" .=) <$> s.ospParentSpanId
    , Just ("name" .= s.ospName)
    , Just ("kind" .= s.ospKind)
    , Just ("startTimeUnixNano" .= show s.ospStartTimeUnixNano)
    , Just ("endTimeUnixNano" .= show s.ospEndTimeUnixNano)
    , Just ("attributes" .= s.ospAttributes)
    , Just ("status" .= s.ospStatus)
    ]
    where
      catMaybes = foldr (\mx acc -> maybe acc (:acc) mx) []

-- | Span status.
data OTLPStatus = OTLPStatus
  { osCode :: OTLPStatusCode
  , osMessage :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON OTLPStatus where
  toJSON st = object $ catMaybes
    [ Just ("code" .= statusCodeToInt st.osCode)
    , ("message" .=) <$> st.osMessage
    ]
    where
      catMaybes = foldr (\mx acc -> maybe acc (:acc) mx) []
      statusCodeToInt :: OTLPStatusCode -> Int
      statusCodeToInt StatusUnset = 0
      statusCodeToInt StatusOk    = 1
      statusCodeToInt StatusError = 2

-- | OTLP status codes.
data OTLPStatusCode = StatusUnset | StatusOk | StatusError
  deriving (Show, Eq, Generic)


-- ════════════════════════════════════════════════════════════════════════════
-- TRACE CONTEXT
-- ════════════════════════════════════════════════════════════════════════════

-- | Active span information.
data ActiveSpan = ActiveSpan
  { asSpanId :: Text           -- ^ Span ID (16 hex chars)
  , asName :: Text             -- ^ Span name
  , asKind :: SpanKind         -- ^ Span kind
  , asStartTime :: Integer     -- ^ Start time in nanos
  , asAttributes :: [SpanAttribute] -- ^ Accumulated attributes
  }
  deriving (Show, Eq, Generic)

-- | Mutable trace context for tracking the current trace and span stack.
data TraceContext = TraceContext
  { tcTraceId :: IORef Text       -- ^ Current trace ID (32 hex chars)
  , tcSpanStack :: IORef [ActiveSpan]  -- ^ Stack of active spans
  , tcCompletedSpans :: IORef [OTLPSpan] -- ^ Spans ready to export
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
generateTraceId :: IO Text
generateTraceId = do
  w1 <- randomIO :: IO Word64
  w2 <- randomIO :: IO Word64
  pure $ T.pack $ padLeft 16 (showHex w1 "") <> padLeft 16 (showHex w2 "")
  where
    padLeft n s = replicate (n - length s) '0' <> s

-- | Generate a random 64-bit span ID (16 hex chars).
generateSpanId :: IO Text
generateSpanId = do
  w <- randomIO :: IO Word64
  pure $ T.pack $ padLeft 16 (showHex w "")
  where
    padLeft n s = replicate (n - length s) '0' <> s

-- | Push an active span onto the stack.
pushActiveSpan :: TraceContext -> ActiveSpan -> IO ()
pushActiveSpan ctx span_ = modifyIORef ctx.tcSpanStack (span_ :)

-- | Pop an active span from the stack.
popActiveSpan :: TraceContext -> IO (Maybe ActiveSpan)
popActiveSpan ctx = do
  stack <- readIORef ctx.tcSpanStack
  case stack of
    [] -> pure Nothing
    (s:rest) -> do
      writeIORef ctx.tcSpanStack rest
      pure (Just s)

-- | Get the current (innermost) active span.
currentActiveSpan :: TraceContext -> IO (Maybe ActiveSpan)
currentActiveSpan ctx = do
  stack <- readIORef ctx.tcSpanStack
  pure $ case stack of
    []    -> Nothing
    (s:_) -> Just s


-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Convert SpanKind to OTLP integer.
spanKindToInt :: SpanKind -> Int
spanKindToInt SpanInternal = 1
spanKindToInt SpanServer   = 2
spanKindToInt SpanClient   = 3

-- | Extract event type for label.
eventToLabels :: Text -> TidepoolEvent -> StreamLabels
eventToLabels job event = StreamLabels
  { slJob       = job
  , slEventType = eventType event
  , slSpan      = Nothing  -- Filled in by executor
  }
  where
    eventType (GraphTransition{})      = "graph_transition"
    eventType (LLMCallEvent{})         = "llm_call"
    eventType (UserActionEvent{})      = "user_action"
    eventType (EffectExecutionEvent{}) = "effect_execution"
    eventType (ErrorEvent{})           = "error"

-- | Convert event to JSON log line.
eventToLine :: TidepoolEvent -> Text
eventToLine event = TL.toStrict . TLE.decodeUtf8 . encode $ toJSON event

-- | Get current time in nanoseconds as text (Loki timestamp format).
nowNanos :: IO Text
nowNanos = T.pack . show <$> nowNanosInt

-- | Get current time in nanoseconds as Integer.
nowNanosInt :: IO Integer
nowNanosInt = do
  t <- getPOSIXTime
  pure $ floor (t * 1e9)
