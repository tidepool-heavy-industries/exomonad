-- | Loki push API types.
--
-- See: https://grafana.com/docs/loki/latest/reference/loki-http-api/
module Tidepool.Observability.Types
  ( -- * Loki Configuration
    LokiConfig(..)
  , defaultLokiConfig
  , grafanaCloudConfig

    -- * Loki Push API Types
  , LokiPushRequest(..)
  , LokiStream(..)
  , StreamLabels(..)

    -- * Span Context
  , SpanContext(..)
  , newSpanContext
  , pushSpan
  , popSpan
  , currentSpan

    -- * Helpers
  , eventToLabels
  , eventToLine
  , nowNanos
  ) where

import Data.Aeson
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.ByteString.Lazy as LBS
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics (Generic)

import Tidepool.Effects.Observability (TidepoolEvent(..))


-- ════════════════════════════════════════════════════════════════════════════
-- LOKI CONFIGURATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Loki push endpoint configuration.
data LokiConfig = LokiConfig
  { lcBaseUrl  :: Text      -- ^ Base URL (e.g., "http://localhost:3100" or Grafana Cloud URL)
  , lcUser     :: Maybe Text  -- ^ Username for basic auth (Grafana Cloud user ID)
  , lcToken    :: Maybe Text  -- ^ API token for basic auth
  , lcJobLabel :: Text      -- ^ Job label for all logs (e.g., "tidepool-native")
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

-- | Create config for Grafana Cloud.
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
-- SPAN CONTEXT
-- ════════════════════════════════════════════════════════════════════════════

-- | Mutable span context for tracking nested spans.
newtype SpanContext = SpanContext
  { scStack :: IORef [Text]  -- ^ Stack of active span names
  }

-- | Create a new empty span context.
newSpanContext :: IO SpanContext
newSpanContext = SpanContext <$> newIORef []

-- | Push a span onto the stack.
pushSpan :: SpanContext -> Text -> IO ()
pushSpan ctx name = modifyIORef ctx.scStack (name :)

-- | Pop a span from the stack.
popSpan :: SpanContext -> IO ()
popSpan ctx = modifyIORef ctx.scStack (drop 1)

-- | Get the current (innermost) span name.
currentSpan :: SpanContext -> IO (Maybe Text)
currentSpan ctx = do
  stack <- readIORef ctx.scStack
  pure $ case stack of
    []    -> Nothing
    (s:_) -> Just s


-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

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
nowNanos = do
  t <- getPOSIXTime
  -- Convert to nanoseconds
  let nanos = floor (t * 1e9) :: Integer
  pure $ T.pack $ show nanos
