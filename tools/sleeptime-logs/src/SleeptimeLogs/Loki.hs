-- | Loki HTTP client for querying logs.
--
-- Uses LogQL to query structured tidepool events from Grafana Loki.
module SleeptimeLogs.Loki
  ( -- * Query Functions
    queryTransitions
  , queryLLMCalls
  , queryErrors
  , querySession
    -- * Types
  , LogEntry(..)
  , QueryResult(..)
  ) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime, NominalDiffTime, getCurrentTime, addUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Data.Vector as V
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)

import SleeptimeLogs.Config (Config(..))
import Tidepool.Effects.Observability (TidepoolEvent(..))

-- | A single log entry from Loki.
data LogEntry = LogEntry
  { entryTimestamp :: Text
  , entryLabels :: Object
  , entryEvent :: TidepoolEvent
  }
  deriving (Show)

-- | Query result wrapper.
data QueryResult = QueryResult
  { resultEntries :: [LogEntry]
  , resultStatus :: Text
  }
  deriving (Show)

-- | Query graph transitions.
queryTransitions :: Config -> NominalDiffTime -> Maybe Int -> IO QueryResult
queryTransitions cfg since limit = do
  let query = "{app=\"tidepool\"} |= `graph_transition` | json"
  queryLoki cfg query since limit (Just filterTransitions)

-- | Query LLM calls, optionally filtering by minimum latency.
queryLLMCalls :: Config -> Maybe Int -> NominalDiffTime -> Maybe Int -> IO QueryResult
queryLLMCalls cfg minLatency since limit = do
  let baseQuery = "{app=\"tidepool\"} |= `llm_call` | json"
      query = case minLatency of
        Just ms -> baseQuery <> " | latency_ms >= " <> T.pack (show ms)
        Nothing -> baseQuery
  queryLoki cfg query since limit (Just filterLLMCalls)

-- | Query errors.
queryErrors :: Config -> NominalDiffTime -> Maybe Int -> IO QueryResult
queryErrors cfg since limit = do
  let query = "{app=\"tidepool\"} |= `error` | json"
  queryLoki cfg query since limit (Just filterErrors)

-- | Query all events for a session.
querySession :: Config -> Text -> Maybe Int -> IO QueryResult
querySession cfg sessionId limit = do
  let query = "{app=\"tidepool\", session_id=\"" <> sessionId <> "\"} | json"
  -- Use 7 days as default for session queries
  queryLoki cfg query (7 * 24 * 3600) limit Nothing

-- | Filter functions for different event types.
filterTransitions :: TidepoolEvent -> Bool
filterTransitions GraphTransition{} = True
filterTransitions _ = False

filterLLMCalls :: TidepoolEvent -> Bool
filterLLMCalls LLMCallEvent{} = True
filterLLMCalls _ = False

filterErrors :: TidepoolEvent -> Bool
filterErrors ErrorEvent{} = True
filterErrors _ = False

-- | Execute a LogQL query against Loki.
queryLoki
  :: Config
  -> Text                          -- ^ LogQL query
  -> NominalDiffTime               -- ^ Time range (how far back)
  -> Maybe Int                     -- ^ Result limit
  -> Maybe (TidepoolEvent -> Bool) -- ^ Optional filter
  -> IO QueryResult
queryLoki cfg query since limit mFilter = do
  manager <- newManager tlsManagerSettings
  now <- getCurrentTime
  let startTime = addUTCTime (negate since) now
      endNanos = utcToNanos now
      startNanos = utcToNanos startTime
      limitN = maybe cfg.defaultLimit id limit

  request <- buildRequest cfg query startNanos endNanos limitN

  response <- httpLbs request manager

  case eitherDecode (responseBody response) of
    Left err -> pure QueryResult
      { resultEntries = []
      , resultStatus = "error: " <> T.pack err
      }
    Right entries ->
      let filtered = case mFilter of
            Just f -> filter (f . entryEvent) entries
            Nothing -> entries
      in pure QueryResult
        { resultEntries = filtered
        , resultStatus = "success"
        }

-- | Build an HTTP request for Loki query.
buildRequest :: Config -> Text -> Integer -> Integer -> Int -> IO Request
buildRequest cfg query start end limit = do
  let url = T.unpack cfg.lokiUrl <> "/loki/api/v1/query_range"
      queryParams =
        [ ("query", Just $ TE.encodeUtf8 query)
        , ("start", Just $ BS.pack $ show start)
        , ("end", Just $ BS.pack $ show end)
        , ("limit", Just $ BS.pack $ show limit)
        ]

  baseRequest <- parseRequest url
  let request = setQueryString queryParams baseRequest

  -- Add auth if configured
  pure $ case (cfg.lokiUser, cfg.lokiToken) of
    (Just user, Just token) ->
      applyBasicAuth (TE.encodeUtf8 user) (TE.encodeUtf8 token) request
    _ -> request

-- | Convert UTCTime to nanoseconds since epoch.
utcToNanos :: UTCTime -> Integer
utcToNanos t = floor (utcTimeToPOSIXSeconds t * 1000000000)

-- | Parse Loki query response.
instance FromJSON LogEntry where
  parseJSON = withObject "LogEntry" $ \v -> do
    -- Loki returns [timestamp_ns, log_line] pairs
    -- We need to parse nested structure
    LogEntry
      <$> v .: "timestamp"
      <*> v .:? "stream" .!= mempty
      <*> (v .: "line" >>= parseEventFromLine)

parseEventFromLine :: Text -> Parser TidepoolEvent
parseEventFromLine line =
  case eitherDecodeStrict (TE.encodeUtf8 line) of
    Left err -> fail $ "Failed to parse event: " <> err
    Right event -> pure event

-- | Parse Loki query_range response format.
instance {-# OVERLAPPING #-} FromJSON [LogEntry] where
  parseJSON = withObject "LokiResponse" $ \v -> do
    status <- v .: "status"
    if status /= ("success" :: Text)
      then pure []
      else do
        result <- v .: "data" >>= (.: "result")
        case result of
          Array streams -> do
            entries <- mapM parseStream (V.toList streams)
            pure (concat entries)
          _ -> pure []

parseStream :: Value -> Parser [LogEntry]
parseStream = withObject "Stream" $ \v -> do
  stream <- v .:? "stream" .!= mempty
  values <- v .: "values" :: Parser [[Value]]
  mapM (parseLogValue stream) values

parseLogValue :: Object -> [Value] -> Parser LogEntry
parseLogValue stream [String ts, String line] = do
  event <- parseEventFromLine line
  pure LogEntry
    { entryTimestamp = ts
    , entryLabels = stream
    , entryEvent = event
    }
parseLogValue _ _ = fail "Expected [timestamp, line] pair"
