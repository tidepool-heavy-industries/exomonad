{-# LANGUAGE LambdaCase #-}

-- | Observability effect interpreter - Loki and OTLP HTTP clients.
--
-- Publishes:
-- - Structured JSON logs to Grafana Loki
-- - OpenTelemetry spans to Grafana Tempo via OTLP HTTP
--
-- = Usage
--
-- @
-- import ExoMonad.Effects.Observability
-- import ExoMonad.Observability.Interpreter
--
-- main :: IO ()
-- main = do
--   let config = defaultLokiConfig
--   ctx <- newTraceContext
--   runM $ runObservabilityWithContext ctx config $ do
--     withSpan "graph:example" SpanServer [] $ do
--       publishEvent $ GraphTransition "entry" "classify" "user_input"
--       publishEvent $ LLMCallEvent "claude-3" 100 50 250
-- @
module ExoMonad.Observability.Interpreter
  ( -- * Interpreter
    runObservability,
    runObservabilityWithContext,
    runObservabilityWithConfig,

    -- * Tracing via Interposition

    -- | Use @interpose@-based tracing to transparently wrap effects without
    -- removing them from the stack. This enables composable tracing that can
    -- be added or removed without changing the effect stack type.
    interposeWithLLMTracing,

    -- * Configuration
    LokiConfig (..),
    OTLPConfig (..),
    ObservabilityConfig (..),
    defaultLokiConfig,
    defaultOTLPConfig,
    grafanaCloudConfig,
    grafanaCloudOTLPConfig,

    -- * Trace Context
    TraceContext (..),
    newTraceContext,

    -- * Low-level API
    pushToLoki,
    pushToOTLP,
    flushTraces,
  )
where

import Control.Exception (SomeException, try)
import Control.Monad (void, when)
import Data.Aeson (encode, object, toJSON, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.IORef (modifyIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import ExoMonad.Effect.Types (LLM (..), TurnOutcome (..))
import ExoMonad.Effects.Observability
  ( Observability (..),
    SpanAttribute (..),
    SpanKind (..),
    endSpan,
    startSpan,
  )
import ExoMonad.Effects.SocketClient
  ( ServiceRequest (..),
    ServiceResponse (..),
    SocketConfig (..),
    sendRequest,
  )
import ExoMonad.Observability.Types
import Network.HTTP.Req
import Polysemy (Member, Sem, embed, intercept, interpret)
import Polysemy.Embed (Embed)
import Polysemy.Internal (send)

-- ════════════════════════════════════════════════════════════════════════════
-- LOKI HTTP CLIENT
-- ════════════════════════════════════════════════════════════════════════════

-- | Push a request to Loki.
--
-- Returns Nothing on success, Just error message on failure.
pushToLoki :: LokiConfig -> LokiPushRequest -> IO (Maybe Text)
pushToLoki config pushReq = do
  result <- try @SomeException $ runReq defaultHttpConfig $ do
    let body = ReqBodyLbs (encode pushReq)

    case parseUrl config.baseUrl of
      Left (httpUrl, _) -> do
        let hdrs =
              header "Content-Type" ("application/json" :: BS.ByteString)
                <> authHeadersHttp config
        void $
          req
            POST
            (httpUrl /: "loki" /: "api" /: "v1" /: "push")
            body
            ignoreResponse
            hdrs
      Right (httpsUrl, _) -> do
        let hdrs =
              header "Content-Type" ("application/json" :: BS.ByteString)
                <> authHeadersHttps config
        void $
          req
            POST
            (httpsUrl /: "loki" /: "api" /: "v1" /: "push")
            body
            ignoreResponse
            hdrs

  pure $ case result of
    -- Don't log full exception - it may contain auth headers/credentials
    Left _ -> Just "Loki push failed (check endpoint and credentials)"
    Right _ -> Nothing

-- | Parse URL into http or https variant.
parseUrl :: Text -> Either (Url 'Http, Option 'Http) (Url 'Https, Option 'Https)
parseUrl urlText
  | "https://" `T.isPrefixOf` urlText =
      let host = T.drop 8 urlText -- Remove "https://"
          (hostPart, _) = T.break (== '/') host
       in Right (https hostPart, mempty)
  | "http://" `T.isPrefixOf` urlText =
      let host = T.drop 7 urlText -- Remove "http://"
          (hostPart, _) = T.break (== '/') host
       in Left (http hostPart, mempty)
  | otherwise =
      -- Default to http for bare hostnames
      Left (http urlText, mempty)

-- | Build auth headers for Grafana Cloud (HTTP).
authHeadersHttp :: LokiConfig -> Option 'Http
authHeadersHttp config = case (config.user, config.token) of
  (Just user, Just token) ->
    let creds = TE.encodeUtf8 $ user <> ":" <> token
        b64 = B64.encode creds
        authValue = "Basic " <> b64 -- Keep as ByteString
     in header "Authorization" authValue
  _ -> mempty

-- | Build auth headers for Grafana Cloud (HTTPS).
authHeadersHttps :: LokiConfig -> Option 'Https
authHeadersHttps config = case (config.user, config.token) of
  (Just user, Just token) ->
    let creds = TE.encodeUtf8 $ user <> ":" <> token
        b64 = B64.encode creds
        authValue = "Basic " <> b64 -- Keep as ByteString
     in header "Authorization" authValue
  _ -> mempty

-- ════════════════════════════════════════════════════════════════════════════
-- OTLP HTTP CLIENT
-- ════════════════════════════════════════════════════════════════════════════

-- | Push traces to OTLP endpoint.
--
-- Returns Nothing on success, Just error message on failure.
pushToOTLP :: OTLPConfig -> OTLPTraceRequest -> IO (Maybe Text)
pushToOTLP config traceReq = do
  result <- try @SomeException $ runReq defaultHttpConfig $ do
    let body = ReqBodyLbs (encode traceReq)

    case parseOTLPUrl config.endpoint of
      Left (httpUrl, _) -> do
        let hdrs =
              header "Content-Type" ("application/json" :: BS.ByteString)
                <> authHeadersHttp' config
        void $ req POST httpUrl body ignoreResponse hdrs
      Right (httpsUrl, _) -> do
        let hdrs =
              header "Content-Type" ("application/json" :: BS.ByteString)
                <> authHeadersHttps' config
        void $ req POST httpsUrl body ignoreResponse hdrs

  pure $ case result of
    -- Don't log full exception - it may contain auth headers/credentials
    Left _ -> Just "OTLP push failed (check endpoint and credentials)"
    Right _ -> Nothing

-- | Parse OTLP URL into http or https variant.
parseOTLPUrl :: Text -> Either (Url 'Http, Option 'Http) (Url 'Https, Option 'Https)
parseOTLPUrl urlText
  | "https://" `T.isPrefixOf` urlText =
      let host = T.drop 8 urlText
          (hostPart, pathPart) = T.break (== '/') host
          pathSegments = filter (not . T.null) $ T.splitOn "/" pathPart
          baseUrl = https hostPart
          urlWithPath = foldl (/:) baseUrl pathSegments
       in Right (urlWithPath, mempty)
  | "http://" `T.isPrefixOf` urlText =
      let host = T.drop 7 urlText
          (hostPart, pathPart) = T.break (== '/') host
          pathSegments = filter (not . T.null) $ T.splitOn "/" pathPart
          baseUrl = http hostPart
          urlWithPath = foldl (/:) baseUrl pathSegments
       in Left (urlWithPath, mempty)
  | otherwise =
      Left (http urlText, mempty)

-- | Build auth headers for OTLP (HTTP).
authHeadersHttp' :: OTLPConfig -> Option 'Http
authHeadersHttp' config = case (config.user, config.token) of
  (Just user, Just token) ->
    let creds = TE.encodeUtf8 $ user <> ":" <> token
        b64 = B64.encode creds
        authValue = "Basic " <> b64
     in header "Authorization" authValue
  _ -> mempty

-- | Build auth headers for OTLP (HTTPS).
authHeadersHttps' :: OTLPConfig -> Option 'Https
authHeadersHttps' config = case (config.user, config.token) of
  (Just user, Just token) ->
    let creds = TE.encodeUtf8 $ user <> ":" <> token
        b64 = B64.encode creds
        authValue = "Basic " <> b64
     in header "Authorization" authValue
  _ -> mempty

-- ════════════════════════════════════════════════════════════════════════════
-- TRACE FLUSHING
-- ════════════════════════════════════════════════════════════════════════════

-- | Flush completed spans to OTLP endpoint.
flushTraces :: OTLPConfig -> Text -> TraceContext -> IO ()
flushTraces config serviceName ctx = do
  spans <- readIORef ctx.completedSpans
  when (not $ null spans) $ do
    -- Clear completed spans
    writeIORef ctx.completedSpans []

    -- Build OTLP request
    let resource_ =
          object
            [ "attributes"
                .= [ object ["key" .= ("service.name" :: Text), "value" .= object ["stringValue" .= serviceName]]
                   ]
            ]
        scope_ =
          object
            [ "name" .= ("exomonad" :: Text),
              "version" .= ("0.1.0" :: Text)
            ]
        scopeSpans =
          OTLPScopeSpans
            { scope = scope_,
              spans = spans
            }
        resourceSpans =
          OTLPResourceSpans
            { resource = resource_,
              scopeSpans = [scopeSpans]
            }
        traceReq = OTLPTraceRequest [resourceSpans]

    -- Push to OTLP
    mErr <- pushToOTLP config traceReq
    case mErr of
      Just err -> putStrLn $ "[OTLP] " <> T.unpack err
      Nothing -> pure ()

-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT INTERPRETER
-- ════════════════════════════════════════════════════════════════════════════

-- | Run Observability effects with Loki-only config (backward compatible).
--
-- Creates a fresh trace context for each run.
runObservability :: (Member (Embed IO) r) => LokiConfig -> Sem (Observability ': r) a -> Sem r a
runObservability lokiConfig action = do
  ctx <- embed newTraceContext
  let config = ObservabilityOtelConfig (Just lokiConfig) Nothing "exomonad"
  runObservabilityWithConfig ctx config action

-- | Deprecated: Use 'runObservabilityWithConfig' instead.
runObservabilityWithContext ::
  (Member (Embed IO) r) =>
  TraceContext ->
  LokiConfig ->
  Sem (Observability ': r) a ->
  Sem r a
runObservabilityWithContext ctx lokiConfig =
  runObservabilityWithConfig ctx (ObservabilityOtelConfig (Just lokiConfig) Nothing "exomonad")

-- | Run Observability effects with explicit trace context and configuration.
--
-- This interpreter:
-- 1. Publishes events to Loki (HTTP) or Socket
-- 2. Tracks spans in the trace context
-- 3. Handles errors gracefully (logs but doesn't throw)
runObservabilityWithConfig ::
  (Member (Embed IO) r) =>
  TraceContext ->
  ObservabilityConfig ->
  Sem (Observability ': r) a ->
  Sem r a
runObservabilityWithConfig ctx config = interpret $ \case
  PublishEvent event -> embed $ case config of
    ObservabilityOtelConfig (Just loki) _ _ -> do
      let labels = eventToLabels loki.jobLabel event
          line = eventToLine event
      ts <- nowNanos
      let stream = LokiStream labels [(ts, line)]
          pushReq = LokiPushRequest [stream]
      mErr <- pushToLoki loki pushReq
      case mErr of
        Just err -> putStrLn $ "[Observability] " <> T.unpack err
        Nothing -> pure ()
    ObservabilityOtelConfig Nothing _ _ -> pure () -- Logs disabled
    ObservabilitySocketConfig _path -> do
      -- TODO: Define socket protocol for logs if needed.
      -- For now we just ignore or log locally.
      pure ()
  StartSpan name kind attrs -> embed $ do
    spanId_ <- generateSpanId
    startTime <- nowNanosInt
    let activeSpan =
          ActiveSpan
            { spanId = spanId_,
              name = name,
              kind = kind,
              startTime = startTime,
              attributes = attrs
            }
    pushActiveSpan ctx activeSpan
    pure (spanId_.unSpanId)
  EndSpan isError extraAttrs -> embed $ do
    mSpan <- popActiveSpan ctx
    case mSpan of
      Nothing -> pure ()
      Just activeSpan -> do
        endTime <- nowNanosInt
        traceId_ <- readIORef ctx.traceId
        parentSpan <- currentActiveSpan ctx
        let parentSpanId_ = (.spanId) <$> parentSpan

        case config of
          ObservabilityOtelConfig _ (Just _otlp) _serviceName -> do
            let allAttrs = activeSpan.attributes <> extraAttrs
                attrValues = map toJSON allAttrs
                status =
                  if isError
                    then OTLPStatus StatusError (Just "error")
                    else OTLPStatus StatusOk Nothing
                otlpSpan =
                  OTLPSpan
                    { traceId = traceId_,
                      spanId = activeSpan.spanId,
                      parentSpanId = parentSpanId_,
                      name = activeSpan.name,
                      kind = spanKindToInt activeSpan.kind,
                      startTimeUnixNano = activeSpan.startTime,
                      endTimeUnixNano = endTime,
                      attributes = attrValues,
                      status = status
                    }
            modifyIORef ctx.completedSpans (otlpSpan :)
          ObservabilitySocketConfig path -> do
            let serviceReq =
                  OtelSpan
                    { traceId = traceId_.unTraceId,
                      spanId = activeSpan.spanId.unSpanId,
                      name = activeSpan.name,
                      startNs = fromIntegral activeSpan.startTime,
                      endNs = fromIntegral endTime,
                      attributes = mempty -- TODO: Convert attrs to Object if needed
                    }
            result <- sendRequest (SocketConfig path 10000) serviceReq
            case result of
              Left _ -> putStrLn "[Observability] Failed to ship span via socket"
              Right (ErrorResponse _ msg) -> putStrLn $ "[Observability] Error shipping span via socket: " <> T.unpack msg
              Right _ -> pure ()
          _ -> pure ()
  AddSpanAttribute attr -> embed $ do
    modifyIORef ctx.spanStack $ \case
      [] -> []
      (s : rest) ->
        let newS =
              ActiveSpan
                { spanId = s.spanId,
                  name = s.name,
                  kind = s.kind,
                  startTime = s.startTime,
                  attributes = s.attributes <> [attr]
                }
         in newS : rest

-- ════════════════════════════════════════════════════════════════════════════
-- TRACING VIA INTERPOSITION
-- ════════════════════════════════════════════════════════════════════════════

-- | Intercept LLM effects and wrap them in spans.
--
-- This uses Polysemy's @intercept@ to transparently trace LLM calls
-- without removing the LLM effect from the stack. The original effect is
-- re-sent after wrapping, so the actual LLM handler elsewhere in the stack
-- still executes the call.
--
-- = The Interpose Pattern
--
-- Unlike @interpret@ which removes an effect from the stack, @intercept@
-- intercepts effects while leaving them in place:
--
-- @
-- -- interpret: removes effect, handles completely
-- interpret :: (e a -> Sem r a) -> Sem (e ': r) a -> Sem r a
--
-- -- intercept: intercepts effect, can re-send it
-- intercept :: Member e r => (e a -> Sem r a) -> Sem r a -> Sem r a
-- @
--
-- = Usage
--
-- Add tracing to any computation that uses LLM effects:
--
-- @
-- -- Without tracing
-- result <- runLLM config $ myAgent input
--
-- -- With tracing (just wrap the computation)
-- result <- runLLM config $ interposeWithLLMTracing $ myAgent input
-- @
interposeWithLLMTracing ::
  (Member LLM r, Member Observability r) =>
  Sem r a ->
  Sem r a
interposeWithLLMTracing = intercept $ \case
  op@(RunTurnOp systemPrompt _userContent _schema tools) -> do
    -- Start span before LLM call
    _ <-
      startSpan
        "llm:turn"
        SpanClient
        [ AttrText "llm.system_prompt_length" (T.pack $ show $ T.length systemPrompt),
          AttrInt "llm.tool_count" (fromIntegral $ length tools)
        ]

    -- Re-send the original effect to the actual handler
    result <- send op

    -- End span after LLM call completes
    let isError = case result of
          TurnBroken _ -> True
          _ -> False
    endSpan isError []

    pure result
