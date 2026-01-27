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
    runObservability
  , runObservabilityWithContext
  , runObservabilityWithConfig

    -- * Tracing via Interposition
    -- | Use @interpose@-based tracing to transparently wrap effects without
    -- removing them from the stack. This enables composable tracing that can
    -- be added or removed without changing the effect stack type.
  , interposeWithLLMTracing

    -- * Configuration
  , LokiConfig(..)
  , OTLPConfig(..)
  , ObservabilityConfig(..)
  , defaultLokiConfig
  , defaultOTLPConfig
  , grafanaCloudConfig
  , grafanaCloudOTLPConfig

    -- * Trace Context
  , TraceContext(..)
  , newTraceContext

    -- * Low-level API
  , pushToLoki
  , pushToOTLP
  , flushTraces
  ) where

import Control.Exception (try, SomeException)
import Control.Monad (void, when)
import Control.Monad.Freer (Eff, LastMember, interpret, sendM, Member, send, interpose)
import Data.Aeson (encode, object, (.=), toJSON)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import Data.IORef (readIORef, writeIORef, modifyIORef)
import Network.HTTP.Req
import qualified Data.ByteString.Base64 as B64

import ExoMonad.Effects.Observability
  ( Observability(..), SpanKind(..), SpanAttribute(..)
  , startSpan, endSpan
  )
import ExoMonad.Effects.SocketClient
  ( SocketConfig(..)
  , ServiceRequest(..)
  , ServiceResponse(..)
  , sendRequest
  )
import ExoMonad.Effect.Types (LLM(..), TurnOutcome(..))
import ExoMonad.Observability.Types


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

    case parseUrl config.lcBaseUrl of
      Left (httpUrl, _) -> do
        let hdrs = header "Content-Type" ("application/json" :: BS.ByteString)
                <> authHeadersHttp config
        void $ req POST (httpUrl /: "loki" /: "api" /: "v1" /: "push")
                   body ignoreResponse hdrs
      Right (httpsUrl, _) -> do
        let hdrs = header "Content-Type" ("application/json" :: BS.ByteString)
                <> authHeadersHttps config
        void $ req POST (httpsUrl /: "loki" /: "api" /: "v1" /: "push")
                   body ignoreResponse hdrs

  pure $ case result of
    -- Don't log full exception - it may contain auth headers/credentials
    Left _  -> Just "Loki push failed (check endpoint and credentials)"
    Right _ -> Nothing

-- | Parse URL into http or https variant.
parseUrl :: Text -> Either (Url 'Http, Option 'Http) (Url 'Https, Option 'Https)
parseUrl urlText
  | "https://" `T.isPrefixOf` urlText =
      let host = T.drop 8 urlText  -- Remove "https://"
          (hostPart, _) = T.break (== '/') host
      in Right (https hostPart, mempty)
  | "http://" `T.isPrefixOf` urlText =
      let host = T.drop 7 urlText  -- Remove "http://"
          (hostPart, _) = T.break (== '/') host
      in Left (http hostPart, mempty)
  | otherwise =
      -- Default to http for bare hostnames
      Left (http urlText, mempty)

-- | Build auth headers for Grafana Cloud (HTTP).
authHeadersHttp :: LokiConfig -> Option 'Http
authHeadersHttp config = case (config.lcUser, config.lcToken) of
  (Just user, Just token) ->
    let creds = TE.encodeUtf8 $ user <> ":" <> token
        b64 = B64.encode creds
        authValue = "Basic " <> b64  -- Keep as ByteString
    in header "Authorization" authValue
  _ -> mempty

-- | Build auth headers for Grafana Cloud (HTTPS).
authHeadersHttps :: LokiConfig -> Option 'Https
authHeadersHttps config = case (config.lcUser, config.lcToken) of
  (Just user, Just token) ->
    let creds = TE.encodeUtf8 $ user <> ":" <> token
        b64 = B64.encode creds
        authValue = "Basic " <> b64  -- Keep as ByteString
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

    case parseOTLPUrl config.otlpEndpoint of
      Left (httpUrl, _) -> do
        let hdrs = header "Content-Type" ("application/json" :: BS.ByteString)
                <> authHeadersHttp' config
        void $ req POST httpUrl body ignoreResponse hdrs
      Right (httpsUrl, _) -> do
        let hdrs = header "Content-Type" ("application/json" :: BS.ByteString)
                <> authHeadersHttps' config
        void $ req POST httpsUrl body ignoreResponse hdrs

  pure $ case result of
    -- Don't log full exception - it may contain auth headers/credentials
    Left _  -> Just "OTLP push failed (check endpoint and credentials)"
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
authHeadersHttp' config = case (config.otlpUser, config.otlpToken) of
  (Just user, Just token) ->
    let creds = TE.encodeUtf8 $ user <> ":" <> token
        b64 = B64.encode creds
        authValue = "Basic " <> b64
    in header "Authorization" authValue
  _ -> mempty

-- | Build auth headers for OTLP (HTTPS).
authHeadersHttps' :: OTLPConfig -> Option 'Https
authHeadersHttps' config = case (config.otlpUser, config.otlpToken) of
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
  spans <- readIORef ctx.tcCompletedSpans
  when (not $ null spans) $ do
    -- Clear completed spans
    writeIORef ctx.tcCompletedSpans []

    -- Build OTLP request
    let resource = object
          [ "attributes" .=
              [ object ["key" .= ("service.name" :: Text), "value" .= object ["stringValue" .= serviceName]]
              ]
          ]
        scope = object
          [ "name" .= ("exomonad" :: Text)
          , "version" .= ("0.1.0" :: Text)
          ]
        scopeSpans = OTLPScopeSpans scope spans
        resourceSpans = OTLPResourceSpans resource [scopeSpans]
        traceReq = OTLPTraceRequest [resourceSpans]

    -- Push to OTLP
    mErr <- pushToOTLP config traceReq
    case mErr of
      Just err -> putStrLn $ "[OTLP] " <> T.unpack err
      Nothing  -> pure ()


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT INTERPRETER
-- ════════════════════════════════════════════════════════════════════════════

-- | Run Observability effects with Loki-only config (backward compatible).
--
-- Creates a fresh trace context for each run.
runObservability :: LastMember IO effs => LokiConfig -> Eff (Observability ': effs) a -> Eff effs a
runObservability lokiConfig action = do
  ctx <- sendM newTraceContext
  let config = ObservabilityOtelConfig (Just lokiConfig) Nothing "exomonad"
  runObservabilityWithConfig ctx config action

-- | Deprecated: Use 'runObservabilityWithConfig' instead.
runObservabilityWithContext
  :: LastMember IO effs
  => TraceContext
  -> LokiConfig
  -> Eff (Observability ': effs) a
  -> Eff effs a
runObservabilityWithContext ctx lokiConfig =
  runObservabilityWithConfig ctx (ObservabilityOtelConfig (Just lokiConfig) Nothing "exomonad")

-- | Run Observability effects with explicit trace context and configuration.
--
-- This interpreter:
-- 1. Publishes events to Loki (HTTP) or Socket
-- 2. Tracks spans in the trace context
-- 3. Handles errors gracefully (logs but doesn't throw)
runObservabilityWithConfig
  :: LastMember IO effs
  => TraceContext
  -> ObservabilityConfig
  -> Eff (Observability ': effs) a
  -> Eff effs a
runObservabilityWithConfig ctx config = interpret $ \case
  PublishEvent event -> sendM $ case config of
    ObservabilityOtelConfig (Just loki) _ _ -> do
      let labels = eventToLabels loki.lcJobLabel event
          line = eventToLine event
      ts <- nowNanos
      let stream = LokiStream labels [(ts, line)]
          pushReq = LokiPushRequest [stream]
      mErr <- pushToLoki loki pushReq
      case mErr of
        Just err -> putStrLn $ "[Observability] " <> T.unpack err
        Nothing  -> pure ()
    ObservabilityOtelConfig Nothing _ _ -> pure () -- Logs disabled
    ObservabilitySocketConfig _path -> do
      -- TODO: Define socket protocol for logs if needed.
      -- For now we just ignore or log locally.
      pure ()

  StartSpan name kind attrs -> sendM $ do
    spanId <- generateSpanId
    startTime <- nowNanosInt
    let activeSpan = ActiveSpan
          { asSpanId = spanId
          , asName = name
          , asKind = kind
          , asStartTime = startTime
          , asAttributes = attrs
          }
    pushActiveSpan ctx activeSpan
    pure (unSpanId spanId)

  EndSpan isError extraAttrs -> sendM $ do
    mSpan <- popActiveSpan ctx
    case mSpan of
      Nothing -> pure ()
      Just activeSpan -> do
        endTime <- nowNanosInt
        traceId <- readIORef ctx.tcTraceId
        parentSpan <- currentActiveSpan ctx
        let parentSpanId = asSpanId <$> parentSpan

        case config of
          ObservabilityOtelConfig _ (Just _otlp) _serviceName -> do
            let allAttrs = activeSpan.asAttributes <> extraAttrs
                attrValues = map toJSON allAttrs
                status = if isError
                         then OTLPStatus StatusError (Just "error")
                         else OTLPStatus StatusOk Nothing
                otlpSpan = OTLPSpan
                  { ospTraceId = traceId
                  , ospSpanId = activeSpan.asSpanId
                  , ospParentSpanId = parentSpanId
                  , ospName = activeSpan.asName
                  , ospKind = spanKindToInt activeSpan.asKind
                  , ospStartTimeUnixNano = activeSpan.asStartTime
                  , ospEndTimeUnixNano = endTime
                  , ospAttributes = attrValues
                  , ospStatus = status
                  }
            modifyIORef ctx.tcCompletedSpans (otlpSpan :)
          
          ObservabilitySocketConfig path -> do
            let serviceReq = OtelSpan (unTraceId traceId) (unSpanId activeSpan.asSpanId) activeSpan.asName
            result <- sendRequest (SocketConfig path 10000) serviceReq
            case result of
              Left _ -> putStrLn "[Observability] Failed to ship span via socket"
              Right (ErrorResponse _ msg) -> putStrLn $ "[Observability] Error shipping span via socket: " <> T.unpack msg
              Right _ -> pure ()

          _ -> pure ()

  AddSpanAttribute attr -> sendM $ do
    modifyIORef ctx.tcSpanStack $ \case
      [] -> []
      (s:rest) -> s { asAttributes = s.asAttributes <> [attr] } : rest


-- ════════════════════════════════════════════════════════════════════════════
-- TRACING VIA INTERPOSITION
-- ════════════════════════════════════════════════════════════════════════════

-- | Intercept LLM effects and wrap them in spans.
--
-- This uses freer-simple's @interpose@ to transparently trace LLM calls
-- without removing the LLM effect from the stack. The original effect is
-- re-sent after wrapping, so the actual LLM handler elsewhere in the stack
-- still executes the call.
--
-- = The Interpose Pattern
--
-- Unlike @interpret@ which removes an effect from the stack, @interpose@
-- intercepts effects while leaving them in place:
--
-- @
-- -- interpret: removes effect, handles completely
-- interpret :: (e a -> Eff es a) -> Eff (e ': es) a -> Eff es a
--
-- -- interpose: intercepts effect, can re-send it
-- interpose :: Member e es => (e a -> Eff es a) -> Eff es a -> Eff es a
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
--
-- = Why This Matters
--
-- Traditional approach requires duplicating logic:
--
-- @
-- -- BAD: Must duplicate dispatch typeclass for traced/untraced versions
-- class DispatchGoto graph ...        -- Untraced
-- class DispatchGotoTraced graph ...  -- Traced (duplicate instances!)
-- @
--
-- With interposition:
--
-- @
-- -- GOOD: Single dispatch, optional tracing wrapper
-- result <- interposeWithLLMTracing $ dispatchGoto graph choice
-- @
--
-- The tracing concern is separated from the dispatch logic.
interposeWithLLMTracing
  :: (Member LLM es, Member Observability es)
  => Eff es a
  -> Eff es a
interposeWithLLMTracing = interpose $ \case
  op@(RunTurnOp _meta systemPrompt _userContent _schema tools) -> do
    -- Start span before LLM call
    _ <- startSpan "llm:turn" SpanClient
      [ AttrText "llm.system_prompt_length" (T.pack $ show $ T.length systemPrompt)
      , AttrInt "llm.tool_count" (fromIntegral $ length tools)
      ]

    -- Re-send the original effect to the actual handler
    result <- send op

    -- End span after LLM call completes
    let isError = case result of
          TurnBroken _ -> True
          _ -> False
    endSpan isError []

    pure result
