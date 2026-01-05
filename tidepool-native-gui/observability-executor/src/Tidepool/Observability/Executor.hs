-- | Observability effect executor - Loki push API client.
--
-- Publishes structured JSON logs to Grafana Loki.
--
-- NOTE: Span tracking has been removed for simplicity. This executor
-- now only handles basic event publishing.
--
-- = Usage
--
-- @
-- import Tidepool.Effects.Observability
-- import Tidepool.Observability.Executor
--
-- main :: IO ()
-- main = do
--   let config = defaultLokiConfig
--   runM $ runObservability config $ do
--     publishEvent $ GraphTransition "entry" "classify" "user_input"
--     publishEvent $ LLMCallEvent "claude-3" 100 50 250
-- @
module Tidepool.Observability.Executor
  ( -- * Executor
    runObservability

    -- * Configuration
  , LokiConfig(..)
  , defaultLokiConfig
  , grafanaCloudConfig

    -- * Low-level API
  , pushToLoki
  ) where

import Control.Exception (try, SomeException)
import Control.Monad (void)
import Control.Monad.Freer (Eff, LastMember, interpret, sendM)
import Data.Aeson (encode)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Req
import qualified Data.ByteString.Base64 as B64

import Tidepool.Effects.Observability (Observability(..), TidepoolEvent)
import Tidepool.Observability.Types


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
    Left e  -> Just $ "Loki push failed: " <> T.pack (show e)
    Right _ -> Nothing

-- | Parse URL into http or https variant.
parseUrl :: Text -> Either (Url 'Http, Option 'Http) (Url 'Https, Option 'Https)
parseUrl urlText
  | "https://" `T.isPrefixOf` urlText =
      let host = T.drop 8 urlText  -- Remove "https://"
          (hostPart, pathPart) = T.break (== '/') host
      in Right (https hostPart, mempty)
  | "http://" `T.isPrefixOf` urlText =
      let host = T.drop 7 urlText  -- Remove "http://"
          (hostPart, pathPart) = T.break (== '/') host
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
-- EFFECT INTERPRETER
-- ════════════════════════════════════════════════════════════════════════════

-- | Run Observability effects by pushing to Loki.
--
-- This interpreter:
-- 1. Publishes events to Loki with appropriate labels
-- 2. Handles errors gracefully (logs but doesn't throw)
--
-- The interpreter is polymorphic over remaining effects, enabling composition
-- with other effect interpreters (UI, Habitica, LLM, etc.).
runObservability :: LastMember IO effs => LokiConfig -> Eff (Observability ': effs) a -> Eff effs a
runObservability config = interpret $ \case
  PublishEvent event -> sendM $ do
    let labels = eventToLabels config.lcJobLabel event
        line = eventToLine event

    -- Build and send request
    ts <- nowNanos
    let stream = LokiStream labels [(ts, line)]
        req = LokiPushRequest [stream]

    mErr <- pushToLoki config req
    case mErr of
      Just err -> putStrLn $ "[Observability] " <> T.unpack err
      Nothing  -> pure ()
