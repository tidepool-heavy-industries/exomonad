{-# LANGUAGE DataKinds #-}
module Tidepool.Control.Observability
  ( withTracerProvider
  , getTracer
  , TracingConfig(..)
  ) where

import OpenTelemetry.Trace hiding (inSpan, getTracer)
import qualified OpenTelemetry.Trace as Trace
import OpenTelemetry.Exporter.OTLP
import OpenTelemetry.Resource
import OpenTelemetry.Processor.Batch (batchProcessor, batchTimeoutConfig)
import OpenTelemetry.Attributes (emptyAttributes)
import Data.ByteString.Base64 (encode)
import qualified Data.Text.Encoding as TE
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import System.Environment (setEnv)

data TracingConfig = TracingConfig
  { tcEndpoint :: String        -- "localhost:5081" or "openobserve:5081"
  , tcAuthEmail :: Text
  , tcAuthPassword :: Text
  , tcServiceName :: Text       -- "tidepool-control-server"
  }

-- | Initialize tracing with OTLP export to OpenObserve
withTracerProvider :: TracingConfig -> (TracerProvider -> IO a) -> IO a
withTracerProvider cfg action = do
  let authHeader = makeAuthHeader cfg.tcAuthEmail cfg.tcAuthPassword
  
  -- Set environment variables for OTLP exporter
  setEnv "OTEL_EXPORTER_OTLP_ENDPOINT" ("http://" <> cfg.tcEndpoint)
  setEnv "OTEL_EXPORTER_OTLP_HEADERS" (BS8.unpack authHeader)
  setEnv "OTEL_EXPORTER_OTLP_INSECURE" "true"
  setEnv "OTEL_SERVICE_NAME" (T.unpack cfg.tcServiceName)
  
  exporterConfig <- loadExporterEnvironmentVariables
  exporter <- otlpExporter exporterConfig

  processor <- batchProcessor batchTimeoutConfig exporter
  
  let res :: Resource 'Nothing
      res = mkResource [ "service.name" .= cfg.tcServiceName ]
      materializedRes = materializeResources res

  provider <- createTracerProvider [processor] $
    emptyTracerProviderOptions
      { tracerProviderOptionsResources = materializedRes
      }

  result <- action provider
  shutdownTracerProvider provider
  pure result

makeAuthHeader :: Text -> Text -> ByteString
makeAuthHeader email password =
  "Basic " <> encode (TE.encodeUtf8 $ email <> ":" <> password)

getTracer :: TracerProvider -> Text -> Tracer
getTracer provider name = makeTracer provider lib tracerOptions
  where
    lib = InstrumentationLibrary name "" "" emptyAttributes
