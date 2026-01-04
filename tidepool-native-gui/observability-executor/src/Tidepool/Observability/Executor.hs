-- | Observability effect executor - Loki push API client.
--
-- Publishes structured JSON logs to Grafana Loki.
module Tidepool.Observability.Executor
  ( -- * Executor
    runObservability
  , LokiConfig(..)
  ) where

import Data.Text (Text)

-- | Loki configuration.
data LokiConfig = LokiConfig
  { lcUrl :: Text
  , lcUser :: Maybe Text
  , lcToken :: Maybe Text
  }

-- | Run Observability effects (stub - actual implementation by Agent 6).
runObservability :: LokiConfig -> a -> a
runObservability _ = id  -- stub
