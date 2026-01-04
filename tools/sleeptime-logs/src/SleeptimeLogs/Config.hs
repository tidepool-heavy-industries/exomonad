-- | Configuration loading for sleeptime-logs CLI.
--
-- Reads Loki connection details from environment variables,
-- falling back to .tidepool.env if present.
module SleeptimeLogs.Config
  ( Config(..)
  , loadConfig
  , defaultConfig
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)

-- | Configuration for Loki connection.
data Config = Config
  { lokiUrl :: Text
    -- ^ Grafana Loki URL (e.g., "https://logs-prod-us-central1.grafana.net")
  , lokiUser :: Maybe Text
    -- ^ Grafana Cloud user ID (optional for local Loki)
  , lokiToken :: Maybe Text
    -- ^ Grafana Cloud API token (optional for local Loki)
  , defaultLimit :: Int
    -- ^ Default result limit
  }
  deriving (Show)

-- | Default configuration for local Loki.
defaultConfig :: Config
defaultConfig = Config
  { lokiUrl = "http://localhost:3100"
  , lokiUser = Nothing
  , lokiToken = Nothing
  , defaultLimit = 100
  }

-- | Load configuration from environment.
--
-- Environment variables:
--   GRAFANA_LOKI_URL   - Loki URL (default: http://localhost:3100)
--   GRAFANA_LOKI_USER  - Grafana Cloud user ID (optional)
--   GRAFANA_LOKI_TOKEN - Grafana Cloud API token (optional)
loadConfig :: IO Config
loadConfig = do
  url <- maybe (lokiUrl defaultConfig) T.pack <$> lookupEnv "GRAFANA_LOKI_URL"
  user <- fmap T.pack <$> lookupEnv "GRAFANA_LOKI_USER"
  token <- fmap T.pack <$> lookupEnv "GRAFANA_LOKI_TOKEN"

  pure Config
    { lokiUrl = url
    , lokiUser = user
    , lokiToken = token
    , defaultLimit = 100
    }
