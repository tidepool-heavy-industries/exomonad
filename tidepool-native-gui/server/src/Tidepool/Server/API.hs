-- | Server API definition - Servant types for HTTP endpoints.
--
-- Defines REST endpoints for health checks and session management.
-- WebSocket is handled at the WAI level via wai-websockets.
module Tidepool.Server.API
  ( -- * API Types
    TidepoolAPI
  , HealthAPI
  , SessionAPI
  , api

    -- * Response Types
  , HealthStatus(..)
  ) where

import Data.Aeson (ToJSON(..), object, (.=))
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Servant

import Tidepool.Server.Session (SessionInfo)


-- ════════════════════════════════════════════════════════════════════════════
-- RESPONSE TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Health check response.
data HealthStatus = HealthStatus
  { hsStatus  :: Text
  , hsVersion :: Text
  }
  deriving (Generic, Show, Eq)

instance ToJSON HealthStatus where
  toJSON hs = object
    [ "status" .= hsStatus hs
    , "version" .= hsVersion hs
    ]


-- ════════════════════════════════════════════════════════════════════════════
-- API TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Health check endpoint.
--
-- GET /health -> HealthStatus
type HealthAPI = "health" :> Get '[JSON] HealthStatus

-- | Session management endpoints.
--
-- GET /sessions -> [SessionInfo]
-- GET /sessions/:id -> Maybe SessionInfo
type SessionAPI =
       "sessions" :> Get '[JSON] [SessionInfo]
  :<|> "sessions" :> Capture "id" UUID :> Get '[JSON] (Maybe SessionInfo)

-- | Combined API (REST endpoints only).
--
-- Static files are served via Raw combinator in the server.
-- WebSocket is handled at WAI level via websocketsOr.
type TidepoolAPI = HealthAPI :<|> SessionAPI

api :: Proxy TidepoolAPI
api = Proxy
