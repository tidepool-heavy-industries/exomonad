-- | Server API definition - Servant types for HTTP endpoints.
--
-- Defines REST endpoints for health checks, session management, and graph introspection.
-- WebSocket is handled at the WAI level via wai-websockets.
module ExoMonad.Server.API
  ( -- * API Types
    ExoMonadAPI
  , HealthAPI
  , SessionAPI
  , GraphAPI
  , api

    -- * Response Types
  , HealthStatus(..)
  ) where

import Data.Aeson (ToJSON(..), object, (.=))
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Servant

import ExoMonad.Server.Session (SessionInfo)
import ExoMonad.Graph.Export (GraphExport)


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

-- | Graph introspection endpoint.
--
-- GET /graph/info -> GraphExport
--
-- Returns the graph structure for visualization (D3, Mermaid, etc.)
-- Only available when server is configured with a graph.
type GraphAPI = "graph" :> "info" :> Get '[JSON] GraphExport

-- | Combined API (REST endpoints only).
--
-- Static files are served via Raw combinator in the server.
-- WebSocket is handled at WAI level via websocketsOr.
type ExoMonadAPI = HealthAPI :<|> SessionAPI :<|> GraphAPI

api :: Proxy ExoMonadAPI
api = Proxy
