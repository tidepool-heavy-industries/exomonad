-- | Session management for WebSocket connections.
--
-- Uses STM for thread-safe in-memory session storage.
--
-- TODO: Persist sessions to SQLite for durability across server restarts.
module ExoMonad.Server.Session
  ( -- * Types
    SessionId
  , Session(..)
  , SessionMap
  , SessionInfo(..)

    -- * Session operations
  , newSessionMap
  , createSession
  , getSession
  , deleteSession
  , listSessions
  ) where

import Control.Concurrent.STM
import Data.Aeson (ToJSON(..), object, (.=))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import GHC.Generics (Generic)

import ExoMonad.Wire.Types (UIState, UserAction)


-- ════════════════════════════════════════════════════════════════════════════
-- TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Unique session identifier.
type SessionId = UUID

-- | Active session with communication channels.
--
-- Each WebSocket connection gets its own session with:
-- - Bidirectional communication via TVars
-- - Creation timestamp for cleanup/TTL
--
-- TODO: Add session state persistence to SQLite
data Session = Session
  { sId        :: SessionId
    -- ^ Unique session identifier
  , sActionVar :: TVar (Maybe UserAction)
    -- ^ Client -> Server: incoming user actions
  , sStateVar  :: TVar (Maybe UIState)
    -- ^ Server -> Client: outgoing UI states
  , sCreatedAt :: UTCTime
    -- ^ When the session was created
  , sGraphNode :: TVar Text
    -- ^ Current graph node (for session info)
  }

-- | Session info for REST API responses.
data SessionInfo = SessionInfo
  { siId        :: SessionId
  , siCreatedAt :: UTCTime
  , siGraphNode :: Text
  }
  deriving (Generic, Show, Eq)

instance ToJSON SessionInfo where
  toJSON si = object
    [ "id" .= UUID.toText (siId si)
    , "createdAt" .= siCreatedAt si
    , "graphNode" .= siGraphNode si
    ]

-- | Thread-safe map of active sessions.
type SessionMap = TVar (Map SessionId Session)


-- ════════════════════════════════════════════════════════════════════════════
-- SESSION OPERATIONS
-- ════════════════════════════════════════════════════════════════════════════

-- | Create a new empty session map.
newSessionMap :: IO SessionMap
newSessionMap = newTVarIO Map.empty

-- | Create a new session and add it to the map.
createSession :: SessionMap -> IO Session
createSession sessionsVar = do
  sessionId <- nextRandom
  now <- getCurrentTime
  actionVar <- newTVarIO Nothing
  stateVar <- newTVarIO Nothing
  graphNodeVar <- newTVarIO "entry"

  let session = Session
        { sId = sessionId
        , sActionVar = actionVar
        , sStateVar = stateVar
        , sCreatedAt = now
        , sGraphNode = graphNodeVar
        }

  atomically $ modifyTVar' sessionsVar (Map.insert sessionId session)
  pure session

-- | Get a session by ID.
getSession :: SessionMap -> SessionId -> IO (Maybe Session)
getSession sessionsVar sessionId =
  Map.lookup sessionId <$> readTVarIO sessionsVar

-- | Delete a session from the map.
deleteSession :: SessionMap -> SessionId -> IO ()
deleteSession sessionsVar sessionId =
  atomically $ modifyTVar' sessionsVar (Map.delete sessionId)

-- | List all sessions as SessionInfo.
listSessions :: SessionMap -> IO [SessionInfo]
listSessions sessionsVar = do
  sessions <- Map.elems <$> readTVarIO sessionsVar
  mapM sessionToInfo sessions
  where
    sessionToInfo :: Session -> IO SessionInfo
    sessionToInfo s = do
      node <- readTVarIO (sGraphNode s)
      pure SessionInfo
        { siId = sId s
        , siCreatedAt = sCreatedAt s
        , siGraphNode = node
        }
