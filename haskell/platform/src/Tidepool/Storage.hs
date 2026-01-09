-- | SQLite persistence for games and chat history
module Tidepool.Storage
  ( -- * Database Management
    Connection
  , initDB
  , withGameDB

    -- * Game Operations
  , GameId(..)
  , gameIdText
  , GameRow(..)
  , createGame
  , loadGame
  , saveGameState
  , listGames
  , deleteGame

    -- * Message Operations
  , appendMessages
  , loadMessages
  , loadMessagesAfter
  , getMessageCount

    -- * Compression
  , updateCompression
  ) where

import Tidepool.Anthropic.Types (Message(..), Role(..))

import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField

import Data.Aeson (ToJSON, FromJSON, encode, decode)
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString.Lazy (ByteString)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID4
import Data.Maybe (fromMaybe)
import Control.Exception (bracket)

-- ══════════════════════════════════════════════════════════════
-- TYPES
-- ══════════════════════════════════════════════════════════════

newtype GameId = GameId { unGameId :: Text }
  deriving (Show, Eq, Ord)
  deriving newtype (ToField, FromField)

-- | Get the text representation of a GameId
gameIdText :: GameId -> Text
gameIdText (GameId t) = t

data GameRow = GameRow
  { gameId :: GameId
  , gameName :: Maybe Text
  , gameWorldState :: ByteString    -- JSON-encoded WorldState
  , gameCompressionCursor :: Maybe Int
  , gameCompressionSummary :: Maybe Text
  , gameCreatedAt :: Text           -- ISO8601
  , gameUpdatedAt :: Text           -- ISO8601
  }
  deriving (Show, Eq)

instance FromRow GameRow where
  fromRow = GameRow
    <$> field  -- id
    <*> field  -- name
    <*> field  -- world_state
    <*> field  -- compression_cursor
    <*> field  -- compression_summary
    <*> field  -- created_at
    <*> field  -- updated_at

-- ══════════════════════════════════════════════════════════════
-- DATABASE MANAGEMENT
-- ══════════════════════════════════════════════════════════════

-- | Initialize database schema
initDB :: Connection -> IO ()
initDB conn = do
  -- Enable foreign keys
  execute_ conn "PRAGMA foreign_keys = ON"

  -- Create games table
  execute_ conn
    "CREATE TABLE IF NOT EXISTS games (\
    \  id TEXT PRIMARY KEY,\
    \  name TEXT,\
    \  world_state BLOB NOT NULL,\
    \  compression_cursor INTEGER,\
    \  compression_summary TEXT,\
    \  created_at TEXT NOT NULL,\
    \  updated_at TEXT NOT NULL\
    \)"

  -- Create messages table
  execute_ conn
    "CREATE TABLE IF NOT EXISTS messages (\
    \  id INTEGER PRIMARY KEY AUTOINCREMENT,\
    \  game_id TEXT NOT NULL REFERENCES games(id) ON DELETE CASCADE,\
    \  sequence INTEGER NOT NULL,\
    \  role TEXT NOT NULL,\
    \  content BLOB NOT NULL,\
    \  created_at TEXT NOT NULL,\
    \  UNIQUE(game_id, sequence)\
    \)"

  -- Create index
  execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_messages_game ON messages(game_id, sequence)"

-- | Open database with initialization
withGameDB :: FilePath -> (Connection -> IO a) -> IO a
withGameDB path action = bracket (open path) close $ \conn -> do
  initDB conn
  action conn

-- ══════════════════════════════════════════════════════════════
-- GAME OPERATIONS
-- ══════════════════════════════════════════════════════════════

-- | Create a new game with initial world state
createGame :: ToJSON state => Connection -> Maybe Text -> state -> IO GameId
createGame conn mName initialState = do
  uuid <- UUID4.nextRandom
  let gameId = GameId (UUID.toText uuid)
  now <- iso8601Now
  let stateJson = encode initialState

  execute conn
    "INSERT INTO games (id, name, world_state, created_at, updated_at) \
    \VALUES (?, ?, ?, ?, ?)"
    (gameId, mName, stateJson, now, now)

  return gameId

-- | Load a game by ID
loadGame :: FromJSON state => Connection -> GameId -> IO (Maybe (state, Maybe Int, Maybe Text))
loadGame conn gid = do
  rows <- query conn
    "SELECT id, name, world_state, compression_cursor, compression_summary, created_at, updated_at \
    \FROM games WHERE id = ?"
    (Only gid) :: IO [GameRow]
  case rows of
    [] -> return Nothing
    (row:_) -> case decode row.gameWorldState of
      Nothing -> return Nothing
      Just state -> return $ Just (state, row.gameCompressionCursor, row.gameCompressionSummary)

-- | Save world state for a game
saveGameState :: ToJSON state => Connection -> GameId -> state -> IO ()
saveGameState conn gid state = do
  now <- iso8601Now
  let stateJson = encode state
  execute conn
    "UPDATE games SET world_state = ?, updated_at = ? WHERE id = ?"
    (stateJson, now, gid)

-- | List all games (most recent first)
listGames :: Connection -> IO [(GameId, Maybe Text, Text)]
listGames conn = do
  rows <- query_ conn
    "SELECT id, name, updated_at FROM games ORDER BY updated_at DESC"
  return [(gid, name, updated) | (gid, name, updated) <- rows]

-- | Delete a game and all its messages
deleteGame :: Connection -> GameId -> IO ()
deleteGame conn gid =
  execute conn "DELETE FROM games WHERE id = ?" (Only gid)

-- ══════════════════════════════════════════════════════════════
-- MESSAGE OPERATIONS
-- ══════════════════════════════════════════════════════════════

-- | Append messages to a game's chat history
-- Uses a transaction to ensure all-or-nothing semantics
appendMessages :: Connection -> GameId -> [Message] -> IO ()
appendMessages _ _ [] = return ()  -- Early return for empty list
appendMessages conn gid messages = withTransaction conn $ do
  -- COALESCE guarantees exactly one row is returned
  [Only maxSeq] <- query conn
    "SELECT COALESCE(MAX(sequence), 0) FROM messages WHERE game_id = ?"
    (Only gid) :: IO [Only Int]

  now <- iso8601Now

  -- Insert each message with incrementing sequence
  let insertMsg seqNum msg = execute conn
        "INSERT INTO messages (game_id, sequence, role, content, created_at) \
        \VALUES (?, ?, ?, ?, ?)"
        (gid, seqNum, roleToText msg.role, encode msg.content, now)

  mapM_ (uncurry insertMsg) (zip [(maxSeq + 1)..] messages)

-- | Load all messages for a game
loadMessages :: Connection -> GameId -> IO [Message]
loadMessages conn gid = do
  rows <- query conn
    "SELECT role, content FROM messages WHERE game_id = ? ORDER BY sequence"
    (Only gid) :: IO [(Text, ByteString)]
  return [Message (textToRole r) (fromMaybe [] $ decode c) | (r, c) <- rows]

-- | Load messages after a given sequence number
loadMessagesAfter :: Connection -> GameId -> Int -> IO [Message]
loadMessagesAfter conn gid cursor = do
  rows <- query conn
    "SELECT role, content FROM messages WHERE game_id = ? AND sequence > ? ORDER BY sequence"
    (gid, cursor) :: IO [(Text, ByteString)]
  return [Message (textToRole r) (fromMaybe [] $ decode c) | (r, c) <- rows]

-- | Get message count for a game
getMessageCount :: Connection -> GameId -> IO Int
getMessageCount conn gid = do
  [Only count] <- query conn
    "SELECT COUNT(*) FROM messages WHERE game_id = ?"
    (Only gid)
  return count

-- ══════════════════════════════════════════════════════════════
-- COMPRESSION
-- ══════════════════════════════════════════════════════════════

-- | Update compression cursor and summary
updateCompression :: Connection -> GameId -> Int -> Text -> IO ()
updateCompression conn gid cursor summary = do
  now <- iso8601Now
  execute conn
    "UPDATE games SET compression_cursor = ?, compression_summary = ?, updated_at = ? \
    \WHERE id = ?"
    (cursor, summary, now, gid)

-- ══════════════════════════════════════════════════════════════
-- HELPERS
-- ══════════════════════════════════════════════════════════════

iso8601Now :: IO Text
iso8601Now = T.pack . iso8601Show <$> getCurrentTime

roleToText :: Role -> Text
roleToText User = "user"
roleToText Assistant = "assistant"

textToRole :: Text -> Role
textToRole "user" = User
textToRole "assistant" = Assistant
textToRole t = error ("Unknown role text: " ++ T.unpack t)
