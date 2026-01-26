-- | Log formatting for human-readable output.
--
-- Produces greppable output like:
--
-- @
-- [12:00:00.050] GRAPH entry → resumeRouter
--   input: {"action": "attack"}
--
-- [12:00:00.051] STATE.mood: {"scene": "encounter"} (unchanged)
--
-- [12:00:00.100] LLM.REQUEST dm_scene
--   model: claude-sonnet-4-20250514
--   tokens: 1847 prompt
--   tools: [engage, end_scene]
-- @
module Tidepool.Log.Formatter
  ( -- * Entry Formatting
    formatLogEntry
  , formatLogEntryHuman
  , formatLogEntryJSON
    -- * Session Header
  , formatSessionHeader
    -- * Utilities
  , formatTimestamp
  , compactJSON
  ) where

import Data.Aeson (Value, encode)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time (UTCTime, formatTime, defaultTimeLocale)
import Data.UUID (UUID)
import qualified Data.UUID as UUID

import Tidepool.Log.Types (LogEntry(..))
import Tidepool.Effect.Log (LogLevel(..))

-- | Format a log entry based on format preference.
formatLogEntry :: Bool -> LogEntry -> Text
formatLogEntry humanReadable
  | humanReadable = formatLogEntryHuman
  | otherwise = formatLogEntryJSON

-- | Format a log entry as human-readable greppable text.
formatLogEntryHuman :: LogEntry -> Text
formatLogEntryHuman entry =
  "[" <> formatTimestamp entry.ts <> "] "
  <> levelPrefix entry.level
  <> entry.msg
  <> fieldsStr
  where
    levelPrefix = \case
      Trace -> "TRACE "
      Debug -> "DEBUG "
      Info  -> ""  -- Info is the default, no prefix needed
      Warn  -> "WARN "
      Error -> "ERROR "

    fieldsStr = case entry.fields of
      Nothing -> ""
      Just [] -> ""
      Just fs -> "\n" <> T.unlines (map formatField fs)

    formatField (k, v) = "  " <> k <> ": " <> compactJSON v

-- | Format a log entry as JSON (one line).
formatLogEntryJSON :: LogEntry -> Text
formatLogEntryJSON entry = TL.toStrict $ TLE.decodeUtf8 $ encode entry

-- | Format session header for log files.
formatSessionHeader :: UUID -> Maybe Text -> UTCTime -> Text
formatSessionHeader sessionId mName now = T.unlines
  [ "═══ SESSION " <> shortUUID sessionId <> nameLabel <> " ════════════════════════════════════════════"
  , "[" <> formatTimestamp now <> "] SESSION START"
  , ""
  ]
  where
    shortUUID = T.take 8 . UUID.toText
    nameLabel = maybe "" (\n -> " (" <> n <> ")") mName

-- | Format timestamp as HH:MM:SS.mmm.
formatTimestamp :: UTCTime -> Text
formatTimestamp = T.pack . formatTime defaultTimeLocale "%H:%M:%S.%03q"

-- | Compact JSON (single line, truncated to 200 chars).
compactJSON :: Value -> Text
compactJSON = T.take 200 . T.replace "\n" " " . TL.toStrict . TLE.decodeUtf8 . encode
