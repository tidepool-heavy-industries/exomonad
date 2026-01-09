-- | Output formatting for sleeptime-logs CLI.
--
-- Provides both pretty-printed and JSON output formats.
module SleeptimeLogs.Output
  ( formatResult
  , formatResultJson
  ) where

import Data.Aeson (encode, object, (.=))
import Data.Aeson.Types (Value(..))
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import SleeptimeLogs.Loki (LogEntry(..), QueryResult(..))
import Tidepool.Effects.Observability (TidepoolEvent(..))

-- | Format result as JSON (for piping to jq).
formatResultJson :: QueryResult -> IO ()
formatResultJson result = do
  let entries = map entryToJson result.resultEntries
  BL.putStrLn $ encode entries

entryToJson :: LogEntry -> Value
entryToJson entry = object
  [ "timestamp" .= entry.entryTimestamp
  , "event" .= entry.entryEvent
  ]

-- | Format result as human-readable text.
formatResult :: QueryResult -> IO ()
formatResult result = do
  if null result.resultEntries
    then TIO.putStrLn "No results found."
    else mapM_ formatEntry result.resultEntries

formatEntry :: LogEntry -> IO ()
formatEntry entry = do
  TIO.putStrLn $ formatTimestamp entry.entryTimestamp <> " " <> formatEvent entry.entryEvent
  TIO.putStrLn ""

formatTimestamp :: Text -> Text
formatTimestamp ts =
  -- Loki timestamps are nanoseconds, truncate for display
  "[" <> T.take 19 ts <> "]"

formatEvent :: TidepoolEvent -> Text
formatEvent = \case
  GraphTransition from to trigger ->
    "TRANSITION: " <> from <> " → " <> to <> " (trigger: " <> trigger <> ")"

  LLMCallEvent model promptToks completionToks latencyMs ->
    "LLM CALL: " <> model <>
    "\n  Tokens: " <> T.pack (show promptToks) <> " prompt, " <>
    T.pack (show completionToks) <> " completion" <>
    "\n  Latency: " <> T.pack (show latencyMs) <> "ms"

  UserActionEvent actionType nodeContext ->
    "USER ACTION: " <> actionType <>
    "\n  Context: " <> nodeContext

  EffectExecutionEvent effectType success latencyMs ->
    "EFFECT: " <> effectType <>
    " (" <> (if success then "success" else "failed") <> ")" <>
    "\n  Latency: " <> T.pack (show latencyMs) <> "ms"

  ErrorEvent message _context ->
    "ERROR: " <> message
