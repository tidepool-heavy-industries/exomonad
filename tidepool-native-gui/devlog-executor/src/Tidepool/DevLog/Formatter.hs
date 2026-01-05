-- | Log line formatting for DevLog.
--
-- Produces greppable output like:
--
-- @
-- [12:00:00.050] GRAPH entry → resumeRouter
--   input: PlayerInput { piActionText = "attack" }
--
-- [12:00:00.051] STATE.mood: MoodScene Encounter (unchanged)
--
-- [12:00:00.100] LLM.REQUEST dm_scene
--   model: claude-sonnet-4-20250514
--   tokens: 1,847 prompt
--   tools: [engage, end_scene]
-- @
module Tidepool.DevLog.Formatter
  ( formatEvent
  , formatSessionHeader
  , formatTimestamp
  , compactJSON
  ) where

import Data.Aeson (Value, encode)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time (UTCTime, formatTime, defaultTimeLocale)
import Data.UUID (UUID)
import qualified Data.UUID as UUID

import Tidepool.Effect.DevLog

-- | Format a DevLog event as a log line.
--
-- The current time is passed in so the executor can add timestamps.
formatEvent :: UTCTime -> DevLogEvent -> Text
formatEvent now = \case
  EventGraph _ info      -> formatGraphTransition now info
  EventState _ info      -> formatStateSnapshot now info
  EventLLMRequest _ info -> formatLLMRequest now info
  EventLLMResponse _ info -> formatLLMResponse now info
  EventError info        -> formatError now info
  EventRaw _ msg         -> "[" <> formatTimestamp now <> "] " <> msg

-- | Format a graph transition.
formatGraphTransition :: UTCTime -> GraphTransitionInfo -> Text
formatGraphTransition now info = T.unlines
  [ "[" <> formatTimestamp now <> "] GRAPH " <> info.gtiFromNode <> " → " <> info.gtiToNode
  , "  input: " <> compactJSON info.gtiPayload
  ]

-- | Format a state snapshot/diff.
formatStateSnapshot :: UTCTime -> StateSnapshotInfo -> Text
formatStateSnapshot now info =
  "[" <> formatTimestamp now <> "] STATE." <> info.ssiField <> ": "
  <> compactJSON info.ssiBefore
  <> if info.ssiChanged
       then " → " <> compactJSON info.ssiAfter
       else " (unchanged)"

-- | Format an LLM request.
formatLLMRequest :: UTCTime -> LLMRequestInfo -> Text
formatLLMRequest now info = T.unlines $ catMaybes
  [ Just $ "[" <> formatTimestamp now <> "] LLM.REQUEST " <> info.lriNodeName
  , Just $ "  model: " <> info.lriModel
  , Just $ "  tokens: " <> T.pack (show info.lriPromptTokens) <> " prompt"
  , Just $ "  tools: [" <> T.intercalate ", " info.lriTools <> "]"
  , info.lriSystemPrompt >>= \p -> Just $ formatPromptSection "SYSTEM" p
  , info.lriUserPrompt >>= \p -> Just $ formatPromptSection "USER" p
  ]

-- | Format an LLM response.
formatLLMResponse :: UTCTime -> LLMResponseInfo -> Text
formatLLMResponse now info = T.unlines $ catMaybes
  [ Just $ "[" <> formatTimestamp now <> "] LLM.RESPONSE " <> info.lroNodeName
         <> " (" <> T.pack (show info.lroLatencyMs) <> "ms)"
  , Just $ "  tokens: " <> T.pack (show info.lroCompletionTokens) <> " completion"
  , Just $ "  tool_calls: " <> T.pack (show info.lroToolCalls)
  , info.lroResponse >>= \r -> Just $ formatPromptSection "RESPONSE" r
  ]

-- | Format an error with context.
formatError :: UTCTime -> ErrorContextInfo -> Text
formatError _now info = T.unlines $ catMaybes
  [ Just "═══ ERROR DUMP ═══════════════════════════════════════════════════"
  , Just $ "Exception: " <> info.eciMessage
  , info.eciNode >>= \n -> Just $ "Node: " <> n
  , Just ""
  , Just "── Last transitions ──"
  , Just $ T.unlines $ map (\(f, t) -> "  " <> f <> " → " <> t) info.eciTransitions
  , Just ""
  , Just "── Current state ──"
  , Just $ compactJSON info.eciState
  , info.eciStackTrace >>= \st -> Just $ "\n── Stack trace ──\n" <> st
  , Just "═══════════════════════════════════════════════════════════════════"
  ]

-- | Format a prompt section with delimiters.
formatPromptSection :: Text -> Text -> Text
formatPromptSection label content = T.unlines
  [ "  ───" <> label <> "───"
  , T.unlines $ map ("  " <>) $ T.lines content
  , "  ───/" <> label <> "───"
  ]

-- | Format session header.
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
