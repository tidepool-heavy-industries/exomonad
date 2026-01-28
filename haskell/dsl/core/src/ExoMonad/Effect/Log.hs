{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeOperators #-}

-- | Unified logging effect for ExoMonad.
--
-- This module provides a single 'Log' effect that all logging dispatches to.
-- Domain-specific helpers (graph transitions, LLM calls, state changes) emit
-- structured fields that interpreters can format as JSON or human-readable.
--
-- == Greppable Patterns
--
-- @
-- grep "GRAPH" latest.log        # Transitions
-- grep "STATE\\." latest.log     # State changes
-- grep "LLM\\." latest.log       # LLM calls
-- grep "ERROR" latest.log        # Errors
-- @
module ExoMonad.Effect.Log
  ( -- * Log Levels
    LogLevel(..)
    -- * Log Context
  , LogContext(..)
  , emptyLogContext
  , LogFields
    -- * Log Effect
  , Log(..)
  , logMsg
  , logMsgWith
  , logTrace
  , logDebug
  , logInfo
  , logWarn
  , logError
  , logTraceWith
  , logDebugWith
  , logInfoWith
  , logWarnWith
  , logErrorWith
    -- * Domain Event Types
  , GraphTransitionInfo(..)
  , StateSnapshotInfo(..)
  , LLMRequestInfo(..)
  , LLMResponseInfo(..)
  , ErrorContextInfo(..)
    -- * Domain Event Helpers
  , logGraph
  , logState
  , logLLMRequest
  , logLLMResponse
  , logErrorContext
    -- * Scoped Logging
  , withEffectSpan
    -- * Runners (simple, use log-interpreter for fast-logger)
  , runLog
  ) where

import Control.Monad.Freer (Eff, Member, send, interpret, LastMember, sendM)
import Control.Monad.Freer.Reader (Reader, local)
import Data.Aeson (Value, ToJSON(..), toJSON)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import System.IO (stderr)

-- | Log severity levels
data LogLevel = Trace | Debug | Info | Warn | Error
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON)

-- | Structured log fields
type LogFields = [(Text, Value)]

-- | Context for distributed tracing and effect chains
data LogContext = LogContext
  { correlationId :: Text
  , effectChain :: [Text]
  , startTime :: Maybe UTCTime -- Maybe because purely functional context might not have time
  }
  deriving (Show, Eq, Generic, ToJSON)

emptyLogContext :: LogContext
emptyLogContext = LogContext "" [] Nothing

-- | The Log effect
data Log r where
  LogMsg :: LogLevel -> Text -> Maybe LogFields -> Log ()

logMsg :: Member Log effs => LogLevel -> Text -> Eff effs ()
logMsg level msg = send (LogMsg level msg Nothing)

logMsgWith :: Member Log effs => LogLevel -> Text -> LogFields -> Eff effs ()
logMsgWith level msg fields = send (LogMsg level msg (Just fields))

logTrace :: Member Log effs => Text -> Eff effs ()
logTrace = logMsg Trace

logDebug :: Member Log effs => Text -> Eff effs ()
logDebug = logMsg Debug

logInfo :: Member Log effs => Text -> Eff effs ()
logInfo = logMsg Info

logWarn :: Member Log effs => Text -> Eff effs ()
logWarn = logMsg Warn

logError :: Member Log effs => Text -> Eff effs ()
logError = logMsg Error

logTraceWith :: Member Log effs => Text -> LogFields -> Eff effs ()
logTraceWith = logMsgWith Trace

logDebugWith :: Member Log effs => Text -> LogFields -> Eff effs ()
logDebugWith = logMsgWith Debug

logInfoWith :: Member Log effs => Text -> LogFields -> Eff effs ()
logInfoWith = logMsgWith Info

logWarnWith :: Member Log effs => Text -> LogFields -> Eff effs ()
logWarnWith = logMsgWith Warn

logErrorWith :: Member Log effs => Text -> LogFields -> Eff effs ()
logErrorWith = logMsgWith Error

-- | Execute an action within a named span.
-- This pushes the name to the effect chain in the LogContext.
-- It requires a Reader LogContext effect to be present.
withEffectSpan :: (Member (Reader LogContext) effs)
               => Text
               -> Eff effs a
               -> Eff effs a
withEffectSpan name action =
  local (\ctx -> ctx { effectChain = ctx.effectChain ++ [name] }) action

-- | Simple runner for Log effect that outputs to stderr.
-- For production use, prefer 'ExoMonad.Log.Interpreter' which uses fast-logger.
runLog :: LastMember IO effs => LogLevel -> Eff (Log ': effs) a -> Eff effs a
runLog minLevel = interpret $ \case
  LogMsg level msg maybeFields
    | level >= minLevel -> do
        let fieldStr = case maybeFields of
              Nothing -> ""
              Just fs -> " | " <> T.intercalate ", " (map fst fs)
        sendM $ TIO.hPutStrLn stderr ("[" <> T.pack (show level) <> "] " <> msg <> fieldStr)
    | otherwise -> pure ()

-- ══════════════════════════════════════════════════════════════
-- DOMAIN EVENT TYPES
-- ══════════════════════════════════════════════════════════════

-- | Graph transition info.
--
-- Logged automatically by instrumented dispatch.
data GraphTransitionInfo = GraphTransitionInfo
  { fromNode :: Text
  , toNode   :: Text
  , payload  :: Value    -- ^ Input to next handler (JSON-encoded)
  }
  deriving (Show, Generic)

instance ToJSON GraphTransitionInfo

-- | State snapshot with diff info.
--
-- Logged after each transition for changed fields.
data StateSnapshotInfo = StateSnapshotInfo
  { field   :: Text   -- ^ e.g., "mood", "dicePool"
  , before  :: Value
  , after   :: Value
  , changed :: Bool   -- ^ Quick check for "(unchanged)"
  }
  deriving (Show, Generic)

instance ToJSON StateSnapshotInfo

-- | LLM request info.
--
-- At Info level: metadata only (model, tokens, tools).
-- At Debug level: includes full prompts.
data LLMRequestInfo = LLMRequestInfo
  { nodeName     :: Text
  , model        :: Text
  , promptTokens :: Int
  , tools        :: [Text]  -- ^ Tool names available
  -- Full content (only at Debug verbosity)
  , systemPrompt :: Maybe Text
  , userPrompt   :: Maybe Text
  }
  deriving (Show, Generic)

instance ToJSON LLMRequestInfo

-- | LLM response info.
data LLMResponseInfo = LLMResponseInfo
  { nodeName         :: Text
  , completionTokens :: Int
  , toolCalls        :: Int
  , latencyMs        :: Int
  -- Full content (only at Debug verbosity)
  , response         :: Maybe Text
  }
  deriving (Show, Generic)

instance ToJSON LLMResponseInfo

-- | Error context for debugging.
--
-- Includes state snapshot and last LLM call for context.
data ErrorContextInfo = ErrorContextInfo
  { message     :: Text
  , node        :: Maybe Text
  , state       :: Value        -- ^ Current state snapshot
  , transitions :: [(Text, Text)]  -- ^ Last N transitions (from, to)
  , lastLLM     :: Maybe LLMResponseInfo
  , stackTrace  :: Maybe Text
  }
  deriving (Show, Generic)

instance ToJSON ErrorContextInfo

-- ══════════════════════════════════════════════════════════════
-- DOMAIN EVENT HELPERS
-- ══════════════════════════════════════════════════════════════

-- | Log a graph transition.
--
-- Produces greppable output like: @GRAPH entry → classify@
--
-- @
-- logGraph GraphTransitionInfo
--   { fromNode = "entry"
--   , toNode = "classify"
--   , payload = toJSON input
--   }
-- @
logGraph :: Member Log effs => GraphTransitionInfo -> Eff effs ()
logGraph info = logInfoWith ("GRAPH " <> info.fromNode <> " → " <> info.toNode)
  [ ("payload", info.payload)
  ]

-- | Log a state field change.
logState :: Member Log effs => StateSnapshotInfo -> Eff effs ()
logState info = logInfoWith ("STATE." <> info.field)
  [ ("before", info.before)
  , ("after", info.after)
  , ("changed", toJSON info.changed)
  ]

-- | Log an LLM request.
--
-- Use 'logDebugWith' for full prompts when debugging.
logLLMRequest :: Member Log effs => LLMRequestInfo -> Eff effs ()
logLLMRequest info = logInfoWith ("LLM.REQUEST " <> info.nodeName)
  [ ("model", toJSON info.model)
  , ("prompt_tokens", toJSON info.promptTokens)
  , ("tools", toJSON info.tools)
  ]

-- | Log an LLM response.
logLLMResponse :: Member Log effs => LLMResponseInfo -> Eff effs ()
logLLMResponse info = logInfoWith ("LLM.RESPONSE " <> info.nodeName)
  [ ("completion_tokens", toJSON info.completionTokens)
  , ("tool_calls", toJSON info.toolCalls)
  , ("latency_ms", toJSON info.latencyMs)
  ]

-- | Log an error with full context.
--
-- Always logs at Error level regardless of verbosity.
logErrorContext :: Member Log effs => ErrorContextInfo -> Eff effs ()
logErrorContext info = logErrorWith ("ERROR: " <> info.message)
  [ ("node", toJSON info.node)
  , ("state", info.state)
  , ("transitions", toJSON info.transitions)
  , ("last_llm", toJSON info.lastLLM)
  , ("stack_trace", toJSON info.stackTrace)
  ]
