{-# LANGUAGE StrictData #-}

-- | Development logging effect for LLM-friendly debugging.
--
-- Separate from 'Log' (internal) and 'Observability' (Loki/production).
-- Designed for greppable, session-scoped log files during local development.
--
-- == Log Format
--
-- @
-- ═══ SESSION f47ac10b ═══════════════════════════════════════════════
-- [12:00:00.000] CONNECT ws://localhost:8080
--
-- [12:00:00.050] GRAPH entry → resumeRouter
--   input: PlayerInput { piActionText = "attack" }
--
-- [12:00:00.051] STATE.mood: MoodScene Encounter (unchanged)
-- [12:00:00.051] STATE.dicePool: [4,4,3]
--
-- [12:00:00.100] LLM.REQUEST dm_scene
--   model: claude-sonnet-4-20250514
--   tokens: 1,847 prompt
--   tools: [engage, end_scene]
-- @
--
-- == Greppable Patterns
--
-- @
-- grep "GRAPH" latest.log        # Transitions
-- grep "STATE\\." latest.log     # State changes
-- grep "LLM\\." latest.log       # LLM calls
-- grep "ERROR" latest.log        # Errors
-- @
module Tidepool.Effect.DevLog
  ( -- * Effect
    DevLog (..)

    -- * Smart Constructors
  , devLogGraph
  , devLogState
  , devLogLLMRequest
  , devLogLLMResponse
  , devLogError
  , devLogRaw

    -- * Event Types
  , DevLogEvent (..)
  , GraphTransitionInfo (..)
  , StateSnapshotInfo (..)
  , LLMRequestInfo (..)
  , LLMResponseInfo (..)
  , ErrorContextInfo (..)

    -- * Configuration
  , Verbosity (..)

    -- * Runners
  , runDevLogPure
  ) where

import Control.Monad.Freer (Eff, Member, interpret, send)
import Data.Aeson (ToJSON (..), Value)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Verbosity levels for filtering log output.
--
-- Higher levels include all lower level output.
data Verbosity
  = VQuiet    -- ^ Errors only
  | VNormal   -- ^ Transitions, state deltas, LLM summaries (default)
  | VVerbose  -- ^ Full LLM prompts/responses
  | VTrace    -- ^ Handler internal decisions
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance ToJSON Verbosity

-- | Core DevLog effect.
--
-- Single operation to emit any event type. The interpreter
-- filters based on verbosity and formats for output.
data DevLog r where
  LogDevEvent :: DevLogEvent -> DevLog ()

-- | All DevLog event variants.
--
-- Each event carries its minimum verbosity level.
data DevLogEvent
  = EventGraph      Verbosity GraphTransitionInfo
  | EventState      Verbosity StateSnapshotInfo
  | EventLLMRequest Verbosity LLMRequestInfo
  | EventLLMResponse Verbosity LLMResponseInfo
  | EventError      ErrorContextInfo  -- Always emitted (VQuiet)
  | EventRaw        Verbosity Text
  deriving (Show, Generic)

instance ToJSON DevLogEvent

-- | Graph transition info.
--
-- Logged automatically by instrumented dispatch.
-- Timestamps are added by the executor, not the caller.
data GraphTransitionInfo = GraphTransitionInfo
  { gtiFromNode :: Text
  , gtiToNode   :: Text
  , gtiPayload  :: Value    -- ^ Input to next handler (JSON-encoded)
  }
  deriving (Show, Generic)

instance ToJSON GraphTransitionInfo

-- | State snapshot with diff info.
--
-- Logged after each transition for changed fields.
data StateSnapshotInfo = StateSnapshotInfo
  { ssiField   :: Text   -- ^ e.g., "mood", "dicePool"
  , ssiBefore  :: Value
  , ssiAfter   :: Value
  , ssiChanged :: Bool   -- ^ Quick check for "(unchanged)"
  }
  deriving (Show, Generic)

instance ToJSON StateSnapshotInfo

-- | LLM request info.
--
-- At VNormal: metadata only (model, tokens, tools).
-- At VVerbose: includes full prompts.
-- Timestamps are added by the executor, not the caller.
data LLMRequestInfo = LLMRequestInfo
  { lriNodeName     :: Text
  , lriModel        :: Text
  , lriPromptTokens :: Int
  , lriTools        :: [Text]  -- ^ Tool names available
  -- Full content (only at VVerbose)
  , lriSystemPrompt :: Maybe Text
  , lriUserPrompt   :: Maybe Text
  }
  deriving (Show, Generic)

instance ToJSON LLMRequestInfo

-- | LLM response info.
-- Timestamps are added by the executor, not the caller.
data LLMResponseInfo = LLMResponseInfo
  { lroNodeName         :: Text
  , lroCompletionTokens :: Int
  , lroToolCalls        :: Int
  , lroLatencyMs        :: Int
  -- Full content (only at VVerbose)
  , lroResponse         :: Maybe Text
  }
  deriving (Show, Generic)

instance ToJSON LLMResponseInfo

-- | Error context for debugging.
--
-- Includes state snapshot and last LLM call for context.
data ErrorContextInfo = ErrorContextInfo
  { eciMessage     :: Text
  , eciNode        :: Maybe Text
  , eciState       :: Value        -- ^ Current state snapshot
  , eciTransitions :: [(Text, Text)]  -- ^ Last N transitions (from, to)
  , eciLastLLM     :: Maybe LLMResponseInfo
  , eciStackTrace  :: Maybe Text
  }
  deriving (Show, Generic)

instance ToJSON ErrorContextInfo

-- ══════════════════════════════════════════════════════════════
-- SMART CONSTRUCTORS
-- ══════════════════════════════════════════════════════════════

-- | Log a graph transition (auto-instrumented).
devLogGraph :: Member DevLog effs => GraphTransitionInfo -> Eff effs ()
devLogGraph = send . LogDevEvent . EventGraph VNormal

-- | Log a state field change.
devLogState :: Member DevLog effs => StateSnapshotInfo -> Eff effs ()
devLogState = send . LogDevEvent . EventState VNormal

-- | Log an LLM request.
--
-- Pass 'VNormal' for metadata only, 'VVerbose' to include prompts.
devLogLLMRequest :: Member DevLog effs => Verbosity -> LLMRequestInfo -> Eff effs ()
devLogLLMRequest v = send . LogDevEvent . EventLLMRequest v

-- | Log an LLM response.
devLogLLMResponse :: Member DevLog effs => Verbosity -> LLMResponseInfo -> Eff effs ()
devLogLLMResponse v = send . LogDevEvent . EventLLMResponse v

-- | Log an error with context.
devLogError :: Member DevLog effs => ErrorContextInfo -> Eff effs ()
devLogError = send . LogDevEvent . EventError

-- | Log a raw message at specified verbosity.
devLogRaw :: Member DevLog effs => Verbosity -> Text -> Eff effs ()
devLogRaw v = send . LogDevEvent . EventRaw v

-- ══════════════════════════════════════════════════════════════
-- RUNNERS
-- ══════════════════════════════════════════════════════════════

-- | Pure runner that discards all events.
--
-- Use for tests or when DevLog is in the effect stack but not needed.
runDevLogPure :: Eff (DevLog ': effs) a -> Eff effs a
runDevLogPure = interpret $ \case
  LogDevEvent _ -> pure ()
