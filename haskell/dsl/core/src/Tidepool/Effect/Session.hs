-- | Session effect for orchestrating dockerized Claude Code sessions.
--
-- This effect replaces ClaudeCodeExec, treating sessions as first-class entities
-- rather than optional parameters. Each operation spawns a `mantle session` process.
--
-- = Architecture
--
-- @
-- Haskell (orchestrator)
--     │
--     │ spawn process
--     ▼
-- mantle session start/continue/fork
--     │
--     │ runs container
--     ▼
-- JSON result to stdout
-- @
--
-- = Key Insight: Sessions Not Containers
--
-- Containers are ephemeral (one turn each), sessions persist via:
-- * Shared `~/.claude/` volume (history lives here)
-- * `--resume <session-id>` for continuation
-- * `--fork-session` for read-only forks
--
-- Mantle manages worktrees at `.mantle/worktrees/<branch>/` and tracks state
-- in `.mantle/sessions.json`. Haskell is stateless.
--
-- = Usage
--
-- @
-- import Tidepool.Effect.Session
--
-- workflow :: Member Session effs => Eff effs Response
-- workflow = do
--   result <- startSession "implement/feat-x" "Implement feature X" Sonnet
--   case detectForkRequest result of
--     Just (childSlug, prompt) -> do
--       childResult <- forkSession (soSessionId result) childSlug prompt
--       continueSession (soSessionId result) "Child completed"
--     Nothing -> pure (extractResult result)
-- @
--
-- The interpreter (in tidepool-session-executor) spawns `mantle session` processes.
module Tidepool.Effect.Session
  ( -- * Effect
    Session(..)

    -- * Types
  , SessionId(..)
  , SessionOutput(..)
  , SessionMetadata(..)
  , InterruptSignal(..)

    -- * Smart Constructors
  , startSession
  , continueSession
  , forkSession
  , sessionInfo

    -- * Fork Detection
  , detectForkRequest

    -- * Pure Interpreter (for testing)
  , runSession
  ) where

import Control.Monad.Freer (Eff, Member, interpret, send)
import Data.Aeson
  ( FromJSON(..), ToJSON(..), Options(..), Value
  , defaultOptions, genericToJSON, genericParseJSON
  )
import Data.Char (toLower, isUpper)
import Data.List (find)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

import Tidepool.Graph.Types (ModelChoice)


-- ════════════════════════════════════════════════════════════════════════════
-- TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Claude Code session identifier.
--
-- Sessions persist across container lifecycles via shared ~/.claude/ volume.
newtype SessionId = SessionId { unSessionId :: Text }
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON)


-- | Result from a session turn (parsed from mantle JSON stdout).
--
-- Returned by start/continue/fork operations.
--
-- Field naming: @so@ prefix stripped, camelCase → snake_case for JSON.
-- @soSessionId@ → @session_id@
data SessionOutput = SessionOutput
  { soSessionId        :: SessionId      -- ^ Claude Code session ID
  , soBranch           :: Text           -- ^ Git branch name
  , soWorktree         :: FilePath       -- ^ Absolute path to worktree
  , soExitCode         :: Int            -- ^ Container exit code
  , soIsError          :: Bool           -- ^ Whether Claude reported an error
  , soResultText       :: Maybe Text     -- ^ Final output text (prose)
  , soStructuredOutput :: Maybe Value    -- ^ JSON output (when schema was provided)
  , soTotalCostUsd     :: Double         -- ^ API cost for this turn
  , soNumTurns         :: Int            -- ^ Number of turns in this run
  , soInterrupts       :: [InterruptSignal] -- ^ Fork requests, escalations, etc.
  , soDurationSecs     :: Double         -- ^ Wall-clock duration in seconds
  , soError            :: Maybe Text     -- ^ Error if failed before Claude ran
  } deriving stock (Show, Eq, Generic)

instance ToJSON SessionOutput where
  toJSON = genericToJSON sessionOutputOptions

instance FromJSON SessionOutput where
  parseJSON = genericParseJSON sessionOutputOptions

sessionOutputOptions :: Options
sessionOutputOptions = defaultOptions
  { fieldLabelModifier = camelToSnake . dropPrefix "so"
  , omitNothingFields = True
  }


-- | Session metadata from @mantle session info@.
--
-- Field naming: @sm@ prefix stripped, camelCase → snake_case for JSON.
data SessionMetadata = SessionMetadata
  { smSessionId     :: SessionId
  , smBranch        :: Text
  , smWorktree      :: FilePath
  , smParentSession :: Maybe SessionId
  , smChildSessions :: [SessionId]
  , smStatus        :: Text           -- ^ "idle", "active", "completed", "failed"
  , smCreatedAt     :: UTCTime
  , smUpdatedAt     :: UTCTime
  , smLastExitCode  :: Int
  , smTotalCostUsd  :: Double
  } deriving stock (Show, Eq, Generic)

instance ToJSON SessionMetadata where
  toJSON = genericToJSON sessionMetadataOptions

instance FromJSON SessionMetadata where
  parseJSON = genericParseJSON sessionMetadataOptions

sessionMetadataOptions :: Options
sessionMetadataOptions = defaultOptions
  { fieldLabelModifier = camelToSnake . dropPrefix "sm"
  , omitNothingFields = True
  }


-- | Signal from Claude via @mantle signal@ bash command.
--
-- Claude can emit signals (fork requests, escalations) that appear in
-- the @soInterrupts@ field of SessionOutput.
--
-- Field naming: @is@ prefix stripped, camelCase → snake_case for JSON.
data InterruptSignal = InterruptSignal
  { isSignalType :: Text           -- ^ "fork", "escalate", "transition", etc.
  , isState      :: Maybe Text     -- ^ e.g., child branch name for fork
  , isReason     :: Maybe Text     -- ^ e.g., child prompt for fork
  } deriving stock (Show, Eq, Generic)

instance ToJSON InterruptSignal where
  toJSON = genericToJSON interruptSignalOptions

instance FromJSON InterruptSignal where
  parseJSON = genericParseJSON interruptSignalOptions

interruptSignalOptions :: Options
interruptSignalOptions = defaultOptions
  { fieldLabelModifier = camelToSnake . dropPrefix "is"
  , omitNothingFields = True
  }


-- ════════════════════════════════════════════════════════════════════════════
-- JSON HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Drop a prefix from a field name.
--
-- @dropPrefix "so" "soSessionId" = "SessionId"@
dropPrefix :: String -> String -> String
dropPrefix prefix field
  | take (length prefix) field == prefix = drop (length prefix) field
  | otherwise = field

-- | Convert camelCase to snake_case.
--
-- @camelToSnake "SessionId" = "session_id"@
-- @camelToSnake "TotalCostUsd" = "total_cost_usd"@
camelToSnake :: String -> String
camelToSnake = go True
  where
    go _ [] = []
    go isFirst (c:cs)
      | isUpper c =
          let lower = toLower c
          in if isFirst
             then lower : go False cs
             else '_' : lower : go False cs
      | otherwise = c : go False cs


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT
-- ════════════════════════════════════════════════════════════════════════════

-- | Session effects for orchestrating dockerized Claude Code.
--
-- Each operation spawns a @mantle session@ process:
--
-- * @StartSession@ → @mantle session start --slug X --prompt Y --model Z [--json-schema S]@
-- * @ContinueSession@ → @mantle session continue <id> --prompt Y [--json-schema S]@
-- * @ForkSession@ → @mantle session fork <id> --child-slug X --child-prompt Y [--json-schema S]@
-- * @SessionInfo@ → @mantle session info <id>@
--
-- Mantle handles container lifecycle and worktree management. Haskell only
-- sees JSON results. The slug is LLM-generated (from preceding node output)
-- and can optionally include a phase prefix (e.g., "implement/user-auth").
-- Mantle appends a hex suffix for uniqueness.
--
-- When a JSON schema is provided, Claude Code enforces structured output and
-- the result appears in @soStructuredOutput@. Without a schema, only
-- @soResultText@ is populated.
data Session r where
  -- | Start a new session (first turn).
  --
  -- Mantle creates branch @<slug>-<hex>@ and worktree at
  -- @.mantle/worktrees/<slug>-<hex>/@.
  StartSession
    :: Text           -- ^ Slug (e.g., "user-auth" or "implement/user-auth")
    -> Text           -- ^ Initial prompt
    -> ModelChoice    -- ^ Model: Haiku, Sonnet, Opus
    -> Maybe Value    -- ^ JSON schema for structured output (optional)
    -> Session SessionOutput

  -- | Continue an existing session (subsequent turns).
  --
  -- Reuses the session's existing worktree.
  ContinueSession
    :: SessionId      -- ^ Session to continue
    -> Text           -- ^ Prompt for this turn
    -> Maybe Value    -- ^ JSON schema for structured output (optional)
    -> Session SessionOutput

  -- | Fork a session (child inherits parent's conversation history).
  --
  -- Uses @--resume <parent> --fork-session@ so parent is unchanged.
  -- Mantle creates child branch @<child-slug>-<hex>@ and worktree.
  ForkSession
    :: SessionId      -- ^ Parent session to fork from
    -> Text           -- ^ Child slug (e.g., "subtask/handle-edge-case")
    -> Text           -- ^ Child prompt
    -> Maybe Value    -- ^ JSON schema for structured output (optional)
    -> Session SessionOutput  -- Returns child's first turn result

  -- | Query session metadata from @.mantle/sessions.json@.
  SessionInfo
    :: SessionId
    -> Session (Maybe SessionMetadata)


-- ════════════════════════════════════════════════════════════════════════════
-- SMART CONSTRUCTORS
-- ════════════════════════════════════════════════════════════════════════════

-- | Start a new session with optional structured output.
--
-- The slug is typically LLM-generated from the preceding node's structured
-- output. It can include a phase prefix (e.g., "implement/user-auth").
-- Mantle appends a hex suffix for uniqueness.
--
-- @
-- -- Without schema (prose output only):
-- result <- startSession "implement/user-auth" "Implement OAuth login" Sonnet Nothing
--
-- -- With schema (structured JSON output):
-- result <- startSession "classify/intent" "Classify: ..." Haiku (Just schema)
-- let output = soStructuredOutput result  -- Just Value when schema provided
-- @
startSession
  :: Member Session effs
  => Text           -- ^ Slug (e.g., "user-auth" or "implement/user-auth")
  -> Text           -- ^ Initial prompt
  -> ModelChoice    -- ^ Model to use
  -> Maybe Value    -- ^ JSON schema for structured output (optional)
  -> Eff effs SessionOutput
startSession slug prompt model schema = send $ StartSession slug prompt model schema

-- | Continue an existing session with optional structured output.
--
-- @
-- result <- continueSession sid "Now add error handling" Nothing
-- @
continueSession
  :: Member Session effs
  => SessionId      -- ^ Session to continue
  -> Text           -- ^ Prompt for this turn
  -> Maybe Value    -- ^ JSON schema for structured output (optional)
  -> Eff effs SessionOutput
continueSession sid prompt schema = send $ ContinueSession sid prompt schema

-- | Fork a session (child inherits parent's history) with optional structured output.
--
-- Parent session is unchanged. Child gets a new session ID.
-- The child slug can include a phase prefix (e.g., "subtask/edge-case").
--
-- @
-- childResult <- forkSession parentSid "subtask/auth-tests" "Write tests for auth" Nothing
-- let childSid = soSessionId childResult
-- -- Child branch: subtask/auth-tests-7b2c4e
-- @
forkSession
  :: Member Session effs
  => SessionId      -- ^ Parent session
  -> Text           -- ^ Child slug (e.g., "subtask/auth-tests")
  -> Text           -- ^ Child prompt
  -> Maybe Value    -- ^ JSON schema for structured output (optional)
  -> Eff effs SessionOutput
forkSession parent childSlug childPrompt schema =
  send $ ForkSession parent childSlug childPrompt schema

-- | Query session metadata.
--
-- @
-- mInfo <- sessionInfo sid
-- case mInfo of
--   Just info -> putStrLn $ "Status: " <> smStatus info
--   Nothing -> putStrLn "Session not found"
-- @
sessionInfo
  :: Member Session effs
  => SessionId
  -> Eff effs (Maybe SessionMetadata)
sessionInfo sid = send $ SessionInfo sid


-- ════════════════════════════════════════════════════════════════════════════
-- FORK DETECTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Check if a session result contains a fork request.
--
-- Returns @(childSlug, childPrompt)@ if Claude requested a fork.
--
-- @
-- case detectForkRequest result of
--   Just (slug, prompt) -> forkSession (soSessionId result) slug prompt
--   Nothing -> continueSession (soSessionId result) "Continue"
-- @
detectForkRequest :: SessionOutput -> Maybe (Text, Text)
detectForkRequest result =
  case find isForkSignal result.soInterrupts of
    Just sig -> (,) <$> sig.isState <*> sig.isReason
    Nothing  -> Nothing
  where
    isForkSignal sig = sig.isSignalType == "fork"


-- ════════════════════════════════════════════════════════════════════════════
-- PURE INTERPRETER
-- ════════════════════════════════════════════════════════════════════════════

-- | Run Session effect with a pure handler.
--
-- Used for testing or alternative implementations.
-- For IO-based execution, see @runSessionIO@ in tidepool-session-executor.
--
-- @
-- let mockHandler = \\case
--       StartSession slug _ _ _ -> pure mockOutput { soBranch = slug <> \"-abc123\" }
--       ContinueSession _ _ _ -> pure mockOutput
--       ForkSession _ childSlug _ _ -> pure mockOutput { soBranch = childSlug <> \"-def456\" }
--       SessionInfo _ -> pure Nothing
--
-- result <- runM $ runSession mockHandler myProgram
-- @
runSession
  :: (forall x. Session x -> Eff effs x)
  -> Eff (Session ': effs) a
  -> Eff effs a
runSession handler = interpret $ \case
  op@(StartSession _ _ _ _) -> handler op
  op@(ContinueSession _ _ _) -> handler op
  op@(ForkSession _ _ _ _) -> handler op
  op@(SessionInfo _) -> handler op
