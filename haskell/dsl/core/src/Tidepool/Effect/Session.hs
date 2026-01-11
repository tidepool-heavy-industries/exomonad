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
  , SessionOperation(..)
  , ToolCall(..)

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
import Tidepool.Schema (HasJSONSchema(..), emptySchema, SchemaType(TString))
import Tidepool.StructuredOutput (StructuredOutput)


-- ════════════════════════════════════════════════════════════════════════════
-- TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Claude Code session identifier.
--
-- Sessions persist across container lifecycles via shared ~/.claude/ volume.
newtype SessionId = SessionId { unSessionId :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, ToJSON)
  deriving anyclass (StructuredOutput)

instance HasJSONSchema SessionId where
  jsonSchema = emptySchema TString


-- | Session operation strategy for ClaudeCode handlers.
--
-- Defines how the next ClaudeCode session should be spawned:
--
-- * 'StartFresh' - Create a new session with given slug
-- * 'ContinueFrom' - Resume an existing session (preserves conversation history)
-- * 'ForkFrom' - Create a read-only fork from parent session
--
-- Before handlers return this alongside template context to communicate
-- session strategy to executeClaudeCodeHandler:
--
-- @
-- beforeHandler input = do
--   ctx <- buildContext input
--   sessionOp <- case mParentSession of
--     Just parentSid -> pure $ ForkFrom parentSid "child-slug"
--     Nothing -> pure $ StartFresh "fresh-slug"
--   pure (ctx, sessionOp)
-- @
data SessionOperation
  = StartFresh Text                    -- ^ Fresh session with slug
  | ContinueFrom SessionId             -- ^ Continue existing session
  | ForkFrom SessionId Text            -- ^ Fork from parent with child slug
  deriving (Show, Eq)


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
  , soToolCalls        :: Maybe [ToolCall] -- ^ Decision tool calls from Claude Code
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


-- | A tool call from Claude Code (for decision tools).
--
-- When Claude calls @decision::approve { approvedBy: "alice", notes: "LGTM" }@,
-- mantle captures this and returns it in @SessionOutput.soToolCalls@.
--
-- Field naming: @tc@ prefix stripped, camelCase → snake_case for JSON.
data ToolCall = ToolCall
  { tcName  :: !Text   -- ^ Full tool name (e.g., "decision::approve")
  , tcInput :: !Value  -- ^ Tool input (the branch's field values)
  } deriving stock (Show, Eq, Generic)

instance ToJSON ToolCall where
  toJSON = genericToJSON toolCallOptions

instance FromJSON ToolCall where
  parseJSON = genericParseJSON toolCallOptions

toolCallOptions :: Options
toolCallOptions = defaultOptions
  { fieldLabelModifier = camelToSnake . dropPrefix "tc"
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
-- * @StartSession@ → @mantle session start --slug X --prompt Y --model Z [--json-schema S] [--decision-tools T]@
-- * @ContinueSession@ → @mantle session continue <id> --prompt Y [--json-schema S] [--decision-tools T]@
-- * @ForkSession@ → @mantle session fork <id> --child-slug X --child-prompt Y [--json-schema S] [--decision-tools T]@
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
--
-- When decision tools are provided, Claude Code is expected to call one of them
-- to indicate its decision. The tool call appears in @soToolCalls@.
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
    -> Maybe Value    -- ^ Decision tools JSON array (optional)
    -> Session SessionOutput

  -- | Continue an existing session (subsequent turns).
  --
  -- Reuses the session's existing worktree.
  ContinueSession
    :: SessionId      -- ^ Session to continue
    -> Text           -- ^ Prompt for this turn
    -> Maybe Value    -- ^ JSON schema for structured output (optional)
    -> Maybe Value    -- ^ Decision tools JSON array (optional)
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
    -> Maybe Value    -- ^ Decision tools JSON array (optional)
    -> Session SessionOutput  -- Returns child's first turn result

  -- | Query session metadata from @.mantle/sessions.json@.
  SessionInfo
    :: SessionId
    -> Session (Maybe SessionMetadata)


-- ════════════════════════════════════════════════════════════════════════════
-- SMART CONSTRUCTORS
-- ════════════════════════════════════════════════════════════════════════════

-- | Start a new session with optional structured output and decision tools.
--
-- The slug is typically LLM-generated from the preceding node's structured
-- output. It can include a phase prefix (e.g., "implement/user-auth").
-- Mantle appends a hex suffix for uniqueness.
--
-- @
-- -- Without schema (prose output only):
-- result <- startSession "implement/user-auth" "Implement OAuth login" Sonnet Nothing Nothing
--
-- -- With schema (structured JSON output):
-- result <- startSession "classify/intent" "Classify: ..." Haiku (Just schema) Nothing
-- let output = soStructuredOutput result  -- Just Value when schema provided
--
-- -- With decision tools (for sum type outputs):
-- result <- startSession "review" "Review this PR" Sonnet Nothing (Just toolsJson)
-- let toolCalls = soToolCalls result  -- Just [ToolCall] when tools provided
-- @
startSession
  :: Member Session effs
  => Text           -- ^ Slug (e.g., "user-auth" or "implement/user-auth")
  -> Text           -- ^ Initial prompt
  -> ModelChoice    -- ^ Model to use
  -> Maybe Value    -- ^ JSON schema for structured output (optional)
  -> Maybe Value    -- ^ Decision tools JSON array (optional)
  -> Eff effs SessionOutput
startSession slug prompt model schema tools =
  send $ StartSession slug prompt model schema tools

-- | Continue an existing session with optional structured output and decision tools.
--
-- @
-- result <- continueSession sid "Now add error handling" Nothing Nothing
-- @
continueSession
  :: Member Session effs
  => SessionId      -- ^ Session to continue
  -> Text           -- ^ Prompt for this turn
  -> Maybe Value    -- ^ JSON schema for structured output (optional)
  -> Maybe Value    -- ^ Decision tools JSON array (optional)
  -> Eff effs SessionOutput
continueSession sid prompt schema tools =
  send $ ContinueSession sid prompt schema tools

-- | Fork a session (child inherits parent's history) with optional structured output
-- and decision tools.
--
-- Parent session is unchanged. Child gets a new session ID.
-- The child slug can include a phase prefix (e.g., "subtask/edge-case").
--
-- @
-- childResult <- forkSession parentSid "subtask/auth-tests" "Write tests for auth" Nothing Nothing
-- let childSid = soSessionId childResult
-- -- Child branch: subtask/auth-tests-7b2c4e
-- @
forkSession
  :: Member Session effs
  => SessionId      -- ^ Parent session
  -> Text           -- ^ Child slug (e.g., "subtask/auth-tests")
  -> Text           -- ^ Child prompt
  -> Maybe Value    -- ^ JSON schema for structured output (optional)
  -> Maybe Value    -- ^ Decision tools JSON array (optional)
  -> Eff effs SessionOutput
forkSession parent childSlug childPrompt schema tools =
  send $ ForkSession parent childSlug childPrompt schema tools

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
--       StartSession slug _ _ _ _ -> pure mockOutput { soBranch = slug <> \"-abc123\" }
--       ContinueSession _ _ _ _ -> pure mockOutput
--       ForkSession _ childSlug _ _ _ -> pure mockOutput { soBranch = childSlug <> \"-def456\" }
--       SessionInfo _ -> pure Nothing
--
-- result <- runM $ runSession mockHandler myProgram
-- @
runSession
  :: (forall x. Session x -> Eff effs x)
  -> Eff (Session ': effs) a
  -> Eff effs a
runSession handler = interpret $ \case
  op@(StartSession _ _ _ _ _) -> handler op
  op@(ContinueSession _ _ _ _) -> handler op
  op@(ForkSession _ _ _ _ _) -> handler op
  op@(SessionInfo _) -> handler op
