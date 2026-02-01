-- | Session effect for orchestrating dockerized Claude Code sessions.
--
-- This effect replaces ClaudeCodeExec, treating sessions as first-class entities
-- rather than optional parameters. Each operation spawns a `exomonad session` process.
--
-- = Architecture
--
-- @
-- Haskell (orchestrator)
--     │
--     │ spawn process
--     ▼
-- exomonad session start/continue/fork
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
-- ExoMonad manages worktrees at `.exomonad/worktrees/<branch>/` and tracks state
-- in `.exomonad/sessions.json`. Haskell is stateless.
--
-- = Usage
--
-- @
-- import ExoMonad.Effect.Session
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
-- The interpreter (in exomonad-session-interpreter) spawns `exomonad session` processes.
module ExoMonad.Effect.Session
  ( -- * Effect
    Session (..),

    -- * Types
    SessionId (..),
    SessionInfo (..),
    SessionOutput (..),
    SessionMetadata (..),
    InterruptSignal (..),
    SessionOperation (..),
    ToolCall (..),

    -- * Smart Constructors
    startSession,
    continueSession,
    forkSession,

    -- * Fork Detection
    detectForkRequest,

    -- * Pure Interpreter (for testing)
    runSession,
  )
where

import Control.Monad.Freer (Eff, Member, send)
import Data.Aeson (FromJSON (..), ToJSON (..), Value)
import ExoMonad.Graph.Types (ModelChoice)

-- ════════════════════════════════════════════════════════════════════════════
-- TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Claude Code session identifier.
--
-- Sessions persist across container lifecycles via shared ~/.claude/ volume.
newtype SessionId = SessionId {unSessionId :: Text}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, ToJSON)
  deriving anyclass (StructuredOutput)

instance HasJSONSchema SessionId where
  jsonSchema = emptySchema TString

-- | Bundled session info for continuation/forking.
--
-- Handlers store this in Memory to enable stateless continuation.
-- All fields come from 'SessionOutput' after a session completes.
--
-- @
-- -- Store after session completes:
-- updateMem @HandlerMem $ \\m -> m { sessionInfo = Just $
--   SessionInfo result.ccrSessionId result.ccrWorktree result.ccrBranch }
--
-- -- Use for continuation:
-- case mem.sessionInfo of
--   Just info -> ContinueFrom info.siSessionId info.siWorktree info.siBranch
--   Nothing -> StartFresh "slug"
-- @
data SessionInfo = SessionInfo
  { -- | CC session ID for --resume flag
    siSessionId :: SessionId,
    -- | Worktree path for exomonad
    siWorktree :: FilePath,
    -- | Git branch name
    siBranch :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Session operation strategy for ClaudeCode handlers.
--
-- Defines how the next ClaudeCode session should be spawned:
--
-- * 'StartFresh' - Create a new session with given slug
-- * 'ContinueFrom' - Resume an existing session (preserves conversation history)
-- * 'ForkFrom' - Create a read-only fork from parent session
--
-- ExoMonad is stateless, so continue/fork operations must include all session data.
-- This data comes from the previous 'SessionOutput' stored in handler Memory.
--
-- Before handlers return this alongside template context to communicate
-- session strategy to executeClaudeCodeHandler:
--
-- @
-- beforeHandler input = do
--   ctx <- buildContext input
--   mem <- getMem @HandlerMem
--   sessionOp <- case (mem.parentInfo, mem.lastSession) of
--     (Just pinfo, _) -> pure $ ForkFrom pinfo.ccSid pinfo.worktree pinfo.branch "child-slug"
--     (_, Just sinfo) -> pure $ ContinueFrom sinfo.ccSid sinfo.worktree sinfo.branch
--     (Nothing, Nothing) -> pure $ StartFresh "fresh-slug"
--   pure (ctx, sessionOp)
-- @
data SessionOperation
  = -- | Fresh session with slug
    StartFresh Text
  | -- | Continue: (cc_session_id, worktree, branch)
    ContinueFrom SessionId FilePath Text
  | -- | Fork: (parent_cc_session_id, parent_worktree, parent_branch, child_slug)
    ForkFrom SessionId FilePath Text Text
  deriving (Show, Eq)

-- | Result from a session turn (parsed from exomonad JSON stdout).
--
-- Returned by start/continue/fork operations.
--
-- Field naming: @so@ prefix stripped, camelCase → snake_case for JSON.
-- @soSessionId@ → @session_id@
data SessionOutput = SessionOutput
  { -- | ExoMonad session ID (UUID)
    soSessionId :: SessionId,
    -- | Claude Code session ID (for --resume/--fork-session)
    soCcSessionId :: Maybe Text,
    -- | Git branch name
    soBranch :: Text,
    -- | Absolute path to worktree
    soWorktree :: FilePath,
    -- | Container exit code
    soExitCode :: Int,
    -- | Whether Claude reported an error
    soIsError :: Bool,
    -- | Final output text (prose)
    soResultText :: Maybe Text,
    -- | JSON output (when schema was provided)
    soStructuredOutput :: Maybe Value,
    -- | API cost for this turn
    soTotalCostUsd :: Double,
    -- | Number of turns in this run
    soNumTurns :: Int,
    -- | Fork requests, escalations, etc.
    soInterrupts :: [InterruptSignal],
    -- | Wall-clock duration in seconds
    soDurationSecs :: Double,
    -- | Error if failed before Claude ran
    soError :: Maybe Text,
    -- | Decision tool calls from Claude Code
    soToolCalls :: Maybe [ToolCall],
    -- | Captured stderr on error (for auth/setup diagnosis)
    soStderrOutput :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON SessionOutput where
  toJSON = genericToJSON sessionOutputOptions

instance FromJSON SessionOutput where
  parseJSON = genericParseJSON sessionOutputOptions

sessionOutputOptions :: Options
sessionOutputOptions =
  defaultOptions
    { fieldLabelModifier = camelToSnake . dropPrefix "so",
      omitNothingFields = True
    }

-- | Session metadata from @exomonad session info@.
--
-- Field naming: @sm@ prefix stripped, camelCase → snake_case for JSON.
data SessionMetadata = SessionMetadata
  { smSessionId :: SessionId,
    smBranch :: Text,
    smWorktree :: FilePath,
    smParentSession :: Maybe SessionId,
    smChildSessions :: [SessionId],
    -- | "idle", "active", "completed", "failed"
    smStatus :: Text,
    smCreatedAt :: UTCTime,
    smUpdatedAt :: UTCTime,
    smLastExitCode :: Int,
    smTotalCostUsd :: Double
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON SessionMetadata where
  toJSON = genericToJSON sessionMetadataOptions

instance FromJSON SessionMetadata where
  parseJSON = genericParseJSON sessionMetadataOptions

sessionMetadataOptions :: Options
sessionMetadataOptions =
  defaultOptions
    { fieldLabelModifier = camelToSnake . dropPrefix "sm",
      omitNothingFields = True
    }

-- | Signal from Claude via @exomonad signal@ bash command.
--
-- Claude can emit signals (fork requests, escalations) that appear in
-- the @soInterrupts@ field of SessionOutput.
--
-- Field naming: @is@ prefix stripped, camelCase → snake_case for JSON.
data InterruptSignal = InterruptSignal
  { -- | "fork", "escalate", "transition", etc.
    isSignalType :: Text,
    -- | e.g., child branch name for fork
    isState :: Maybe Text,
    -- | e.g., child prompt for fork
    isReason :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON InterruptSignal where
  toJSON = genericToJSON interruptSignalOptions

instance FromJSON InterruptSignal where
  parseJSON = genericParseJSON interruptSignalOptions

interruptSignalOptions :: Options
interruptSignalOptions =
  defaultOptions
    { fieldLabelModifier = camelToSnake . dropPrefix "is",
      omitNothingFields = True
    }

-- | A tool call from Claude Code (for decision tools).
--
-- When Claude calls @decision::approve { approvedBy: "alice", notes: "LGTM" }@,
-- exomonad captures this and returns it in @SessionOutput.soToolCalls@.
--
-- Field naming: @tc@ prefix stripped, camelCase → snake_case for JSON.
data ToolCall = ToolCall
  { -- | Full tool name (e.g., "decision::approve")
    tcName :: !Text,
    -- | Tool input (the branch's field values)
    tcInput :: !Value
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ToolCall where
  toJSON = genericToJSON toolCallOptions

instance FromJSON ToolCall where
  parseJSON = genericParseJSON toolCallOptions

toolCallOptions :: Options
toolCallOptions =
  defaultOptions
    { fieldLabelModifier = camelToSnake . dropPrefix "tc",
      omitNothingFields = True
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
    go isFirst (c : cs)
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
-- Each operation spawns a @exomonad session@ process:
--
-- * @StartSession@ → @exomonad session start --slug X --prompt Y --model Z [--json-schema S] [--decision-tools T]@
-- * @ContinueSession@ → @exomonad session continue <id> --prompt Y [--json-schema S] [--decision-tools T]@
-- * @ForkSession@ → @exomonad session fork <id> --child-slug X --child-prompt Y [--json-schema S] [--decision-tools T]@
-- * @SessionInfo@ → @exomonad session info <id>@
--
-- ExoMonad handles container lifecycle and worktree management. Haskell only
-- sees JSON results. The slug is LLM-generated (from preceding node output)
-- and can optionally include a phase prefix (e.g., "implement/user-auth").
-- ExoMonad appends a hex suffix for uniqueness.
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
  -- ExoMonad creates branch @<slug>-<hex>@ and worktree at
  -- @.exomonad/worktrees/<slug>-<hex>/@.
  StartSession ::
    -- | Slug (e.g., "user-auth" or "implement/user-auth")
    Text ->
    -- | Initial prompt
    Text ->
    -- | Model: Haiku, Sonnet, Opus
    ModelChoice ->
    -- | JSON schema for structured output (optional)
    Maybe Value ->
    -- | Decision tools JSON array (optional)
    Maybe Value ->
    Session SessionOutput
  -- | Continue an existing session (subsequent turns).
  --
  -- All session data passed as args (exomonad is stateless).
  ContinueSession ::
    -- | Claude Code session ID (for --resume)
    SessionId ->
    -- | Worktree path
    FilePath ->
    -- | Branch name
    Text ->
    -- | Model to use
    ModelChoice ->
    -- | Prompt for this turn
    Text ->
    -- | JSON schema for structured output (optional)
    Maybe Value ->
    -- | Decision tools JSON array (optional)
    Maybe Value ->
    Session SessionOutput
  -- | Fork a session (child inherits parent's conversation history).
  --
  -- Uses @--resume <parent> --fork-session@ so parent is unchanged.
  -- All parent session data passed as args (exomonad is stateless).
  ForkSession ::
    -- | Parent's Claude Code session ID (for --fork-session)
    SessionId ->
    -- | Parent's worktree path
    FilePath ->
    -- | Parent's branch name
    Text ->
    -- | Model to use
    ModelChoice ->
    -- | Child slug (e.g., "subtask/handle-edge-case")
    Text ->
    -- | Child prompt
    Text ->
    -- | JSON schema for structured output (optional)
    Maybe Value ->
    -- | Decision tools JSON array (optional)
    Maybe Value ->
    Session SessionOutput -- Returns child's first turn result

-- ════════════════════════════════════════════════════════════════════════════
-- SMART CONSTRUCTORS
-- ════════════════════════════════════════════════════════════════════════════

-- | Start a new session with optional structured output and decision tools.
--
-- The slug is typically LLM-generated from the preceding node's structured
-- output. It can include a phase prefix (e.g., "implement/user-auth").
-- ExoMonad appends a hex suffix for uniqueness.
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
startSession ::
  (Member Session effs) =>
  -- | Slug (e.g., "user-auth" or "implement/user-auth")
  Text ->
  -- | Initial prompt
  Text ->
  -- | Model to use
  ModelChoice ->
  -- | JSON schema for structured output (optional)
  Maybe Value ->
  -- | Decision tools JSON array (optional)
  Maybe Value ->
  Eff effs SessionOutput
startSession slug prompt model schema tools =
  send $ StartSession slug prompt model schema tools

-- | Continue an existing session with optional structured output and decision tools.
--
-- All session data must be passed (exomonad is stateless).
--
-- @
-- result <- continueSession ccSid worktree branch Sonnet "Now add error handling" Nothing Nothing
-- @
continueSession ::
  (Member Session effs) =>
  -- | Claude Code session ID (for --resume)
  SessionId ->
  -- | Worktree path
  FilePath ->
  -- | Branch name
  Text ->
  -- | Model to use
  ModelChoice ->
  -- | Prompt for this turn
  Text ->
  -- | JSON schema for structured output (optional)
  Maybe Value ->
  -- | Decision tools JSON array (optional)
  Maybe Value ->
  Eff effs SessionOutput
continueSession ccSid worktree branch model prompt schema tools =
  send $ ContinueSession ccSid worktree branch model prompt schema tools

-- | Fork a session (child inherits parent's history) with optional structured output
-- and decision tools.
--
-- Parent session is unchanged. Child gets a new session ID.
-- All parent session data must be passed (exomonad is stateless).
--
-- @
-- childResult <- forkSession parentCcSid parentWorktree parentBranch Sonnet "subtask/auth-tests" "Write tests for auth" Nothing Nothing
-- let childSid = soSessionId childResult
-- -- Child branch: subtask/auth-tests-7b2c4e
-- @
forkSession ::
  (Member Session effs) =>
  -- | Parent's Claude Code session ID (for --fork-session)
  SessionId ->
  -- | Parent's worktree path
  FilePath ->
  -- | Parent's branch name
  Text ->
  -- | Model to use
  ModelChoice ->
  -- | Child slug (e.g., "subtask/auth-tests")
  Text ->
  -- | Child prompt
  Text ->
  -- | JSON schema for structured output (optional)
  Maybe Value ->
  -- | Decision tools JSON array (optional)
  Maybe Value ->
  Eff effs SessionOutput
forkSession parentCcSid parentWorktree parentBranch model childSlug childPrompt schema tools =
  send $ ForkSession parentCcSid parentWorktree parentBranch model childSlug childPrompt schema tools

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
    Nothing -> Nothing
  where
    isForkSignal sig = sig.isSignalType == "fork"

-- ════════════════════════════════════════════════════════════════════════════
-- PURE INTERPRETER
-- ════════════════════════════════════════════════════════════════════════════

-- | Run Session effect with a pure handler.
--
-- Used for testing or alternative implementations.
-- For IO-based execution, see @runSessionIO@ in exomonad-session-interpreter.
--
-- @
-- let mockHandler = \\case
--       StartSession slug _ _ _ _ -> pure mockOutput { soBranch = slug <> \"-abc123\" }
--       ContinueSession _ _ _ _ _ _ _ -> pure mockOutput
--       ForkSession _ _ _ _ childSlug _ _ _ -> pure mockOutput { soBranch = childSlug <> \"-def456\" }
--
-- result <- runM $ runSession mockHandler myProgram
-- @
runSession ::
  (forall x. Session x -> Eff effs x) ->
  Eff (Session ': effs) a ->
  Eff effs a
runSession handler = interpret $ \case
  op@(StartSession _ _ _ _ _) -> handler op
  op@(ContinueSession _ _ _ _ _ _ _) -> handler op
  op@(ForkSession _ _ _ _ _ _ _ _) -> handler op
