-- | Session effect executor - dockerized Claude Code orchestration.
--
-- Implements Session effect by spawning @mantle session@ processes.
-- Each operation runs one turn and returns JSON to stdout.
--
-- = Architecture
--
-- @
-- Haskell (this executor)
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
--
-- * Shared @~/.claude/@ volume (history lives here)
-- * @--resume <session-id>@ for continuation
-- * @--fork-session@ for read-only forks
--
-- Mantle manages worktrees at @.mantle/worktrees/<branch>/@ and tracks state
-- in @.mantle/sessions.json@. Haskell is stateless.
--
-- = Usage
--
-- @
-- import Tidepool.Session.Executor (runSessionIO, defaultSessionConfig)
-- import Tidepool.Effect.Session
--
-- main = runM $ runSessionIO config $ do
--   result <- startSession "implement/feat-x" "Implement feature X" Sonnet
--   case detectForkRequest result of
--     Just (childSlug, prompt) -> forkSession (soSessionId result) childSlug prompt
--     Nothing -> continueSession (soSessionId result) "Continue"
-- @
module Tidepool.Session.Executor
  ( -- * Executor
    runSessionIO

    -- * Configuration
  , SessionConfig(..)
  , defaultSessionConfig
  ) where

import Control.Exception (try, SomeException)
import Control.Monad.Freer (Eff, LastMember, interpret, sendM)
import Data.Aeson (Value, eitherDecodeStrict, encode)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Exit (ExitCode(..))
import System.IO (hFlush, stdout)
import System.Process (readProcessWithExitCode)

import Tidepool.Effect.Session
  ( Session(..)
  , SessionId(..)
  , SessionOutput(..)
  , SessionMetadata(..)
  )
import Tidepool.Graph.Types (ModelChoice(..))


-- ════════════════════════════════════════════════════════════════════════════
-- CONFIGURATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Configuration for Session executor.
data SessionConfig = SessionConfig
  { scMantlePath :: FilePath
    -- ^ Path to mantle binary. Defaults to "mantle" (expects it on PATH).
  , scRepoRoot :: FilePath
    -- ^ Root of the git repository. Mantle creates worktrees relative to this.
  }
  deriving (Show, Eq)

-- | Default configuration.
--
-- Uses @mantle@ on PATH and current directory as repo root.
defaultSessionConfig :: FilePath -> SessionConfig
defaultSessionConfig repoRoot = SessionConfig
  { scMantlePath = "mantle"
  , scRepoRoot = repoRoot
  }


-- ════════════════════════════════════════════════════════════════════════════
-- EXECUTOR
-- ════════════════════════════════════════════════════════════════════════════

-- | Run Session effects by spawning mantle processes.
--
-- Each effect operation spawns one @mantle session@ process:
--
-- * @StartSession@ → @mantle session start --slug X --prompt Y --model Z@
-- * @ContinueSession@ → @mantle session continue <id> --prompt Y@
-- * @ForkSession@ → @mantle session fork <id> --child-slug X --child-prompt Y@
-- * @SessionInfo@ → @mantle session info <id>@
--
-- The process runs a single turn (container lifecycle), then exits with JSON
-- on stdout. This function parses that JSON into Haskell types.
runSessionIO :: LastMember IO effs => SessionConfig -> Eff (Session ': effs) a -> Eff effs a
runSessionIO config = interpret $ \case
  StartSession slug prompt model schema ->
    sendM $ startSessionIO config slug prompt model schema

  ContinueSession sid prompt schema ->
    sendM $ continueSessionIO config sid prompt schema

  ForkSession parent childSlug childPrompt schema ->
    sendM $ forkSessionIO config parent childSlug childPrompt schema

  SessionInfo sid ->
    sendM $ sessionInfoIO config sid


-- ════════════════════════════════════════════════════════════════════════════
-- IMPLEMENTATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Start a new session.
--
-- Spawns: @mantle session start --slug <slug> --prompt <prompt> --model <model> [--json-schema <schema>]@
--
-- The slug can include a phase prefix (e.g., "implement/user-auth").
-- Mantle appends a hex suffix for uniqueness and creates the branch.
startSessionIO :: SessionConfig -> Text -> Text -> ModelChoice -> Maybe Value -> IO SessionOutput
startSessionIO config slug prompt model schema = do
  let args =
        [ "session", "start"
        , "--slug", T.unpack slug
        , "--prompt", T.unpack prompt
        , "--model", modelToArg model
        ] <> schemaArg schema

  runMantleSession config args


-- | Continue an existing session.
--
-- Spawns: @mantle session continue <session-id> --prompt <prompt> [--json-schema <schema>]@
continueSessionIO :: SessionConfig -> SessionId -> Text -> Maybe Value -> IO SessionOutput
continueSessionIO config sid prompt schema = do
  let args =
        [ "session", "continue"
        , T.unpack sid.unSessionId
        , "--prompt", T.unpack prompt
        ] <> schemaArg schema

  runMantleSession config args


-- | Fork a session (child inherits parent's history).
--
-- Spawns: @mantle session fork <parent-id> --child-slug <slug> --child-prompt <prompt> [--json-schema <schema>]@
--
-- The child slug can include a phase prefix (e.g., "subtask/handle-edge-case").
-- Mantle appends a hex suffix for uniqueness and creates the child branch.
forkSessionIO :: SessionConfig -> SessionId -> Text -> Text -> Maybe Value -> IO SessionOutput
forkSessionIO config parent childSlug childPrompt schema = do
  let args =
        [ "session", "fork"
        , T.unpack parent.unSessionId
        , "--child-slug", T.unpack childSlug
        , "--child-prompt", T.unpack childPrompt
        ] <> schemaArg schema

  runMantleSession config args


-- | Query session metadata.
--
-- Spawns: @mantle session info <session-id>@
sessionInfoIO :: SessionConfig -> SessionId -> IO (Maybe SessionMetadata)
sessionInfoIO config sid = do
  let args =
        [ "session", "info"
        , T.unpack sid.unSessionId
        ]

  result <- try @SomeException $ readProcessWithExitCode
    config.scMantlePath
    args
    ""

  case result of
    Left _ -> pure Nothing
    Right (exitCode, stdoutStr, _) ->
      case exitCode of
        ExitFailure _ -> pure Nothing  -- Session not found
        ExitSuccess ->
          case eitherDecodeStrict (BS.pack stdoutStr) of
            Left _ -> pure Nothing
            Right metadata -> pure (Just metadata)


-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Run a mantle session command and parse JSON output.
--
-- All session operations (start, continue, fork) return SessionOutput.
-- On failure, returns a SessionOutput with error details.
runMantleSession :: SessionConfig -> [String] -> IO SessionOutput
runMantleSession config args = do
  -- Log command before execution
  putStrLn $ "[SESSION] Running: " <> config.scMantlePath <> " " <> unwords args
  hFlush stdout

  startTime <- getCurrentTime
  result <- try @SomeException $ readProcessWithExitCode
    config.scMantlePath
    args
    ""
  endTime <- getCurrentTime
  let duration = realToFrac (diffUTCTime endTime startTime) :: Double

  case result of
    Left e -> do
      putStrLn $ "[SESSION] Failed to spawn mantle: " <> show e
      hFlush stdout
      pure $ errorOutput $ "Failed to spawn mantle: " <> T.pack (show e)
    Right (exitCode, stdoutStr, stderrStr) -> do
      let exitCodeInt = case exitCode of
            ExitSuccess -> 0
            ExitFailure c -> c
      putStrLn $ "[SESSION] Completed (exit: " <> show exitCodeInt <> ", " <> show (round duration :: Int) <> "s)"
      hFlush stdout
      case eitherDecodeStrict (BS.pack stdoutStr) of
        Right output -> pure output
        Left parseErr -> do
          putStrLn $ "[SESSION] Failed to parse output: " <> parseErr
          hFlush stdout
          -- Try to extract error from stderr or report parse failure
          pure $ errorOutput $ case exitCode of
            ExitSuccess -> "Failed to parse mantle output: " <> T.pack parseErr
            ExitFailure code ->
              "mantle exited with code " <> T.pack (show code) <> ": " <> T.pack stderrStr


-- | Convert ModelChoice to mantle CLI argument.
modelToArg :: ModelChoice -> String
modelToArg = \case
  Haiku -> "haiku"
  Sonnet -> "sonnet"
  Opus -> "opus"


-- | JSON schema argument if provided.
--
-- Encodes the Value to compact JSON and passes via @--json-schema@.
schemaArg :: Maybe Value -> [String]
schemaArg Nothing = []
schemaArg (Just schema) =
  ["--json-schema", BS.unpack $ LBS.toStrict $ encode schema]


-- | Create an error SessionOutput.
--
-- Used when mantle fails to spawn or returns unparseable output.
errorOutput :: Text -> SessionOutput
errorOutput errMsg = SessionOutput
  { soSessionId = SessionId ""
  , soBranch = ""
  , soWorktree = ""
  , soExitCode = -1
  , soIsError = True
  , soResultText = Nothing
  , soStructuredOutput = Nothing
  , soTotalCostUsd = 0
  , soNumTurns = 0
  , soInterrupts = []
  , soDurationSecs = 0
  , soError = Just errMsg
  }
