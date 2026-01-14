-- | Session effect interpreter - dockerized Claude Code orchestration.
--
-- Implements Session effect by spawning @mantle session@ processes.
-- Each operation runs one turn and returns JSON to stdout.
--
-- = Architecture
--
-- @
-- Haskell (this interpreter)
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
-- Mantle manages worktrees at @.mantle/worktrees/<branch>/@.
-- Mantle is stateless - all session data passed as CLI arguments.
--
-- = Usage
--
-- @
-- import Tidepool.Session.Interpreter (runSessionIO, defaultSessionConfig)
-- import Tidepool.Effect.Session
--
-- main = runM $ runSessionIO config $ do
--   result <- startSession "implement/feat-x" "Implement feature X" Sonnet Nothing Nothing
--   -- Continue requires all session data
--   continueSession (soSessionId result) (soWorktree result) (soBranch result) Sonnet "Continue" Nothing Nothing
-- @
module Tidepool.Session.Interpreter
  ( -- * Interpreter
    runSessionIO

    -- * Configuration
  , SessionConfig(..)
  , defaultSessionConfig

    -- * Direct IO (for testing)
  , startSessionIO
  ) where

import Control.Exception (try, SomeException)
import Control.Monad (unless)
import System.Directory (doesDirectoryExist)
import Control.Monad.Freer (Eff, LastMember, interpret, sendM)
import Data.Aeson (Value, eitherDecodeStrict, encode)
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as LBS
import Data.Char (ord)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Exit (ExitCode(..))
import System.IO (hFlush, stdout)
import System.Process (readCreateProcessWithExitCode, proc, CreateProcess(..))
import System.Random (randomRIO)
import Numeric (showHex)

import Tidepool.Effect.Session
  ( Session(..)
  , SessionId(..)
  , SessionOutput(..)
  )
import Tidepool.Graph.Types (ModelChoice(..))


-- ════════════════════════════════════════════════════════════════════════════
-- CONFIGURATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Configuration for Session interpreter.
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
-- INTERPRETER
-- ════════════════════════════════════════════════════════════════════════════

-- | Run Session effects by spawning mantle processes.
--
-- Each effect operation spawns one @mantle session@ process:
--
-- * @StartSession@ → @mantle session start --slug X --prompt Y --model Z [--decision-tools T]@
-- * @ContinueSession@ → @mantle session continue --cc-session-id X --worktree Y --branch Z --model M --prompt P [--decision-tools T]@
-- * @ForkSession@ → @mantle session fork --parent-cc-session-id X --parent-worktree Y --parent-branch Z --model M --child-slug S --child-prompt P [--decision-tools T]@
--
-- The process runs a single turn (container lifecycle), then exits with JSON
-- on stdout. This function parses that JSON into Haskell types.
--
-- Mantle is stateless - all session data is passed as CLI arguments.
runSessionIO :: LastMember IO effs => SessionConfig -> Eff (Session ': effs) a -> Eff effs a
runSessionIO config = interpret $ \case
  StartSession slug prompt model schema tools ->
    sendM $ startSessionIO config slug prompt model schema tools

  ContinueSession ccSid worktree branch model prompt schema tools ->
    sendM $ continueSessionIO config ccSid worktree branch model prompt schema tools

  ForkSession parentCcSid parentWorktree parentBranch model childSlug childPrompt schema tools ->
    sendM $ forkSessionIO config parentCcSid parentWorktree parentBranch model childSlug childPrompt schema tools


-- ════════════════════════════════════════════════════════════════════════════
-- IMPLEMENTATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Start a new session.
--
-- Spawns: @mantle session start --slug <slug> --prompt <prompt> --model <model> [--json-schema <schema>] [--decision-tools <tools>]@
--
-- The slug can include a phase prefix (e.g., "implement/user-auth").
-- Mantle appends a hex suffix for uniqueness and creates the branch.
startSessionIO :: SessionConfig -> Text -> Text -> ModelChoice -> Maybe Value -> Maybe Value -> IO SessionOutput
startSessionIO config slug prompt model schema tools = do
  let args =
        [ "session", "start"
        , "--slug", T.unpack slug
        , "--prompt", T.unpack prompt
        , "--model", modelToArg model
        ] <> schemaArg schema

  runMantleSession config args tools


-- | Continue an existing session.
--
-- Spawns: @mantle session continue --cc-session-id <id> --worktree <path> --branch <name> --model <model> --prompt <prompt> [--json-schema <schema>] [--decision-tools <tools>]@
--
-- All session data passed as args (mantle is stateless).
continueSessionIO :: SessionConfig -> SessionId -> FilePath -> Text -> ModelChoice -> Text -> Maybe Value -> Maybe Value -> IO SessionOutput
continueSessionIO config ccSid worktree branch model prompt schema tools = do
  let args =
        [ "session", "continue"
        , "--cc-session-id", T.unpack ccSid.unSessionId
        , "--worktree", worktree
        , "--branch", T.unpack branch
        , "--model", modelToArg model
        , "--prompt", T.unpack prompt
        ] <> schemaArg schema

  runMantleSession config args tools


-- | Fork a session (child inherits parent's history).
--
-- Spawns: @mantle session fork --parent-cc-session-id <id> --parent-worktree <path> --parent-branch <name> --model <model> --child-slug <slug> --child-prompt <prompt> [--json-schema <schema>] [--decision-tools <tools>]@
--
-- All parent session data passed as args (mantle is stateless).
forkSessionIO :: SessionConfig -> SessionId -> FilePath -> Text -> ModelChoice -> Text -> Text -> Maybe Value -> Maybe Value -> IO SessionOutput
forkSessionIO config parentCcSid parentWorktree parentBranch model childSlug childPrompt schema tools = do
  let args =
        [ "session", "fork"
        , "--parent-cc-session-id", T.unpack parentCcSid.unSessionId
        , "--parent-worktree", parentWorktree
        , "--parent-branch", T.unpack parentBranch
        , "--model", modelToArg model
        , "--child-slug", T.unpack childSlug
        , "--child-prompt", T.unpack childPrompt
        ] <> schemaArg schema

  runMantleSession config args tools


-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Run a mantle session command and parse JSON output.
--
-- All session operations (start, continue, fork) return SessionOutput.
-- On failure, returns a SessionOutput with error details.
--
-- Decision tools are passed via temporary file (--decision-tools-file).
-- Temp files are cleaned up by the OS and should not be manually deleted.
--
-- Inherits stderr so stream-json output is visible in real-time.
runMantleSession :: SessionConfig -> [String] -> Maybe Value -> IO SessionOutput
runMantleSession config args mtools = do
  -- Generate temp file path and write tools if provided
  toolsArgs <- case mtools of
    Nothing -> pure []
    Just tools -> do
      rand <- randomRIO (0 :: Int, 0xFFFFFF)
      let uuid = showHex rand ""
          tempFile = "/tmp/mantle-tools-" <> uuid <> ".json"
      let toolsJson = BSC.unpack $ LBS.toStrict $ encode tools
      writeFile tempFile toolsJson
      pure ["--decision-tools-file", tempFile]

  let finalArgs = args <> toolsArgs

  -- Log command before execution
  putStrLn $ "[SESSION] Running: " <> config.scMantlePath <> " " <> unwords (take 3 finalArgs)  -- Just show command type
  putStrLn $ "[SESSION] Working directory: " <> config.scRepoRoot
  -- Verify working directory exists
  cwdExists <- doesDirectoryExist config.scRepoRoot
  unless cwdExists $
    putStrLn $ "[SESSION] WARNING: Working directory does not exist!"
  hFlush stdout

  startTime <- getCurrentTime
  -- Capture stderr for debugging (previously inherited, now captured)
  result <- try @SomeException $ readCreateProcessWithExitCode
    (proc config.scMantlePath finalArgs) { cwd = Just config.scRepoRoot }
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
      -- Print stderr if non-empty (for debugging)
      unless (null stderrStr) $ do
        putStrLn $ "[SESSION] Stderr: " <> take 2000 stderrStr
      hFlush stdout
      -- Sanitize control chars in JSON strings.
      -- IMPORTANT: Use T.pack (not BSC.pack!) - BSC.pack truncates chars > 255,
      -- destroying valid Unicode. T.pack + TE.encodeUtf8 preserves Unicode properly.
      let inputText = T.pack stdoutStr
          sanitizedText = T.pack $ escapeControlChars (T.unpack inputText)
          jsonBytes = TE.encodeUtf8 sanitizedText
      case eitherDecodeStrict jsonBytes of
        Right output -> pure output
        Left parseErr -> do
          putStrLn $ "[SESSION] Failed to parse output: " <> parseErr
          putStrLn $ "[SESSION] Raw stdout length: " <> show (length stdoutStr)
          putStrLn $ "[SESSION] First 500 chars: " <> take 500 stdoutStr
          putStrLn $ "[SESSION] Last 500 chars: " <> reverse (take 500 (reverse stdoutStr))
          hFlush stdout
          -- Report parse failure (stderr was inherited, so check terminal output)
          pure $ errorOutput $ case exitCode of
            ExitSuccess -> "Failed to parse mantle output: " <> T.pack parseErr
            ExitFailure code ->
              "mantle exited with code " <> T.pack (show code) <> " (see stderr output above)"


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
  ["--json-schema", BSC.unpack $ LBS.toStrict $ encode schema]



-- | Escape control characters in JSON string values.
--
-- JSON strings should have control characters escaped, but sometimes they
-- slip through. This function finds unescaped control characters inside
-- string literals and escapes them properly.
--
-- We detect string context by tracking quotes, handling escaped quotes.
escapeControlChars :: String -> String
escapeControlChars = go False False
  where
    -- inString: are we inside a JSON string?
    -- escaped: was the previous char a backslash?
    go _ _ [] = []
    go inString escaped (c:cs)
      -- Handle escape sequences
      | escaped = c : go inString False cs
      -- Backslash starts escape
      | c == '\\' = c : go inString True cs
      -- Quote toggles string context
      | c == '"' = c : go (not inString) False cs
      -- Control char inside string: escape it
      | inString && isControlChar c = escapeChar c <> go inString False cs
      -- Everything else passes through
      | otherwise = c : go inString False cs

    -- Check if character is a control character that needs escaping
    -- (ASCII 0-31, excluding valid JSON escapes that are already single chars)
    isControlChar c = ord c < 32

    -- Escape a control character to its JSON representation
    escapeChar c = case c of
      '\n' -> "\\n"
      '\r' -> "\\r"
      '\t' -> "\\t"
      '\b' -> "\\b"
      '\f' -> "\\f"
      _    -> "\\u" <> pad4 (localShowHex (ord c) "")

    -- Pad hex to 4 digits
    pad4 s = replicate (4 - length s) '0' <> s

    localShowHex n acc
      | n < 16    = hexDigit n : acc
      | otherwise = localShowHex (n `div` 16) (hexDigit (n `mod` 16) : acc)

    hexDigit n
      | n < 10    = toEnum (fromEnum '0' + n)
      | otherwise = toEnum (fromEnum 'a' + n - 10)


-- | Create an error SessionOutput.
--
-- Used when mantle fails to spawn or returns unparseable output.
errorOutput :: Text -> SessionOutput
errorOutput errMsg = SessionOutput
  { soSessionId = SessionId ""
  , soCcSessionId = Nothing
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
  , soToolCalls = Nothing
  , soStderrOutput = Nothing
  }
