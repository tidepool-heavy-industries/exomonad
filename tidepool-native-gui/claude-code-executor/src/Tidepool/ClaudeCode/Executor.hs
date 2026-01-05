-- | Low-level executor for ClaudeCode - shells out to zellij-cc.
--
-- This module provides the IO functions to run zellij-cc commands.
-- The Effect module builds on this to provide an effect-based interface.
module Tidepool.ClaudeCode.Executor
  ( -- * Executor
    runClaudeCodeRequest

    -- * Helpers
  , modelToString
  ) where

import Control.Exception (try, SomeException)
import Data.Aeson (Value, eitherDecode, encode)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import System.Directory (doesFileExist)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)

import Tidepool.Graph.Types (ModelChoice(..))
import Tidepool.ClaudeCode.Config (ClaudeCodeConfig(..))
import Tidepool.ClaudeCode.Types (ClaudeCodeResult(..), ClaudeCodeError(..))


-- | Run a ClaudeCode request by shelling out to zellij-cc.
--
-- This function:
-- 1. Generates a unique output file path
-- 2. Builds zellij-cc arguments
-- 3. Runs zellij-cc (blocks until Claude Code completes)
-- 4. Parses the JSON output
--
-- @
-- result <- runClaudeCodeRequest config Sonnet (Just "/my/project") "Analyze this code" schema tools
-- case result of
--   Left err -> handleError err
--   Right ccr -> use ccr.ccrStructuredOutput
-- @
runClaudeCodeRequest
  :: ClaudeCodeConfig
  -> ModelChoice      -- ^ Model: Haiku, Sonnet, or Opus
  -> Maybe FilePath   -- ^ Working directory (Nothing inherits from runner)
  -> Text             -- ^ Rendered prompt
  -> Maybe Value      -- ^ JSON schema for structured output
  -> Maybe Text       -- ^ Tools to allow (comma-separated, e.g. "Glob,Read")
  -> IO (Either ClaudeCodeError ClaudeCodeResult)
runClaudeCodeRequest cfg model cwd prompt schema tools = do
  -- Generate unique output file
  uuid <- nextRandom
  let outputFile = cfg.ccTempDir </> ("cc-" <> T.unpack (toText uuid) <> ".json")

  -- Build arguments
  let args = buildArgs cfg model cwd prompt schema tools outputFile

  -- Run zellij-cc
  result <- try $ readProcessWithExitCode cfg.ccZellijCcPath args ""
  case result of
    Left (e :: SomeException) ->
      pure $ Left $ ClaudeCodeProcessError $
        "zellij-cc failed to start: " <> T.pack (show e)

    Right (exitCode, stdout, stderr) ->
      case exitCode of
        ExitFailure code ->
          pure $ Left $ ClaudeCodeProcessError $
            "zellij-cc exited with code " <> T.pack (show code) <> ": " <> T.pack stderr

        ExitSuccess -> do
          -- zellij-cc prints JSON result to stdout
          case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 $ T.pack stdout) of
            Left parseErr ->
              pure $ Left $ ClaudeCodeParseError $
                "Failed to parse zellij-cc output: " <> T.pack parseErr

            Right ccResult ->
              pure $ Right ccResult


-- | Build command-line arguments for zellij-cc.
buildArgs
  :: ClaudeCodeConfig
  -> ModelChoice
  -> Maybe FilePath
  -> Text
  -> Maybe Value
  -> Maybe Text
  -> FilePath
  -> [String]
buildArgs cfg model cwd prompt schema tools outputFile =
  [ "run"
  , "--session", T.unpack cfg.ccZellijSession
  , "--name", "cc-node"  -- Could make this configurable
  , "--model", modelToString model
  , "--prompt", T.unpack prompt
  , "--output-file", outputFile
  , "--timeout", show cfg.ccDefaultTimeout
  ]
  ++ cwdArgs
  ++ schemaArgs
  ++ toolsArgs
  where
    cwdArgs = case cwd of
      Nothing -> []
      Just dir -> ["--cwd", dir]

    schemaArgs = case schema of
      Nothing -> []
      Just s -> ["--json-schema", T.unpack $ TE.decodeUtf8 $ LBS.toStrict $ encode s]

    toolsArgs = case tools of
      Nothing -> []
      Just t -> ["--tools", T.unpack t]


-- | Convert ModelChoice to zellij-cc model string.
modelToString :: ModelChoice -> String
modelToString Haiku  = "haiku"
modelToString Sonnet = "sonnet"
modelToString Opus   = "opus"
