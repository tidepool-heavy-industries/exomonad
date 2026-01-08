{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Handlers for the types-first development workflow graph.
--
-- Parallel version with fork/merge handlers for concurrent agent execution.
-- Also includes sequential TDD workflow handlers.
module TypesFirstDev.Handlers
  ( typesFirstHandlers

    -- * Effect Stack
  , DevEffects
  , TDDEffects

    -- * Logging
  , logToFile

    -- * v3 Workflow (semantic descriptions)
  , stubsHandlerV3
  , forkHandlerV3
  , runWorkflowV3

    -- * TDD Workflow (sequential with validation loop)
  , typesFirstHandlersTDD
  , maxFixAttempts

    -- * Session-Aware Execution
  , runAgentWithBuildValidationV2
  , handleSessionEnd

    -- * Crosstalk (Parallel Agent Communication)
  , runAgentWithCrosstalk
  ) where

import Control.Concurrent.Async (concurrently)
import Control.Monad (when)
import Control.Monad.Freer (Eff, sendM)
import Control.Monad.Freer.Error (Error, throwError)
import Control.Monad.Freer.Reader (Reader, ask)
import Data.Maybe (fromMaybe, isJust)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO qualified as TIO
import qualified Data.Aeson as Aeson
import System.Directory (createDirectoryIfMissing, withCurrentDirectory, doesDirectoryExist)
import System.FilePath (takeDirectory, (</>))
import System.IO (hFlush, stdout)
import System.Exit (ExitCode(..))
import System.Process (callProcess, readProcessWithExitCode)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

import Tidepool.ClaudeCode.Config (ClaudeCodeConfig)
import Tidepool.ClaudeCode.Executor (runClaudeCodeRequest, runClaudeCodeRequestWithHooks)
import Tidepool.ClaudeCode.Types qualified as CC
import Tidepool.ClaudeCode.Hooks (HookCallbacks(..), defaultHookCallbacks, SessionEndContext(..), SessionEndAction(..))
import Tidepool.ClaudeCode.SessionState
  ( SessionState(..)
  , SessionExitReason(..)
  , loadSessionState
  , saveSessionState
  , clearSessionState
  , shouldResume
  )
import Tidepool.ClaudeCode.Crosstalk
  ( CrosstalkState
  , newCrosstalkStatePair
  , crosstalkPostToolUse
  , PostToolUseContext(..)
  )
import Tidepool.Effect.ClaudeCode (ClaudeCodeExec)
import Tidepool.Effects.Worktree (Worktree, createWorktree, deleteWorktree, mergeWorktree, WorktreeSpec(..), WorktreePath(..), MergeResult(..), WorktreeError(..))
import Tidepool.Graph.Generic (AsHandler)
import Tidepool.Graph.Goto (GotoChoice, To, ClaudeCodeLLMHandler(..), ClaudeCodeResult(..), gotoChoice, gotoExit)
import Tidepool.Graph.Memory (Memory, getMem, updateMem)
import Tidepool.Graph.Template (runTypedTemplate, TypedTemplate)
import Text.Parsec.Pos (SourcePos)
import Tidepool.Graph.Types (ModelChoice(..), Exit)
import Tidepool.Schema (schemaToValue)
import Tidepool.StructuredOutput (StructuredOutput(..), formatDiagnostic)

import TypesFirstDev.Context (TypesContext(..), TestsContext(..), ImplContext(..), SkeletonContext(..), StubsContext(..), TestsContextV3(..), FixContext(..))
import TypesFirstDev.Graph (TypesFirstGraph(..), TypesFirstGraphTDD(..))
import TypesFirstDev.Types
  ( StackSpec(..)
  , ProjectType(..)
  , WorkflowError(..)
  , TypeDefinitions(..)
  , ForkInput(..)
  , SkeletonGenerated(..)
  , StubsGenerated(..)
  , StubsOutput(..)
  , FunctionSemantics(..)
  , ParallelResults(..)
  , TestsResult(..)
  , ImplResult(..)
  , SessionContext(..)
  , ResumeStrategy(..)
    -- TDD workflow types
  , SkeletonState(..)
  , TestsWritten(..)
  , TestsVerified(..)
  , ImplWritten(..)
  , ValidationFailure(..)
  , FixResult(..)
  , TDDResult(..)
  )
import TypesFirstDev.Templates
  ( typesCompiled, testsCompiled, implCompiled, implSkeletonCompiled, testSkeletonCompiled
  , servantTypesCompiled, servantTestsCompiled, servantImplCompiled
  , servantImplSkeletonCompiled, servantTestSkeletonCompiled
  , servantStubsCompiled, servantTestsV3Compiled
  , fixCompiled
  )

import Tidepool.Effects.Cabal (Cabal, TestFailure(..))
import Tidepool.Cabal.Executor (parseTestOutput)


-- ════════════════════════════════════════════════════════════════════════════
-- LOGGING
-- ════════════════════════════════════════════════════════════════════════════

-- | Log a message to stdout (which gets tee'd to log file by run-test.sh).
--
-- We only print to stdout - the shell script handles file logging via tee.
-- This avoids duplicate lines in the log file.
logMsg :: String -> IO ()
logMsg msg = do
  ts <- formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" <$> getCurrentTime
  let line = "[" <> ts <> "] " <> msg
  putStrLn line
  hFlush stdout

-- | Log to a specific file (for external use).
logToFile :: FilePath -> String -> IO ()
logToFile logFile msg = do
  ts <- formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" <$> getCurrentTime
  let line = "[" <> ts <> "] " <> msg
  appendFile logFile (line <> "\n")

-- | Log entry to a phase.
logPhase :: String -> IO ()
logPhase phase = logMsg $ "═══ PHASE: " <> phase <> " ═══"

-- | Log a key-value detail.
logDetail :: String -> String -> IO ()
logDetail key val = logMsg $ "  " <> key <> ": " <> val

-- | Log a git operation and its result.
logGit :: String -> IO a -> IO a
logGit desc action = do
  logMsg $ "GIT: " <> desc
  result <- action
  logMsg $ "GIT: " <> desc <> " [OK]"
  pure result

-- | Log an error (but don't fail).
logError :: String -> IO ()
logError msg = logMsg $ "ERROR: " <> msg

-- | Log all events from a Claude Code session.
--
-- This captures the full trace of what the agent did:
-- tool calls, file operations, bash commands, reasoning.
logClaudeCodeEvents :: Text -> [CC.StreamEvent] -> IO ()
logClaudeCodeEvents agentName events = do
  logMsg $ "=== " <> T.unpack agentName <> " Agent Event Log (" <> show (length events) <> " events) ==="
  mapM_ logEvent (zip [1..] events)
  logMsg $ "=== End " <> T.unpack agentName <> " Agent Event Log ==="
  where
    logEvent :: (Int, CC.StreamEvent) -> IO ()
    logEvent (n, CC.StreamSystem sys) =
      logMsg $ "  [" <> show n <> "] SYSTEM: model=" <> T.unpack sys.seModel
               <> ", tools=" <> show sys.seTools
    logEvent (n, CC.StreamAssistant asst) =
      mapM_ (logContentBlock n) asst.aeMessage.amContent
    logEvent (n, CC.StreamUser usr) =
      case usr.ueToolUseResult of
        Just res -> logMsg $ "  [" <> show n <> "] TOOL_RESULT: " <> truncate' 200 (T.unpack res)
        Nothing -> case usr.ueMessage of
          Just msg -> mapM_ (logContentBlock n) msg.umContent
          Nothing -> pure ()
    logEvent (n, CC.StreamResult res) =
      logMsg $ "  [" <> show n <> "] RESULT: subtype=" <> T.unpack res.reSubtype
               <> ", is_error=" <> show res.reIsError
               <> ", turns=" <> show res.reNumTurns
               <> ", cost=$" <> maybe "?" show res.reTotalCostUsd

    logContentBlock :: Int -> CC.ContentBlock -> IO ()
    logContentBlock n (CC.TextBlock txt) =
      logMsg $ "  [" <> show n <> "] TEXT: " <> truncate' 300 (T.unpack txt)
    logContentBlock n (CC.ToolUseBlock{..}) =
      logMsg $ "  [" <> show n <> "] TOOL_USE: " <> T.unpack tubName
               <> " | " <> truncate' 200 (show tubInput)
    logContentBlock n (CC.ToolResultBlock{..}) =
      logMsg $ "  [" <> show n <> "] TOOL_RESULT_BLOCK: " <> truncate' 200 (T.unpack trbContent)
               <> if trbIsError then " [ERROR]" else ""

    truncate' :: Int -> String -> String
    truncate' maxLen s
      | length s <= maxLen = s
      | otherwise = take maxLen s <> "..."


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT STACK
-- ════════════════════════════════════════════════════════════════════════════

-- | Effect stack for the types-first workflow (parallel version).
--
-- - Error WorkflowError: Workflow failures (compile errors, agent failures, etc.)
-- - Memory SessionContext: Persistent session state (session ID, project path)
-- - Reader ClaudeCodeConfig: Config for spawning Claude Code subprocesses
-- - Reader StackSpec: The original specification (for routing)
-- - ClaudeCodeExec: For spawning Claude Code subprocesses
-- - Worktree: For managing git worktrees (parallel isolation)
-- - IO: For system operations
type DevEffects = '[Error WorkflowError, Memory SessionContext, Reader ClaudeCodeConfig, Reader StackSpec, ClaudeCodeExec, Worktree, IO]


-- ════════════════════════════════════════════════════════════════════════════
-- HANDLERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Handlers for TypesFirstGraph (parallel version).
--
-- Takes StackSpec to dynamically select templates based on ProjectType.
--
-- - types: Writes type signatures, routes to skeleton
-- - skeleton: Generates impl/test skeleton files
-- - fork: Creates worktrees, spawns parallel agents
-- - merge: Collects results, cleans up worktrees
typesFirstHandlers :: StackSpec -> TypesFirstGraph (AsHandler DevEffects)
typesFirstHandlers spec = TypesFirstGraph
  { entry = Proxy @StackSpec

    -- Types handler: Writes type signatures, routes to skeleton
    -- Template selection based on project type
  , types = ClaudeCodeLLMHandler @'Sonnet @'Nothing
      Nothing                              -- no system template
      (selectTypesTemplate spec.ssProjectType)  -- dynamic template selection
      buildTypesContext                    -- before: builds context
      routeToSkeleton                      -- after: routes to skeleton node

    -- Skeleton handler: Generates impl/test skeleton files
  , skeleton = skeletonHandler

    -- Fork handler: Creates worktrees and spawns parallel agents
  , fork = forkHandler

    -- Merge handler: Cleans up worktrees, exits with results
  , merge = mergeHandler

  , exit = Proxy @ParallelResults
  }

-- | Select the types template based on project type.
selectTypesTemplate :: ProjectType -> TypedTemplate TypesContext SourcePos
selectTypesTemplate PureLibrary   = typesCompiled
selectTypesTemplate ServantServer = servantTypesCompiled
selectTypesTemplate CLIApp        = typesCompiled  -- TODO: CLI-specific template


-- ════════════════════════════════════════════════════════════════════════════
-- TYPES NODE
-- ════════════════════════════════════════════════════════════════════════════

-- | Build context for the types template.
buildTypesContext
  :: StackSpec
  -> Eff DevEffects TypesContext
buildTypesContext spec = pure TypesContext
  { moduleName = spec.ssModuleName
  , description = spec.ssDescription
  , acceptanceCriteria = spec.ssAcceptanceCriteria
  }

-- | Route after types agent completes.
--
-- Routes to skeleton node for file generation.
-- Also stores the session ID in Memory for use by subsequent handlers.
routeToSkeleton
  :: ClaudeCodeResult TypeDefinitions
  -> Eff DevEffects (GotoChoice '[To "skeleton" ForkInput])
routeToSkeleton result = do
  spec <- ask @StackSpec
  -- Store session context in Memory (accessible by all subsequent handlers)
  updateMem @SessionContext $ \ctx -> ctx
    { scSessionId = fromMaybe "" result.ccrSessionId
    , scProjectPath = spec.ssProjectPath
    }
  pure $ gotoChoice @"skeleton" ForkInput
    { fiTypeDefs = result.ccrParsedOutput
    , fiProjectPath = spec.ssProjectPath
    , fiModuleName = spec.ssModuleName
    }


-- ════════════════════════════════════════════════════════════════════════════
-- SKELETON NODE
-- ════════════════════════════════════════════════════════════════════════════

-- | Skeleton handler generates impl/test skeleton files from TypeDefinitions.
--
-- 1. Renders impl-skeleton.jinja → src/<ModulePath>.hs
-- 2. Renders test-skeleton.jinja → test/Main.hs
-- 3. Outputs SkeletonGenerated with paths
skeletonHandler
  :: ForkInput
  -> Eff DevEffects (GotoChoice '[To "fork" SkeletonGenerated])
skeletonHandler input = do
  spec <- ask @StackSpec
  let projectPath = input.fiProjectPath
      modulePath = T.unpack $ T.replace "." "/" input.fiModuleName
      implPath = projectPath <> "/src/" <> modulePath <> ".hs"
      testPath = projectPath <> "/test/Main.hs"
      gitignorePath = projectPath <> "/.gitignore"

      -- Build skeleton context from type definitions
      skeletonCtx = SkeletonContext
        { moduleName = input.fiModuleName
        , typeName = input.fiTypeDefs.tdTypeName
        , dataType = input.fiTypeDefs.tdDataType
        , signatures = input.fiTypeDefs.tdSignatures
        , testPriorities = input.fiTypeDefs.tdTestPriorities
        , imports = input.fiTypeDefs.tdImports
        }

      -- Select templates based on project type
      (implTemplate, testTemplate) = case spec.ssProjectType of
        PureLibrary   -> (implSkeletonCompiled, testSkeletonCompiled)
        ServantServer -> (servantImplSkeletonCompiled, servantTestSkeletonCompiled)
        CLIApp        -> (implSkeletonCompiled, testSkeletonCompiled)  -- TODO: CLI templates

      -- Render templates
      implCode = runTypedTemplate skeletonCtx implTemplate
      testCode = runTypedTemplate skeletonCtx testTemplate

  -- Write skeleton files
  sendM $ do
    logPhase "SKELETON GENERATION"
    logDetail "projectPath" projectPath
    logDetail "implPath" implPath
    logDetail "testPath" testPath
    logDetail "typeName" (T.unpack input.fiTypeDefs.tdTypeName)
    logDetail "numSignatures" (show $ length input.fiTypeDefs.tdSignatures)

    createDirectoryIfMissing True (takeDirectory implPath)
    createDirectoryIfMissing True (takeDirectory testPath)
    TIO.writeFile implPath implCode
    logMsg $ "Wrote impl skeleton: " <> implPath
    TIO.writeFile testPath testCode
    logMsg $ "Wrote test skeleton: " <> testPath

    -- Write .gitignore to prevent build artifacts from being committed
    writeFile gitignorePath "dist-newstyle/\n*.hi\n*.o\n*.dyn_hi\n*.dyn_o\n"
    logMsg $ "Wrote .gitignore: " <> gitignorePath

  -- Validate skeleton compiles before committing
  sendM $ logMsg "Validating skeleton compiles..."
  (exitCode, buildOut, buildErr) <- sendM $ withCurrentDirectory projectPath $
    readProcessWithExitCode "cabal" ["build", "-v0", "all"] ""

  case exitCode of
    ExitSuccess -> sendM $ logMsg "Skeleton validation passed"
    ExitFailure code -> do
      sendM $ do
        logError $ "Skeleton failed to compile (exit code " <> show code <> "):"
        logError buildErr
        logError buildOut
      throwError $ SkeletonCompileFailed $ T.pack (buildErr <> "\n" <> buildOut)

  -- Commit skeletons so worktrees will have them
  sendM $ do
    logGit "Committing skeleton files" $ withCurrentDirectory projectPath $ do
      callProcess "git" ["add", implPath, testPath, gitignorePath]
      callProcess "git" ["commit", "-m", "Add skeleton files for " <> T.unpack input.fiModuleName]
    logMsg "Skeleton generation complete"

  -- Route to fork with generated paths (session ID is in Memory)
  pure $ gotoChoice @"fork" SkeletonGenerated
    { sgImplPath = implPath
    , sgTestPath = testPath
    , sgTypeDefs = input.fiTypeDefs
    , sgProjectPath = projectPath
    , sgModuleName = input.fiModuleName
    }


-- ════════════════════════════════════════════════════════════════════════════
-- AGENT VALIDATION HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Maximum retries for agent build validation.
maxAgentRetries :: Int
maxAgentRetries = 3

-- | Run agent with build validation loop.
--
-- After agent completes, runs `cabal build` to verify the code compiles.
-- If build fails, retries the agent with the error message appended to prompt.
--
-- Session parameters:
-- - resumeSession: Session ID to resume (for multi-turn or forking from parent)
-- - forkSession: If True, fork the session (read-only, doesn't modify original)
-- | Run a Claude Code agent with automatic retry on build failure.
--
-- == Design Note: Local Recursion vs Graph Self-Loop
--
-- This uses local @go@ recursion rather than the graph DSL's @Goto Self@ pattern.
-- Both are valid approaches with different trade-offs:
--
-- * **Local recursion** (used here): The retry logic is encapsulated within the
--   function and not visible in the graph topology. Appropriate when:
--   - The retry is an implementation detail, not a workflow step
--   - The function is called within parallel execution (@concurrently@)
--   - You want simple, self-contained retry logic
--
-- * **Graph self-loop** (@Goto Self@): The retry is a visible node transition,
--   handled by @dispatchGotoWithSelf@. Appropriate when:
--   - Retry attempts should be observable in graph tracing/telemetry
--   - The retry is a conceptually separate workflow step
--   - You need to compose with other graph dispatch features
--
-- Here, retry is an implementation detail of "spawn agent with build validation"
-- rather than a workflow-level concept, so local recursion is the right choice.
runAgentWithBuildValidation
  :: ClaudeCodeConfig
  -> WorktreePath
  -> Text  -- ^ Base prompt
  -> Maybe Aeson.Value  -- ^ JSON schema
  -> Text  -- ^ Agent name (for logging)
  -> Maybe Text  -- ^ Resume session ID (for multi-turn or forking)
  -> Bool  -- ^ Fork session (True for parallel agents forking from parent)
  -> IO (Either CC.ClaudeCodeError CC.ClaudeCodeResult)
runAgentWithBuildValidation ccConfig (WorktreePath wtPath) basePrompt schema agentName resumeSession forkSession = go 0 Nothing Nothing
  where
    -- go tracks: attempt count, last build error, agent's own session ID (for retries)
    go :: Int -> Maybe Text -> Maybe Text -> IO (Either CC.ClaudeCodeError CC.ClaudeCodeResult)
    go attempt lastBuildError agentSessionId
      | attempt >= maxAgentRetries = do
          logError $ T.unpack agentName <> " agent: max retries (" <> show maxAgentRetries <> ") exceeded"
          case lastBuildError of
            Just err -> pure $ Left $ CC.ClaudeCodeExecutionError $ "Build failed after " <> T.pack (show maxAgentRetries) <> " attempts: " <> err
            Nothing -> pure $ Left $ CC.ClaudeCodeExecutionError "Max retries exceeded with no error"
      | otherwise = do
          -- Build prompt with retry context if we have a previous error
          let fullPrompt = case lastBuildError of
                Nothing -> basePrompt
                Just buildErr -> basePrompt <> "\n\n## PREVIOUS ATTEMPT FAILED\n\nYour code did not compile. The build failed with:\n```\n" <> buildErr <> "\n```\n\nPlease fix these issues and try again."

          logMsg $ T.unpack agentName <> " agent attempt " <> show (attempt + 1) <> "/" <> show maxAgentRetries

          -- Run agent with session parameters
          -- Attempt 0: Fork from parent session (if provided)
          -- Retries: Resume agent's own session to maintain context from previous attempt
          let (thisResumeSession, thisForkSession) = case agentSessionId of
                Nothing -> (resumeSession, forkSession)  -- First attempt: use parent settings
                Just sid -> (Just sid, False)            -- Retry: resume agent's own session
          result <- runClaudeCodeRequest ccConfig Sonnet (Just wtPath) fullPrompt schema Nothing thisResumeSession thisForkSession

          case result of
            Left err -> pure $ Left err
            Right ccResult -> do
              -- Capture agent's session ID for potential retries
              let newAgentSessionId = Just $ CC.ccrSessionId ccResult
              -- Validate build
              logMsg $ T.unpack agentName <> " agent completed, validating build..."
              (exitCode, buildOut, buildErr) <- withCurrentDirectory wtPath $
                readProcessWithExitCode "cabal" ["build", "-v0"] ""
              case exitCode of
                ExitSuccess -> do
                  logMsg $ T.unpack agentName <> " agent: build passed"
                  pure $ Right ccResult
                ExitFailure code -> do
                  let errorMsg = T.pack $ "Exit code " <> show code <> ":\n" <> buildErr <> buildOut
                  logError $ T.unpack agentName <> " agent: build failed, retrying..."
                  logError $ T.unpack errorMsg
                  go (attempt + 1) (Just errorMsg) newAgentSessionId


-- | Run stubs agent with build validation (v3 workflow).
--
-- Like runAgentWithBuildValidation but works in a FilePath (not WorktreePath).
-- Used for the initial stubs phase before worktrees are created.
--
-- Session parameters:
-- - resumeSession: Session ID to resume (for multi-turn)
-- - forkSession: If True, fork the session (read-only)
runStubsAgentWithBuildValidation
  :: ClaudeCodeConfig
  -> FilePath  -- ^ Project path
  -> Text  -- ^ Base prompt
  -> Maybe Aeson.Value  -- ^ JSON schema
  -> Maybe Text  -- ^ Resume session ID
  -> Bool  -- ^ Fork session
  -> IO (Either CC.ClaudeCodeError CC.ClaudeCodeResult)
runStubsAgentWithBuildValidation ccConfig projectPath basePrompt schema resumeSession forkSession = go 0 Nothing Nothing
  where
    -- go tracks: attempt count, last build error, agent's own session ID (for retries)
    go :: Int -> Maybe Text -> Maybe Text -> IO (Either CC.ClaudeCodeError CC.ClaudeCodeResult)
    go attempt lastBuildError agentSessionId
      | attempt >= maxAgentRetries = do
          logError $ "Stubs agent: max retries (" <> show maxAgentRetries <> ") exceeded"
          case lastBuildError of
            Just err -> pure $ Left $ CC.ClaudeCodeExecutionError $ "Build failed after " <> T.pack (show maxAgentRetries) <> " attempts: " <> err
            Nothing -> pure $ Left $ CC.ClaudeCodeExecutionError "Max retries exceeded with no error"
      | otherwise = do
          -- Build prompt with retry context if we have a previous error
          let fullPrompt = case lastBuildError of
                Nothing -> basePrompt
                Just buildErr -> basePrompt <> "\n\n## PREVIOUS ATTEMPT FAILED\n\nYour code did not compile. The build failed with:\n```\n" <> buildErr <> "\n```\n\nPlease fix the compilation errors and try again."

          logMsg $ "Stubs agent attempt " <> show (attempt + 1) <> "/" <> show maxAgentRetries

          -- Run agent in the project directory with session parameters
          -- Attempt 0: Use parent settings; Retries: resume agent's own session
          let (thisResumeSession, thisForkSession) = case agentSessionId of
                Nothing -> (resumeSession, forkSession)  -- First attempt
                Just sid -> (Just sid, False)            -- Retry: resume agent's session
          result <- runClaudeCodeRequest ccConfig Sonnet (Just projectPath) fullPrompt schema Nothing thisResumeSession thisForkSession

          case result of
            Left err -> pure $ Left err
            Right ccResult -> do
              -- Capture agent's session ID for potential retries
              let newAgentSessionId = Just $ CC.ccrSessionId ccResult
              -- Validate build
              logMsg "Stubs agent completed, validating build..."
              (exitCode, buildOut, buildErr) <- withCurrentDirectory projectPath $
                readProcessWithExitCode "cabal" ["build", "-v0"] ""
              case exitCode of
                ExitSuccess -> do
                  logMsg "Stubs agent: build passed"
                  pure $ Right ccResult
                ExitFailure code -> do
                  let errorMsg = T.pack $ "Exit code " <> show code <> ":\n" <> buildErr <> buildOut
                  logError $ "Stubs agent: build failed, retrying..."
                  logError $ T.unpack errorMsg
                  go (attempt + 1) (Just errorMsg) newAgentSessionId


-- ════════════════════════════════════════════════════════════════════════════
-- SESSION-AWARE AGENT EXECUTION (v2)
-- ════════════════════════════════════════════════════════════════════════════

-- | Run agent with build validation and session continuity support.
--
-- Enhanced version of 'runAgentWithBuildValidation' that:
--
-- 1. Loads existing session state from disk
-- 2. Uses the configured 'ResumeStrategy' to decide whether to resume
-- 3. Registers a typed 'SessionEnd' callback to persist state
-- 4. Passes session ID to Claude Code when resuming
--
-- Session state is stored per-worktree at @.claude/tidepool-session.json@.
--
-- == Resume Strategies
--
-- * 'AlwaysFresh': Always start a new session (current default behavior)
-- * 'ResumeOnRetry': Resume only when retrying after build failure
-- * 'AlwaysResume': Resume if session state exists
-- * 'SmartResume': Resume based on previous exit reason
runAgentWithBuildValidationV2
  :: ClaudeCodeConfig
  -> FilePath        -- ^ Working directory (worktree path)
  -> Text            -- ^ Base prompt
  -> Maybe Aeson.Value -- ^ JSON schema for structured output
  -> Text            -- ^ Agent name (for logging and state)
  -> ResumeStrategy  -- ^ Resume strategy
  -> IO (Either CC.ClaudeCodeError CC.ClaudeCodeResult)
runAgentWithBuildValidationV2 ccConfig wtPath basePrompt schema agentName strategy = do
  -- Load existing session state
  existingState <- loadSessionState wtPath

  -- Determine initial session ID based on strategy
  let initialSessionId = case strategy of
        AlwaysFresh -> Nothing
        AlwaysResume ->
          existingState >>= \st -> Just st.ssSessionId
        SmartResume ->
          existingState >>= \st ->
            case st.ssLastExitReason of
              Just reason | shouldResume reason -> Just st.ssSessionId
              _ -> Nothing
        ResumeOnRetry ->
          -- Only resume after first attempt (handled in the loop)
          Nothing

  logMsg $ T.unpack agentName <> " agent: strategy=" <> show strategy
  case initialSessionId of
    Nothing -> logMsg $ T.unpack agentName <> " agent: starting fresh session"
    Just sid -> logMsg $ T.unpack agentName <> " agent: resuming session " <> T.unpack sid

  go 0 Nothing initialSessionId
  where
    go :: Int -> Maybe Text -> Maybe Text -> IO (Either CC.ClaudeCodeError CC.ClaudeCodeResult)
    go attempt lastBuildError sessionId
      | attempt >= maxAgentRetries = do
          logError $ T.unpack agentName <> " agent: max retries (" <> show maxAgentRetries <> ") exceeded"
          case lastBuildError of
            Just err -> pure $ Left $ CC.ClaudeCodeExecutionError $
              "Build failed after " <> T.pack (show maxAgentRetries) <> " attempts: " <> err
            Nothing -> pure $ Left $ CC.ClaudeCodeExecutionError "Max retries exceeded with no error"
      | otherwise = do
          -- Build prompt with retry context if we have a previous error
          let fullPrompt = case lastBuildError of
                Nothing -> basePrompt
                Just buildErr -> basePrompt <> "\n\n## PREVIOUS ATTEMPT FAILED\n\nYour code did not compile. The build failed with:\n```\n" <> buildErr <> "\n```\n\nPlease fix these issues and try again."

          logMsg $ T.unpack agentName <> " agent attempt " <> show (attempt + 1) <> "/" <> show maxAgentRetries

          -- Create callbacks with typed SessionEnd handler
          let callbacks = defaultHookCallbacks
                { hcOnSessionEndTyped = handleSessionEnd wtPath agentName
                }

          -- Run agent with hooks (using Sonnet for capability)
          result <- runClaudeCodeRequestWithHooks
            ccConfig
            callbacks
            Sonnet
            (Just wtPath)
            fullPrompt
            schema
            Nothing      -- tools
            sessionId    -- resume session ID
            False        -- don't fork session

          case result of
            Left err -> pure $ Left err
            Right ccResult -> do
              -- Validate build
              logMsg $ T.unpack agentName <> " agent completed, validating build..."
              (exitCode, buildOut, buildErr) <- withCurrentDirectory wtPath $
                readProcessWithExitCode "cabal" ["build", "-v0"] ""
              case exitCode of
                ExitSuccess -> do
                  logMsg $ T.unpack agentName <> " agent: build passed"
                  pure $ Right ccResult
                ExitFailure code -> do
                  let errorMsg = T.pack $ "Exit code " <> show code <> ":\n" <> buildErr <> buildOut
                  logError $ T.unpack agentName <> " agent: build failed, retrying..."
                  logError $ T.unpack errorMsg

                  -- Determine next session ID based on strategy
                  let nextSessionId = case strategy of
                        ResumeOnRetry -> Just $ CC.ccrSessionId ccResult
                        SmartResume -> Just $ CC.ccrSessionId ccResult
                        _ -> Nothing

                  go (attempt + 1) (Just errorMsg) nextSessionId

-- | Handle SessionEnd hook - persists session state.
handleSessionEnd :: FilePath -> Text -> SessionEndContext -> IO SessionEndAction
handleSessionEnd wtPath agentName ctx = do
  now <- getCurrentTime
  let state = SessionState
        { ssSessionId = ctx.secSessionId
        , ssLastExitReason = Just ctx.secReason
        , ssTimestamp = now
        , ssAgentName = agentName
        , ssRetryCount = 0
        , ssTranscriptPath = Just ctx.secTranscriptPath
        }

  logMsg $ T.unpack agentName <> " session ended: " <> show ctx.secReason

  case ctx.secReason of
    NormalExit -> do
      saveSessionState wtPath state
      pure $ CommitWork "Auto-commit: Agent completed successfully"

    ClearExit -> do
      saveSessionState wtPath state
      pure PreserveForResume

    LogoutExit -> do
      clearSessionState wtPath
      pure Cleanup

    PromptInputExit -> do
      clearSessionState wtPath
      pure Cleanup

    UnknownExit _ -> do
      saveSessionState wtPath state
      pure NoAction


-- | Run agent with build validation and crosstalk support.
--
-- Combines build validation with crosstalk for parallel agent communication.
-- After each tool use (Write, Edit, Bash), checks if the other agent has
-- committed new work and injects context about those commits.
--
-- This enables parallel agents to stay aware of each other's progress
-- without explicit coordination.
--
-- NOTE: Session resumption on retry is not supported with crosstalk.
-- Each retry starts a fresh session. This is intentional: crosstalk agents
-- fork from a parent session and we want each retry attempt to have the
-- same starting context (the parent's state at fork time).
runAgentWithCrosstalk
  :: ClaudeCodeConfig
  -> FilePath          -- ^ Working directory (worktree path)
  -> Text              -- ^ Base prompt
  -> Maybe Aeson.Value -- ^ JSON schema for structured output
  -> Text              -- ^ Agent name (for logging)
  -> Maybe Text        -- ^ Resume session ID (for forking from parent)
  -> Bool              -- ^ Fork session (True for parallel agents forking from parent)
  -> CrosstalkState    -- ^ Crosstalk state for tracking other agent
  -> IO (Either CC.ClaudeCodeError CC.ClaudeCodeResult)
runAgentWithCrosstalk ccConfig wtPath basePrompt schema agentName resumeSession forkSession crosstalk = go 0 Nothing
  where
    go :: Int -> Maybe Text -> IO (Either CC.ClaudeCodeError CC.ClaudeCodeResult)
    go attempt lastBuildError
      | attempt >= maxAgentRetries = do
          logError $ T.unpack agentName <> " agent: max retries (" <> show maxAgentRetries <> ") exceeded"
          case lastBuildError of
            Just err -> pure $ Left $ CC.ClaudeCodeExecutionError $
              "Build failed after " <> T.pack (show maxAgentRetries) <> " attempts: " <> err
            Nothing -> pure $ Left $ CC.ClaudeCodeExecutionError "Max retries exceeded with no error"
      | otherwise = do
          -- Build prompt with retry context if we have a previous error
          let fullPrompt = case lastBuildError of
                Nothing -> basePrompt
                Just buildErr -> basePrompt <> "\n\n## PREVIOUS ATTEMPT FAILED\n\nYour code did not compile. The build failed with:\n```\n" <> buildErr <> "\n```\n\nPlease fix these issues and try again."

          logMsg $ T.unpack agentName <> " agent attempt " <> show (attempt + 1) <> "/" <> show maxAgentRetries

          -- Create callbacks with crosstalk support
          -- Both SessionEnd (for state persistence) and PostToolUse (for crosstalk)
          let callbacks = defaultHookCallbacks
                { hcOnSessionEndTyped = handleSessionEnd wtPath agentName
                , hcOnPostToolUseTyped = \ctx ->
                    crosstalkPostToolUse crosstalk ctx.ptcToolName ctx.ptcToolInput ctx.ptcToolResponse
                }

          -- Run agent with hooks (using Sonnet for capability)
          result <- runClaudeCodeRequestWithHooks
            ccConfig
            callbacks
            Sonnet
            (Just wtPath)
            fullPrompt
            schema
            Nothing        -- tools
            resumeSession  -- session ID for forking from parent
            forkSession    -- fork session flag

          case result of
            Left err -> pure $ Left err
            Right ccResult -> do
              -- Validate build
              logMsg $ T.unpack agentName <> " agent completed, validating build..."
              (exitCode, buildOut, buildErr) <- withCurrentDirectory wtPath $
                readProcessWithExitCode "cabal" ["build", "-v0"] ""
              case exitCode of
                ExitSuccess -> do
                  logMsg $ T.unpack agentName <> " agent: build passed"
                  pure $ Right ccResult
                ExitFailure code -> do
                  let errorMsg = T.pack $ "Exit code " <> show code <> ":\n" <> buildErr <> buildOut
                  logError $ T.unpack agentName <> " agent: build failed, retrying..."
                  logError $ T.unpack errorMsg
                  go (attempt + 1) (Just errorMsg)


-- ════════════════════════════════════════════════════════════════════════════
-- v3 STUBS HANDLER
-- ════════════════════════════════════════════════════════════════════════════

-- | Stubs handler for v3 workflow.
--
-- Runs Claude Code to write actual .hs files with undefined implementations.
-- Returns semantic descriptions that drive test generation.
--
-- This replaces both types + skeleton handlers from v2.
stubsHandlerV3
  :: StackSpec
  -> ClaudeCodeConfig
  -> IO (Either CC.ClaudeCodeError StubsGenerated)
stubsHandlerV3 spec config = do
  logPhase "v3 STUBS - Writing implementation stubs"
  logDetail "projectPath" spec.ssProjectPath
  logDetail "moduleName" (T.unpack spec.ssModuleName)

  let projectPath = spec.ssProjectPath
      modulePath = T.replace "." "/" spec.ssModuleName
      implPath = projectPath <> "/src/" <> T.unpack modulePath <> ".hs"
      testPath = projectPath <> "/test/Main.hs"

      -- Build stubs context for the prompt
      stubsCtx = StubsContext
        { moduleName = spec.ssModuleName
        , modulePath = modulePath
        , description = spec.ssDescription
        , acceptanceCriteria = spec.ssAcceptanceCriteria
        }

      -- Render the stubs prompt
      stubsPrompt = runTypedTemplate stubsCtx servantStubsCompiled

      -- Build schema for StubsOutput
      stubsSchema = Just $ schemaToValue (structuredSchema @StubsOutput)

  logDetail "implPath" implPath
  logDetail "stubsPromptLength" (show $ T.length stubsPrompt)

  -- Run stubs agent with build validation (no parent session to fork from)
  result <- runStubsAgentWithBuildValidation config projectPath stubsPrompt stubsSchema Nothing False

  case result of
    Left err -> do
      logError $ "Stubs agent failed: " <> show err
      pure $ Left err
    Right ccResult -> do
      -- Parse the structured output
      case CC.ccrStructuredOutput ccResult of
        Nothing -> do
          logError "Stubs agent returned no structured output"
          pure $ Left $ CC.ClaudeCodeExecutionError "No structured output"
        Just val -> case Aeson.fromJSON val of
          Aeson.Error msg -> do
            logError $ "Failed to parse StubsOutput: " <> msg
            pure $ Left $ CC.ClaudeCodeExecutionError $ "JSON parse error: " <> T.pack msg
          Aeson.Success (stubsOutput :: StubsOutput) -> do
            logMsg "Stubs agent completed successfully"
            logDetail "numFunctions" (show $ length stubsOutput.soFunctions)
            logDetail "commitMessage" (T.unpack stubsOutput.soCommitMessage)

            -- Log semantic descriptions
            logMsg "=== Semantic Descriptions ==="
            mapM_ logSemantics stubsOutput.soFunctions

            -- Commit the stubs (so worktrees will have them)
            logGit "Committing stubs" $ withCurrentDirectory projectPath $ do
              -- Write .gitignore if needed
              let gitignorePath = projectPath <> "/.gitignore"
              writeFile gitignorePath "dist-newstyle/\n*.hi\n*.o\n*.dyn_hi\n*.dyn_o\n"

              callProcess "git" ["add", implPath, testPath, gitignorePath]
              callProcess "git" ["commit", "-m", T.unpack stubsOutput.soCommitMessage]

            pure $ Right StubsGenerated
              { stgImplPath = implPath
              , stgTestPath = testPath
              , stgSemantics = stubsOutput.soFunctions
              , stgDataType = stubsOutput.soDataType
              , stgProjectPath = projectPath
              , stgModuleName = spec.ssModuleName
              , stgSessionId = CC.ccrSessionId ccResult
              }
  where
    logSemantics :: FunctionSemantics -> IO ()
    logSemantics sem = do
      logMsg $ "  " <> T.unpack sem.fsmName <> " :: " <> T.unpack sem.fsmSignature
      logDetail "    behavior" (T.unpack sem.fsmBehavior)
      logDetail "    examples" (show $ length sem.fsmExamples)


-- ════════════════════════════════════════════════════════════════════════════
-- FORK NODE
-- ════════════════════════════════════════════════════════════════════════════

-- | Fork handler spawns parallel agents in separate worktrees.
--
-- Creates two worktrees, spawns tests and impl agents concurrently,
-- then collects results for the merge node.
--
-- == Design Note: Parallelism Pattern
--
-- The graph DSL doesn't currently express parallel fan-out. Instead, we use
-- a 'LogicNode' with @sendM $ concurrently@ to spawn parallel IO operations.
--
-- @
-- Graph topology:  fork ──► merge (linear transition)
-- Actual execution: fork ──┬──► tests agent ──┐
--                          └──► impl agent  ──┴──► merge
-- @
--
-- This is the blessed pattern for parallel execution within graphs:
--
-- 1. **Graph shows workflow steps** - fork and merge are distinct phases
-- 2. **Parallelism is implementation detail** - hidden in handler, not graph
-- 3. **Results collected before transition** - merge receives complete results
--
-- A future DSL enhancement could add explicit 'ForkNode' and 'JoinNode'
-- annotations, but for now @LogicNode + concurrently@ works well.
forkHandler
  :: SkeletonGenerated
  -> Eff DevEffects (GotoChoice '[To "merge" ParallelResults])
forkHandler input = do
  config <- ask @ClaudeCodeConfig
  spec <- ask @StackSpec
  sessionCtx <- getMem @SessionContext  -- Read session from Memory

  sendM $ logPhase "FORK - Creating worktrees"

  -- Create worktrees for isolation
  testsWtResult <- createWorktree (WorktreeSpec "tests" Nothing)
  implWtResult <- createWorktree (WorktreeSpec "impl" Nothing)

  -- Handle worktree creation errors
  (testsWt, implWt) <- case (testsWtResult, implWtResult) of
    (Right t, Right i) -> pure (t, i)
    (Left err, _) -> do
      sendM $ logError $ "Failed to create tests worktree: " <> show err
      throwError $ WorktreeCreationFailed $ T.pack $ show err
    (_, Left err) -> do
      sendM $ logError $ "Failed to create impl worktree: " <> show err
      throwError $ WorktreeCreationFailed $ T.pack $ show err

  sendM $ do
    logDetail "testsWorktree" testsWt.unWorktreePath
    logDetail "implWorktree" implWt.unWorktreePath

  -- Copy build artifacts from parent for faster builds
  sendM $ do
    let parentDistNewstyle = input.sgProjectPath </> "dist-newstyle"
    parentHasArtifacts <- doesDirectoryExist parentDistNewstyle
    when parentHasArtifacts $ do
      logMsg "Copying build artifacts to worktrees..."
      let testsDistNewstyle = testsWt.unWorktreePath </> "dist-newstyle"
          implDistNewstyle = implWt.unWorktreePath </> "dist-newstyle"
      -- Use cp -r for the copy (more portable than rsync)
      callProcess "cp" ["-r", parentDistNewstyle, testsDistNewstyle]
      callProcess "cp" ["-r", parentDistNewstyle, implDistNewstyle]
      logMsg "Build artifacts copied"

  -- Build contexts for each agent
  let testsCtx = TestsContext
        { moduleName = input.sgModuleName
        , dataType = input.sgTypeDefs.tdDataType
        , signatures = input.sgTypeDefs.tdSignatures
        , testPriorities = input.sgTypeDefs.tdTestPriorities
        }
      implCtx = ImplContext
        { moduleName = input.sgModuleName
        , dataType = input.sgTypeDefs.tdDataType
        , signatures = input.sgTypeDefs.tdSignatures
        }

      -- Select prompt templates based on project type
      (testsTemplate, implTemplate) = case spec.ssProjectType of
        PureLibrary   -> (testsCompiled, implCompiled)
        ServantServer -> (servantTestsCompiled, servantImplCompiled)
        CLIApp        -> (testsCompiled, implCompiled)  -- TODO: CLI templates

      -- Render templates
      testsPrompt = runTypedTemplate testsCtx testsTemplate
      implPrompt = runTypedTemplate implCtx implTemplate

      -- Build schemas (using new result types - no code, just metadata)
      testsSchema = Just $ schemaToValue (structuredSchema @TestsResult)
      implSchema = Just $ schemaToValue (structuredSchema @ImplResult)

  sendM $ do
    logPhase "FORK - Spawning parallel agents"
    logDetail "testsPromptLength" (show $ T.length testsPrompt)
    logDetail "implPromptLength" (show $ T.length implPrompt)

  -- Run both agents in parallel at IO level (freer-simple doesn't support parallel Eff)
  -- Each agent runs with build validation - if build fails, retries with error context
  -- Both agents fork from the types agent's session to share context
  -- If parent session ID is empty, start fresh sessions instead of trying to fork
  let parentSessionId = if T.null sessionCtx.scSessionId then Nothing else Just sessionCtx.scSessionId
      shouldFork = isJust parentSessionId
  (testsResponse, implResponse) <- sendM $ do
    logMsg "Launching parallel agents with build validation..."
    logDetail "parentSessionId" (maybe "(none - starting fresh)" T.unpack parentSessionId)
    concurrently
      (do
        logMsg $ "Tests agent starting in worktree: " <> testsWt.unWorktreePath
        runAgentWithBuildValidation config testsWt testsPrompt testsSchema "tests"
          parentSessionId shouldFork)  -- Fork from parent if available
      (do
        logMsg $ "Impl agent starting in worktree: " <> implWt.unWorktreePath
        runAgentWithBuildValidation config implWt implPrompt implSchema "impl"
          parentSessionId shouldFork)  -- Fork from parent if available

  sendM $ logPhase "FORK - Agents completed, parsing results"

  -- Extract session IDs and costs before parsing (for aggregation)
  let (testsSessionId, testsCost) = case testsResponse of
        Left _ -> ("", 0.0)
        Right r -> (CC.ccrSessionId r, CC.ccrTotalCostUsd r)
      (implSessionId, implCost) = case implResponse of
        Left _ -> ("", 0.0)
        Right r -> (CC.ccrSessionId r, CC.ccrTotalCostUsd r)

  -- Parse results (now metadata only, not code)
  testsResult <- parseOrError "tests" testsResponse
  implResult <- parseOrError "impl" implResponse

  -- Log the parsed results
  sendM $ do
    logMsg "=== Tests Agent Result ==="
    logDetail "buildPassed" (show testsResult.trBuildPassed)
    logDetail "allPropertiesWritten" (show testsResult.trAllPropertiesWritten)
    logDetail "commitMessage" (T.unpack testsResult.trCommitMessage)
    logDetail "testingStrategy" (T.unpack testsResult.trTestingStrategy)
    logDetail "sessionId" (T.unpack testsSessionId)
    logDetail "cost" (show testsCost)
    case testsResult.trBlocker of
      Nothing -> logMsg "  (no blocker)"
      Just b -> logDetail "BLOCKER" (T.unpack b)

    logMsg "=== Impl Agent Result ==="
    logDetail "buildPassed" (show implResult.irBuildPassed)
    logDetail "allFunctionsImplemented" (show implResult.irAllFunctionsImplemented)
    logDetail "commitMessage" (T.unpack implResult.irCommitMessage)
    logDetail "designNotes" (T.unpack implResult.irDesignNotes)
    logDetail "sessionId" (T.unpack implSessionId)
    logDetail "cost" (show implCost)
    case implResult.irBlocker of
      Nothing -> logMsg "  (no blocker)"
      Just b -> logDetail "BLOCKER" (T.unpack b)

  pure $ gotoChoice @"merge" ParallelResults
    { prTestsWorktree = testsWt
    , prImplWorktree = implWt
    , prTestsResult = testsResult
    , prImplResult = implResult
    , prTestsSessionId = testsSessionId
    , prImplSessionId = implSessionId
    , prTestsCost = testsCost
    , prImplCost = implCost
    , prParentSessionId = fromMaybe "" parentSessionId
    }

-- | Parse ClaudeCode response or error.
--
-- Logs errors with full context before failing.
parseOrError
  :: (StructuredOutput a)
  => Text
  -> Either CC.ClaudeCodeError CC.ClaudeCodeResult
  -> Eff DevEffects a
parseOrError agentName (Left err) = do
  sendM $ do
    logError $ "Claude Code " <> T.unpack agentName <> " agent failed"
    logDetail "error" (show err)
  throwError $ AgentFailed agentName (T.pack $ show err)
parseOrError agentName (Right result) = do
  -- Log the full event stream (all tool calls, reasoning, etc.)
  sendM $ logClaudeCodeEvents agentName (CC.ccrEvents result)

  -- Log the raw response for debugging
  sendM $ do
    logMsg $ "Parsing " <> T.unpack agentName <> " agent response..."
    logDetail "sessionId" (T.unpack $ CC.ccrSessionId result)
    logDetail "numTurns" (show $ CC.ccrNumTurns result)
    logDetail "costUsd" (show $ CC.ccrTotalCostUsd result)
    case CC.ccrStructuredOutput result of
      Nothing -> logDetail "structuredOutput" "NONE"
      Just val -> logDetail "structuredOutput (raw)" (take 500 $ show val)

  case CC.ccrStructuredOutput result of
    Nothing -> do
      sendM $ logError $ "Claude Code " <> T.unpack agentName <> " agent returned no structured output"
      throwError $ AgentNoOutput agentName
    Just val -> case parseStructured val of
      Left diag -> do
        sendM $ do
          logError $ "JSON parse error for " <> T.unpack agentName <> " agent"
          logDetail "parseError" (T.unpack $ formatDiagnostic diag)
          logDetail "rawJSON" (take 1000 $ show val)
        throwError $ AgentParseFailed agentName (formatDiagnostic diag)
      Right a -> do
        sendM $ logMsg $ T.unpack agentName <> " agent response parsed successfully"
        pure a


-- ════════════════════════════════════════════════════════════════════════════
-- v3 FORK HANDLER (semantic descriptions)
-- ════════════════════════════════════════════════════════════════════════════

-- | v3 Fork handler using semantic descriptions.
--
-- Like forkHandler but receives StubsGenerated (with semantic descriptions)
-- instead of SkeletonGenerated. The tests agent prompt includes semantic
-- descriptions to guide test writing.
forkHandlerV3
  :: StubsGenerated
  -> ClaudeCodeConfig
  -> StackSpec
  -> IO (Either String ParallelResults)
forkHandlerV3 input config _spec = do
  logPhase "v3 FORK - Creating worktrees"
  logDetail "projectPath" input.stgProjectPath
  logDetail "moduleName" (T.unpack input.stgModuleName)

  let projectPath = input.stgProjectPath

  -- Create worktrees for isolation
  -- Note: In real use, this would use the Worktree effect
  -- For now, we use the simpler git worktree commands directly
  testsWtPath <- createWorktreeIO projectPath "tests"
  implWtPath <- createWorktreeIO projectPath "impl"

  logDetail "testsWorktree" testsWtPath
  logDetail "implWorktree" implWtPath

  -- Copy build artifacts from parent for faster builds
  let parentDistNewstyle = projectPath </> "dist-newstyle"
  parentHasArtifacts <- doesDirectoryExist parentDistNewstyle
  when parentHasArtifacts $ do
    logMsg "Copying build artifacts to worktrees..."
    callProcess "cp" ["-r", parentDistNewstyle, testsWtPath </> "dist-newstyle"]
    callProcess "cp" ["-r", parentDistNewstyle, implWtPath </> "dist-newstyle"]
    logMsg "Build artifacts copied"

  -- Build v3 tests context (uses semantic descriptions instead of signatures)
  let testsCtxV3 = TestsContextV3
        { moduleName = input.stgModuleName
        , dataType = input.stgDataType
        , semantics = input.stgSemantics
        }

      -- Build impl context (still uses minimal info - impl agent reads the stubs)
      implCtx = ImplContext
        { moduleName = input.stgModuleName
        , dataType = input.stgDataType
        , signatures = []  -- Empty - impl agent reads stubs directly
        }

      -- Render prompts
      testsPrompt = runTypedTemplate testsCtxV3 servantTestsV3Compiled
      implPrompt = runTypedTemplate implCtx servantImplCompiled

      -- Build schemas
      testsSchema = Just $ schemaToValue (structuredSchema @TestsResult)
      implSchema = Just $ schemaToValue (structuredSchema @ImplResult)

  logPhase "v3 FORK - Spawning parallel agents"
  logDetail "testsPromptLength" (show $ T.length testsPrompt)
  logDetail "implPromptLength" (show $ T.length implPrompt)

  -- Both agents fork from the stubs agent's session to share context
  -- If parent session ID is empty, start fresh sessions instead of trying to fork
  let parentSessionId = input.stgSessionId
      parentSessionMaybe = if T.null parentSessionId then Nothing else Just parentSessionId
      shouldFork = isJust parentSessionMaybe
  logDetail "parentSessionId" (maybe "(none - starting fresh)" T.unpack parentSessionMaybe)

  -- Create crosstalk state for parallel agent communication
  -- Each agent tracks the other's worktree for commit notifications
  logMsg "Setting up crosstalk between parallel agents..."
  (testsCrosstalk, implCrosstalk) <- newCrosstalkStatePair
    testsWtPath "tests"
    implWtPath "impl"

  -- Run both agents in parallel with session forking and crosstalk
  logMsg "Launching parallel agents with build validation and crosstalk..."
  (testsResponse, implResponse) <- concurrently
    (do
      logMsg $ "Tests agent starting in worktree: " <> testsWtPath
      runAgentWithCrosstalk config testsWtPath testsPrompt testsSchema "tests"
        parentSessionMaybe shouldFork testsCrosstalk)
    (do
      logMsg $ "Impl agent starting in worktree: " <> implWtPath
      runAgentWithCrosstalk config implWtPath implPrompt implSchema "impl"
        parentSessionMaybe shouldFork implCrosstalk)

  logPhase "v3 FORK - Agents completed, parsing results"

  -- Parse results
  case (testsResponse, implResponse) of
    (Left err, _) -> pure $ Left $ "Tests agent failed: " <> show err
    (_, Left err) -> pure $ Left $ "Impl agent failed: " <> show err
    (Right testsCC, Right implCC) -> do
      -- Parse structured output
      testsResult <- parseStructuredOutput "tests" testsCC
      implResult <- parseStructuredOutput "impl" implCC

      case (testsResult, implResult) of
        (Left err, _) -> pure $ Left err
        (_, Left err) -> pure $ Left err
        (Right tr, Right ir) -> do
          -- Extract session IDs and costs for tracking
          let testsSessionId = CC.ccrSessionId testsCC
              implSessionId = CC.ccrSessionId implCC
              testsCost = CC.ccrTotalCostUsd testsCC
              implCost = CC.ccrTotalCostUsd implCC

          logMsg "=== Tests Agent Result ==="
          logDetail "buildPassed" (show tr.trBuildPassed)
          logDetail "allPropertiesWritten" (show tr.trAllPropertiesWritten)
          logDetail "commitMessage" (T.unpack tr.trCommitMessage)
          logDetail "sessionId" (T.unpack testsSessionId)
          logDetail "cost" (show testsCost)

          logMsg "=== Impl Agent Result ==="
          logDetail "buildPassed" (show ir.irBuildPassed)
          logDetail "allFunctionsImplemented" (show ir.irAllFunctionsImplemented)
          logDetail "commitMessage" (T.unpack ir.irCommitMessage)
          logDetail "sessionId" (T.unpack implSessionId)
          logDetail "cost" (show implCost)

          pure $ Right ParallelResults
            { prTestsWorktree = WorktreePath testsWtPath
            , prImplWorktree = WorktreePath implWtPath
            , prTestsResult = tr
            , prImplResult = ir
            , prTestsSessionId = testsSessionId
            , prImplSessionId = implSessionId
            , prTestsCost = testsCost
            , prImplCost = implCost
            , prParentSessionId = parentSessionId
            }

-- | Create a git worktree (simple IO version for v3 handler).
createWorktreeIO :: FilePath -> String -> IO FilePath
createWorktreeIO projectPath name = do
  let worktreePath = projectPath </> ".worktrees" </> name
      branchName = "worktree-" <> name

  -- Create parent directory
  createDirectoryIfMissing True (projectPath </> ".worktrees")

  -- Create worktree with new branch
  withCurrentDirectory projectPath $ do
    -- Remove existing worktree if present
    existingWt <- doesDirectoryExist worktreePath
    when existingWt $ do
      callProcess "git" ["worktree", "remove", "-f", worktreePath]

    callProcess "git" ["worktree", "add", "-B", branchName, worktreePath]

  pure worktreePath

-- | Parse structured output from Claude Code result.
parseStructuredOutput :: Aeson.FromJSON a => String -> CC.ClaudeCodeResult -> IO (Either String a)
parseStructuredOutput name result = do
  case CC.ccrStructuredOutput result of
    Nothing -> pure $ Left $ name <> " agent returned no structured output"
    Just val -> case Aeson.fromJSON val of
      Aeson.Error msg -> pure $ Left $ "Failed to parse " <> name <> " response: " <> msg
      Aeson.Success a -> pure $ Right a


-- ════════════════════════════════════════════════════════════════════════════
-- v3 WORKFLOW ORCHESTRATOR
-- ════════════════════════════════════════════════════════════════════════════

-- | Run the complete v3 workflow.
--
-- This is the entry point for the semantic descriptions workflow:
-- 1. Stubs agent writes code files, returns semantic descriptions
-- 2. Fork creates worktrees, spawns parallel tests + impl agents
-- 3. Merge combines results and runs tests
--
-- Returns success/failure with final test results.
runWorkflowV3
  :: StackSpec
  -> ClaudeCodeConfig
  -> IO (Either String Bool)  -- ^ Left = error, Right = tests passed
runWorkflowV3 spec config = do
  logPhase "v3 WORKFLOW START"
  logDetail "moduleName" (T.unpack spec.ssModuleName)
  logDetail "projectPath" spec.ssProjectPath
  logDetail "projectType" (show spec.ssProjectType)

  -- Phase 1: Stubs
  stubsResult <- stubsHandlerV3 spec config
  case stubsResult of
    Left err -> do
      logError $ "Stubs phase failed: " <> show err
      pure $ Left $ "Stubs phase failed: " <> show err
    Right stubsGen -> do
      logMsg "Stubs phase complete, proceeding to fork..."

      -- Phase 2: Fork (parallel agents)
      forkResult <- forkHandlerV3 stubsGen config spec
      case forkResult of
        Left err -> do
          logError $ "Fork phase failed: " <> err
          pure $ Left $ "Fork phase failed: " <> err
        Right parallelResults -> do
          logMsg "Fork phase complete, proceeding to merge..."

          -- Phase 3: Merge
          let projectPath = stubsGen.stgProjectPath
          mergeResult <- mergeHandlerV3 parallelResults projectPath

          pure mergeResult


-- | v3 Merge handler (simplified IO version).
--
-- Commits agent work, merges worktrees, runs tests.
mergeHandlerV3 :: ParallelResults -> FilePath -> IO (Either String Bool)
mergeHandlerV3 results projectPath = do
  logPhase "v3 MERGE - Committing and merging agent work"

  -- Commit impl work
  let implSuccess = results.prImplResult.irBuildPassed && results.prImplResult.irAllFunctionsImplemented
  when implSuccess $ do
    logMsg "Committing impl work..."
    commitWorktreeWork results.prImplWorktree.unWorktreePath results.prImplResult.irCommitMessage

  -- Commit tests work
  let testsSuccess = results.prTestsResult.trBuildPassed && results.prTestsResult.trAllPropertiesWritten
  when testsSuccess $ do
    logMsg "Committing tests work..."
    commitWorktreeWork results.prTestsWorktree.unWorktreePath results.prTestsResult.trCommitMessage

  -- Merge impl to main
  logMsg "Merging impl to main..."
  implMergeOk <- mergeWorktreeIO projectPath results.prImplWorktree.unWorktreePath "Merge impl agent work"

  -- Merge tests to main
  logMsg "Merging tests to main..."
  testsMergeOk <- mergeWorktreeIO projectPath results.prTestsWorktree.unWorktreePath "Merge tests agent work"

  let bothMergeSuccess = implMergeOk && testsMergeOk

  -- Run tests post-merge
  testsPassed <- if bothMergeSuccess
    then do
      logPhase "POST-MERGE TEST VALIDATION"
      logMsg "Running cabal test to verify implementation..."
      (testExit, testOut, testErr) <- withCurrentDirectory projectPath $
        readProcessWithExitCode "cabal" ["test", "--test-show-details=always"] ""
      case testExit of
        ExitSuccess -> do
          logMsg "POST-MERGE TESTS PASSED!"
          logMsg testOut
          pure True
        ExitFailure code -> do
          logError $ "POST-MERGE TESTS FAILED (exit code " <> show code <> ")"
          logError testErr
          logError testOut
          pure False
    else do
      logError "Merge failed, skipping test validation"
      pure False

  -- Cleanup worktrees
  if bothMergeSuccess
    then do
      logMsg "Cleaning up worktrees..."
      removeWorktreeIO projectPath results.prTestsWorktree.unWorktreePath
      removeWorktreeIO projectPath results.prImplWorktree.unWorktreePath
      logMsg "Worktrees deleted"
    else do
      logMsg "WARNING: Merge failed, preserving worktrees for debugging"
      logDetail "testsWorktree" results.prTestsWorktree.unWorktreePath
      logDetail "implWorktree" results.prImplWorktree.unWorktreePath

  -- Log final state
  logPhase $ "v3 WORKFLOW " <> if testsPassed then "COMPLETE (tests passed)" else "COMPLETE (tests FAILED)"
  (_, gitLog, _) <- readProcessWithExitCode "git" ["-C", projectPath, "log", "--oneline", "-10"] ""
  logMsg $ "Final git log:\n" <> gitLog

  pure $ Right testsPassed


-- | Merge a worktree branch to main (simple IO version).
mergeWorktreeIO :: FilePath -> FilePath -> String -> IO Bool
mergeWorktreeIO projectPath worktreePath commitMsg = do
  -- Get worktree branch name
  let branchName = "worktree-" <> takeBaseName worktreePath
  withCurrentDirectory projectPath $ do
    (mergeCode, _, mergeErr) <- readProcessWithExitCode "git" ["merge", branchName, "-m", commitMsg] ""
    case mergeCode of
      ExitSuccess -> do
        logMsg $ "Merged " <> branchName <> " successfully"
        pure True
      ExitFailure _ -> do
        logError $ "Failed to merge " <> branchName <> ": " <> mergeErr
        -- Try to abort the merge
        _ <- readProcessWithExitCode "git" ["merge", "--abort"] ""
        pure False
  where
    takeBaseName :: FilePath -> String
    takeBaseName = reverse . takeWhile (/= '/') . reverse


-- | Remove a worktree (simple IO version).
removeWorktreeIO :: FilePath -> FilePath -> IO ()
removeWorktreeIO projectPath worktreePath = do
  withCurrentDirectory projectPath $ do
    callProcess "git" ["worktree", "remove", "-f", worktreePath]


-- ════════════════════════════════════════════════════════════════════════════
-- MERGE NODE
-- ════════════════════════════════════════════════════════════════════════════

-- | Merge handler commits agent work, merges worktrees, and exits with results.
--
-- New design: agents edit files directly, we commit using their commit messages.
-- 1. Check if agents reported success
-- 2. Commit in each worktree using their commit messages
-- 3. Merge worktree branches to main using the Worktree effect
-- 4. Clean up worktrees
mergeHandler
  :: ParallelResults
  -> Eff DevEffects (GotoChoice '[To Exit ParallelResults])
mergeHandler results = do
  sendM $ logPhase "MERGE - Committing and merging agent work"

  -- Step 1: Commit impl work in its worktree (if successful)
  let implSuccess = results.prImplResult.irBuildPassed && results.prImplResult.irAllFunctionsImplemented
  sendM $ do
    logMsg $ "Impl agent success: " <> show implSuccess
    logDetail "implWorktree" results.prImplWorktree.unWorktreePath
  when implSuccess $ do
    sendM $ commitWorktreeWork results.prImplWorktree.unWorktreePath results.prImplResult.irCommitMessage

  -- Step 2: Commit tests work in its worktree (if successful)
  let testsSuccess = results.prTestsResult.trBuildPassed && results.prTestsResult.trAllPropertiesWritten
  sendM $ do
    logMsg $ "Tests agent success: " <> show testsSuccess
    logDetail "testsWorktree" results.prTestsWorktree.unWorktreePath
  when testsSuccess $ do
    sendM $ commitWorktreeWork results.prTestsWorktree.unWorktreePath results.prTestsResult.trCommitMessage

  -- Step 3: Merge worktree branches to main using the Worktree effect
  -- The mergeWorktree effect correctly gets the branch name from the worktree
  sendM $ logMsg "Merging impl worktree to main..."
  implMergeResult <- mergeWorktree results.prImplWorktree "Merge impl agent work"
  sendM $ logMergeResult "impl" implMergeResult

  sendM $ logMsg "Merging tests worktree to main..."
  testsMergeResult <- mergeWorktree results.prTestsWorktree "Merge tests agent work"
  sendM $ logMergeResult "tests" testsMergeResult

  -- Step 4: Clean up worktrees (only if both merges succeeded)
  let bothMergeSuccess = case (implMergeResult, testsMergeResult) of
        (Right MergeSuccess, Right MergeSuccess) -> True
        _ -> False

  -- Extract project path from worktree path (worktree is projectPath/.worktrees/name)
  let projectPath = takeDirectory (takeDirectory results.prImplWorktree.unWorktreePath)

  -- Step 5: Post-merge test validation
  -- Now that impl and tests are merged, tests should pass!
  testsPassed <- if bothMergeSuccess
    then sendM $ do
      logPhase "POST-MERGE TEST VALIDATION"
      logMsg "Running cabal test to verify implementation..."
      (testExit, testOut, testErr) <- withCurrentDirectory projectPath $
        readProcessWithExitCode "cabal" ["test", "--test-show-details=always"] ""
      case testExit of
        ExitSuccess -> do
          logMsg "POST-MERGE TESTS PASSED!"
          logMsg testOut
          pure True
        ExitFailure code -> do
          logError $ "POST-MERGE TESTS FAILED (exit code " <> show code <> ")"
          logError testErr
          logError testOut
          pure False
    else pure False

  if bothMergeSuccess
    then do
      sendM $ logMsg "Cleaning up worktrees..."
      _ <- deleteWorktree results.prTestsWorktree
      _ <- deleteWorktree results.prImplWorktree
      sendM $ logMsg "Worktrees deleted"
    else do
      sendM $ do
        logMsg "WARNING: Merge failed, preserving worktrees for debugging"
        logDetail "testsWorktree" results.prTestsWorktree.unWorktreePath
        logDetail "implWorktree" results.prImplWorktree.unWorktreePath

  -- Log final git state (in the correct directory!)
  sendM $ do
    logPhase $ "MERGE COMPLETE" <> if testsPassed then " (tests passed)" else " (tests FAILED)"
    logMsg $ "Final git log (in " <> projectPath <> "):"
    (_, gitLog, _) <- readProcessWithExitCode "git" ["-C", projectPath, "log", "--oneline", "-10"] ""
    logMsg gitLog

  -- Exit with the results
  pure $ gotoExit results

-- | Log merge result (handles Either from the effect).
logMergeResult :: String -> Either WorktreeError MergeResult -> IO ()
logMergeResult name (Right MergeSuccess) = logMsg $ name <> " merge: SUCCESS"
logMergeResult name (Right (MergeConflict msg)) = do
  logError $ name <> " merge: CONFLICT"
  logDetail "conflict" (T.unpack msg)
logMergeResult name (Left err) = do
  logError $ name <> " merge: ERROR"
  logDetail "error" (show err)

-- | Commit all changes in a worktree with the given message.
--
-- Handles the case where there's nothing to commit gracefully.
commitWorktreeWork :: FilePath -> Text -> IO ()
commitWorktreeWork worktreePath commitMsg = do
  logMsg $ "Committing in worktree: " <> worktreePath
  logDetail "commitMsg" (T.unpack commitMsg)

  withCurrentDirectory worktreePath $ do
    -- Check if there are changes to commit
    (_statusCode, statusOut, _) <- readProcessWithExitCode "git" ["status", "--porcelain"] ""

    if null statusOut
      then logMsg "  No changes to commit (worktree clean)"
      else do
        logMsg $ "  Changes detected:\n" <> statusOut
        -- Stage all changes
        (addCode, _, addErr) <- readProcessWithExitCode "git" ["add", "-A"] ""
        case addCode of
          ExitSuccess -> logMsg "  Staged all changes"
          ExitFailure _ -> logError $ "  git add failed: " <> addErr

        -- Commit with agent's message
        (commitCode, _, commitErr) <- readProcessWithExitCode
          "git" ["commit", "-m", T.unpack commitMsg] ""
        case commitCode of
          ExitSuccess -> logMsg "  Committed successfully"
          ExitFailure _ -> logError $ "  git commit failed: " <> commitErr


-- ════════════════════════════════════════════════════════════════════════════
-- TDD WORKFLOW (Sequential with validation loop)
-- ════════════════════════════════════════════════════════════════════════════

-- | Maximum fix attempts before giving up.
maxFixAttempts :: Int
maxFixAttempts = 5

-- | Effect stack for TDD workflow.
--
-- Includes Cabal effect for running builds/tests.
type TDDEffects = '[Reader ClaudeCodeConfig, Reader StackSpec, ClaudeCodeExec, Cabal, IO]

-- | Handlers for TypesFirstGraphTDD (sequential TDD version).
--
-- Sequential workflow:
-- 1. Types agent designs types
-- 2. Skeleton generator creates stubs
-- 3. Tests agent writes tests
-- 4. Verify tests fail (proves tests are meaningful)
-- 5. Impl agent implements functions
-- 6. Validate tests pass
-- 7. Fix loop if tests fail
typesFirstHandlersTDD :: StackSpec -> TypesFirstGraphTDD (AsHandler TDDEffects)
typesFirstHandlersTDD spec = TypesFirstGraphTDD
  { tddEntry = Proxy @StackSpec

    -- Types handler: Writes type signatures
  , tddTypes = ClaudeCodeLLMHandler @'Haiku @'Nothing
      Nothing                              -- no system template
      (selectTypesTemplate spec.ssProjectType)
      tddBuildTypesContext
      tddRouteToSkeleton

    -- Skeleton handler: Generates impl/test skeleton files
  , tddSkeleton = tddSkeletonHandler spec

    -- Tests handler: Writes QuickCheck property tests
  , tddTests = ClaudeCodeLLMHandler @'Haiku @'Nothing
      Nothing
      (selectTestsTemplate spec.ssProjectType)
      tddBuildTestsContext
      tddRouteToVerify

    -- Verify tests fail (proves tests are meaningful)
  , tddVerifyTestsFail = tddVerifyTestsFailHandler

    -- Impl handler: Implements functions
  , tddImpl = ClaudeCodeLLMHandler @'Haiku @'Nothing
      Nothing
      (selectImplTemplate spec.ssProjectType)
      tddBuildImplContext
      tddRouteToValidate

    -- Validate tests pass
  , tddValidate = tddValidateHandler

    -- Fix handler: Fixes implementation based on test failures
  , tddFix = ClaudeCodeLLMHandler @'Haiku @'Nothing
      Nothing
      fixCompiled
      tddBuildFixContext
      tddRouteAfterFix

  , tddExit = Proxy @TDDResult
  }

-- | Select tests template based on project type.
selectTestsTemplate :: ProjectType -> TypedTemplate TestsContext SourcePos
selectTestsTemplate PureLibrary   = testsCompiled
selectTestsTemplate ServantServer = servantTestsCompiled
selectTestsTemplate CLIApp        = testsCompiled

-- | Select impl template based on project type.
selectImplTemplate :: ProjectType -> TypedTemplate ImplContext SourcePos
selectImplTemplate PureLibrary   = implCompiled
selectImplTemplate ServantServer = servantImplCompiled
selectImplTemplate CLIApp        = implCompiled


-- ════════════════════════════════════════════════════════════════════════════
-- TDD TYPES HANDLER
-- ════════════════════════════════════════════════════════════════════════════

-- | Build context for the types template (TDD version).
tddBuildTypesContext
  :: StackSpec
  -> Eff TDDEffects TypesContext
tddBuildTypesContext spec = pure TypesContext
  { moduleName = spec.ssModuleName
  , description = spec.ssDescription
  , acceptanceCriteria = spec.ssAcceptanceCriteria
  }

-- | Route from types to skeleton in TDD workflow.
tddRouteToSkeleton
  :: ClaudeCodeResult TypeDefinitions
  -> Eff TDDEffects (GotoChoice '[To "tddSkeleton" TypeDefinitions])
tddRouteToSkeleton result = do
  sendM $ logMsg "Types agent completed, routing to skeleton generation"
  pure $ gotoChoice @"tddSkeleton" result.ccrParsedOutput


-- ════════════════════════════════════════════════════════════════════════════
-- TDD SKELETON HANDLER
-- ════════════════════════════════════════════════════════════════════════════

-- | TDD skeleton handler - generates impl/test skeleton files.
--
-- Similar to regular skeleton handler but produces SkeletonState for TDD flow.
tddSkeletonHandler
  :: StackSpec
  -> TypeDefinitions
  -> Eff TDDEffects (GotoChoice '[To "tddTests" SkeletonState])
tddSkeletonHandler spec typeDefs = do
  let projectPath = spec.ssProjectPath
      modulePath = T.unpack $ T.replace "." "/" spec.ssModuleName
      implPath = projectPath <> "/src/" <> modulePath <> ".hs"
      testPath = projectPath <> "/test/Main.hs"

      -- Build skeleton context from type definitions
      skeletonCtx = SkeletonContext
        { moduleName = spec.ssModuleName
        , typeName = typeDefs.tdTypeName
        , dataType = typeDefs.tdDataType
        , signatures = typeDefs.tdSignatures
        , testPriorities = typeDefs.tdTestPriorities
        , imports = typeDefs.tdImports
        }

      -- Select templates based on project type
      (implTemplate, testTemplate) = case spec.ssProjectType of
        PureLibrary   -> (implSkeletonCompiled, testSkeletonCompiled)
        ServantServer -> (servantImplSkeletonCompiled, servantTestSkeletonCompiled)
        CLIApp        -> (implSkeletonCompiled, testSkeletonCompiled)

      -- Render templates
      implCode = runTypedTemplate skeletonCtx implTemplate
      testCode = runTypedTemplate skeletonCtx testTemplate

  sendM $ do
    logPhase "TDD SKELETON GENERATION"
    logDetail "projectPath" projectPath
    logDetail "implPath" implPath
    logDetail "testPath" testPath

    createDirectoryIfMissing True (takeDirectory implPath)
    createDirectoryIfMissing True (takeDirectory testPath)
    TIO.writeFile implPath implCode
    logMsg $ "Wrote impl skeleton: " <> implPath
    TIO.writeFile testPath testCode
    logMsg $ "Wrote test skeleton: " <> testPath

    -- Write .gitignore
    let gitignorePath = projectPath <> "/.gitignore"
    writeFile gitignorePath "dist-newstyle/\n*.hi\n*.o\n*.dyn_hi\n*.dyn_o\n"

    -- Validate skeleton compiles
    logMsg "Validating skeleton compiles..."
    (exitCode, buildOut, buildErr) <- withCurrentDirectory projectPath $
      readProcessWithExitCode "cabal" ["build", "-v0", "all"] ""
    case exitCode of
      ExitSuccess -> logMsg "Skeleton validation passed"
      ExitFailure code -> do
        logError $ "Skeleton failed to compile (exit code " <> show code <> "):"
        logError buildErr
        logError buildOut
        error "Skeleton compilation failed - fix templates before proceeding"

    -- Commit skeletons
    logGit "Committing skeleton files" $ withCurrentDirectory projectPath $ do
      callProcess "git" ["add", implPath, testPath, gitignorePath]
      callProcess "git" ["commit", "-m", "Add skeleton files for " <> T.unpack spec.ssModuleName]

    logMsg "TDD skeleton generation complete"

  -- Route to tests with skeleton state
  pure $ gotoChoice @"tddTests" SkeletonState
    { ssTypeDefs = typeDefs
    , ssImplPath = implPath
    , ssTestPath = testPath
    , ssProjectPath = projectPath
    }


-- ════════════════════════════════════════════════════════════════════════════
-- TDD TESTS HANDLER
-- ════════════════════════════════════════════════════════════════════════════

-- | Build context for tests template in TDD workflow.
tddBuildTestsContext
  :: SkeletonState
  -> Eff TDDEffects TestsContext
tddBuildTestsContext skeleton = do
  spec <- ask @StackSpec
  pure TestsContext
    { moduleName = spec.ssModuleName
    , dataType = skeleton.ssTypeDefs.tdDataType
    , signatures = skeleton.ssTypeDefs.tdSignatures
    , testPriorities = skeleton.ssTypeDefs.tdTestPriorities
    }

-- | Route from tests to verify in TDD workflow.
tddRouteToVerify
  :: ClaudeCodeResult TestsResult
  -> Eff TDDEffects (GotoChoice '[To "tddVerifyTestsFail" TestsWritten])
tddRouteToVerify result = do
  sendM $ logMsg "Tests agent completed, routing to verification"
  -- We need the skeleton state - get it from reader or reconstruct
  spec <- ask @StackSpec
  let modulePath = T.unpack $ T.replace "." "/" spec.ssModuleName
      implPath = spec.ssProjectPath <> "/src/" <> modulePath <> ".hs"
      testPath = spec.ssProjectPath <> "/test/Main.hs"
      -- We need typeDefs but don't have them here - this is a design issue
      -- For now, create a minimal skeleton state (the actual types were written to files)
      skeleton = SkeletonState
        { ssTypeDefs = TypeDefinitions
            { tdTypeName = ""  -- Not needed for downstream
            , tdDataType = ""
            , tdSignatures = []
            , tdTestPriorities = []
            , tdImports = []
            }
        , ssImplPath = implPath
        , ssTestPath = testPath
        , ssProjectPath = spec.ssProjectPath
        }
  pure $ gotoChoice @"tddVerifyTestsFail" TestsWritten
    { twSkeletonState = skeleton
    , twTestsResult = result.ccrParsedOutput
    }


-- ════════════════════════════════════════════════════════════════════════════
-- TDD VERIFY TESTS FAIL HANDLER
-- ════════════════════════════════════════════════════════════════════════════

-- | Verify tests fail (TDD step 2).
--
-- Runs `cabal test` and verifies that tests FAIL.
-- This proves the tests are meaningful (not trivially passing).
tddVerifyTestsFailHandler
  :: TestsWritten
  -> Eff TDDEffects (GotoChoice '[To "tddImpl" TestsVerified])
tddVerifyTestsFailHandler testsWritten = do
  let projectPath = testsWritten.twSkeletonState.ssProjectPath

  sendM $ logPhase "TDD VERIFY TESTS FAIL"
  sendM $ logMsg "Running tests to verify they fail before implementation..."

  -- Run cabal test
  testResult <- sendM $ withCurrentDirectory projectPath $
    readProcessWithExitCode "cabal" ["test", "--test-show-details=always"] ""

  case testResult of
    (ExitFailure _, _, testOut) -> do
      -- Good! Tests fail as expected
      let failures = parseTestOutput (T.pack testOut)
      sendM $ do
        logMsg $ "✓ Tests fail as expected (" <> show (length failures) <> " failures)"
        logMsg "This proves the tests are meaningful"
      pure $ gotoChoice @"tddImpl" TestsVerified
        { tvTestsWritten = testsWritten
        , tvFailingTests = failures
        }
    (ExitSuccess, _, _) -> do
      -- Bad! Tests pass before impl - tests are trivial
      sendM $ logError "✗ Tests PASS before implementation!"
      sendM $ logError "This suggests tests are trivially passing (e.g., always return True)"
      error "TDD violation: tests pass before implementation - tests are likely trivial"


-- ════════════════════════════════════════════════════════════════════════════
-- TDD IMPL HANDLER
-- ════════════════════════════════════════════════════════════════════════════

-- | Build context for impl template in TDD workflow.
tddBuildImplContext
  :: TestsVerified
  -> Eff TDDEffects ImplContext
tddBuildImplContext verified = do
  spec <- ask @StackSpec
  pure ImplContext
    { moduleName = spec.ssModuleName
    , dataType = verified.tvTestsWritten.twSkeletonState.ssTypeDefs.tdDataType
    , signatures = verified.tvTestsWritten.twSkeletonState.ssTypeDefs.tdSignatures
    }

-- | Route from impl to validate in TDD workflow.
tddRouteToValidate
  :: ClaudeCodeResult ImplResult
  -> Eff TDDEffects (GotoChoice '[To "tddValidate" ImplWritten])
tddRouteToValidate result = do
  sendM $ logMsg "Impl agent completed, routing to validation"
  -- Reconstruct TestsVerified from context (same design issue as above)
  spec <- ask @StackSpec
  let modulePath = T.unpack $ T.replace "." "/" spec.ssModuleName
      implPath = spec.ssProjectPath <> "/src/" <> modulePath <> ".hs"
      testPath = spec.ssProjectPath <> "/test/Main.hs"
      skeleton = SkeletonState
        { ssTypeDefs = TypeDefinitions
            { tdTypeName = ""
            , tdDataType = ""
            , tdSignatures = []
            , tdTestPriorities = []
            , tdImports = []
            }
        , ssImplPath = implPath
        , ssTestPath = testPath
        , ssProjectPath = spec.ssProjectPath
        }
      testsWritten = TestsWritten
        { twSkeletonState = skeleton
        , twTestsResult = TestsResult
            { trBuildPassed = True
            , trAllPropertiesWritten = True
            , trCommitMessage = ""
            , trTestingStrategy = ""
            , trBlocker = Nothing
            }
        }
      verified = TestsVerified
        { tvTestsWritten = testsWritten
        , tvFailingTests = []
        }
  pure $ gotoChoice @"tddValidate" ImplWritten
    { iwTestsVerified = verified
    , iwImplResult = result.ccrParsedOutput
    , iwAttempt = 1
    }


-- ════════════════════════════════════════════════════════════════════════════
-- TDD VALIDATE HANDLER
-- ════════════════════════════════════════════════════════════════════════════

-- | Validate tests pass (TDD step 4).
--
-- Runs `cabal test` and checks if all tests pass.
-- On success: exits with TDDResult
-- On failure: routes to fix handler for iteration
tddValidateHandler
  :: ImplWritten
  -> Eff TDDEffects (GotoChoice '[To "tddFix" ValidationFailure, To Exit TDDResult])
tddValidateHandler implWritten = do
  spec <- ask @StackSpec
  let projectPath = spec.ssProjectPath

  sendM $ logPhase $ "TDD VALIDATE (attempt " <> show implWritten.iwAttempt <> ")"
  sendM $ logMsg "Running tests to verify implementation..."

  -- Run cabal test
  testResult <- sendM $ withCurrentDirectory projectPath $
    readProcessWithExitCode "cabal" ["test", "--test-show-details=always"] ""

  case testResult of
    (ExitSuccess, _, testOut) -> do
      -- Success! All tests pass
      sendM $ do
        logMsg "✓ All tests pass!"
        logMsg "TDD workflow complete"
      pure $ gotoExit TDDResult
        { tdrSuccess = True
        , tdrAttempts = implWritten.iwAttempt
        , tdrTypeDefs = implWritten.iwTestsVerified.tvTestsWritten.twSkeletonState.ssTypeDefs
        , tdrTestsResult = implWritten.iwTestsVerified.tvTestsWritten.twTestsResult
        , tdrImplResult = implWritten.iwImplResult
        , tdrFinalTestOutput = T.pack testOut
        }
    (ExitFailure _, _, testOut) -> do
      -- Tests failed - need to fix
      let failures = parseTestOutput (T.pack testOut)
      sendM $ do
        logMsg $ "✗ Tests failed (" <> show (length failures) <> " failures)"
        mapM_ (\f -> logDetail "failure" (T.unpack f.tfPropertyName)) failures

      if implWritten.iwAttempt >= maxFixAttempts
        then do
          sendM $ logError $ "Max fix attempts (" <> show maxFixAttempts <> ") exceeded"
          error $ "TDD failed: could not fix implementation after " <> show maxFixAttempts <> " attempts"
        else do
          sendM $ logMsg $ "Routing to fix handler (attempt " <> show (implWritten.iwAttempt + 1) <> ")"
          pure $ gotoChoice @"tddFix" ValidationFailure
            { vfImplWritten = implWritten
            , vfFailures = failures
            , vfAttempt = implWritten.iwAttempt + 1
            }


-- ════════════════════════════════════════════════════════════════════════════
-- TDD FIX HANDLER
-- ════════════════════════════════════════════════════════════════════════════

-- | Build context for fix template.
tddBuildFixContext
  :: ValidationFailure
  -> Eff TDDEffects FixContext
tddBuildFixContext failure = do
  spec <- ask @StackSpec
  let modulePath = T.unpack $ T.replace "." "/" spec.ssModuleName
      implPath = spec.ssProjectPath <> "/src/" <> modulePath <> ".hs"
  pure FixContext
    { moduleName = spec.ssModuleName
    , implPath = T.pack implPath
    , failures = failure.vfFailures
    , attempt = failure.vfAttempt
    }

-- | Route after fix - back to validate.
tddRouteAfterFix
  :: ClaudeCodeResult FixResult
  -> Eff TDDEffects (GotoChoice '[To "tddValidate" ImplWritten])
tddRouteAfterFix result = do
  sendM $ do
    logMsg "Fix agent completed"
    logDetail "buildPassed" (show result.ccrParsedOutput.frBuildPassed)
    mapM_ (\c -> logDetail "change" (T.unpack c)) result.ccrParsedOutput.frChangesMade

  -- Reconstruct ImplWritten for the next validation attempt
  spec <- ask @StackSpec
  let modulePath = T.unpack $ T.replace "." "/" spec.ssModuleName
      implPath = spec.ssProjectPath <> "/src/" <> modulePath <> ".hs"
      testPath = spec.ssProjectPath <> "/test/Main.hs"
      skeleton = SkeletonState
        { ssTypeDefs = TypeDefinitions
            { tdTypeName = ""
            , tdDataType = ""
            , tdSignatures = []
            , tdTestPriorities = []
            , tdImports = []
            }
        , ssImplPath = implPath
        , ssTestPath = testPath
        , ssProjectPath = spec.ssProjectPath
        }
      testsWritten = TestsWritten
        { twSkeletonState = skeleton
        , twTestsResult = TestsResult
            { trBuildPassed = True
            , trAllPropertiesWritten = True
            , trCommitMessage = ""
            , trTestingStrategy = ""
            , trBlocker = Nothing
            }
        }
      verified = TestsVerified
        { tvTestsWritten = testsWritten
        , tvFailingTests = []
        }
      -- Extract attempt from fix result context (we don't have ValidationFailure here)
      -- This is another design issue - we need to pass attempt through
      -- For now, assume attempt counter is managed elsewhere
      newImplResult = ImplResult
        { irBuildPassed = result.ccrParsedOutput.frBuildPassed
        , irAllFunctionsImplemented = True
        , irCommitMessage = result.ccrParsedOutput.frCommitMessage
        , irDesignNotes = ""
        , irBlocker = result.ccrParsedOutput.frBlocker
        }
  -- TODO: We need to track attempt count properly - for now increment
  pure $ gotoChoice @"tddValidate" ImplWritten
    { iwTestsVerified = verified
    , iwImplResult = newImplResult
    , iwAttempt = 2  -- This should be vfAttempt but we don't have access here
    }
