{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Handlers for the types-first development workflow graph.
--
-- Parallel version with fork/merge handlers for concurrent agent execution.
module TypesFirstDev.Handlers
  ( typesFirstHandlers

    -- * Effect Stack
  , DevEffects

    -- * Logging
  , logToFile

    -- * v3 Workflow (semantic descriptions)
  , stubsHandlerV3
  , forkHandlerV3
  , runWorkflowV3
  ) where

import Control.Concurrent.Async (concurrently)
import Control.Monad (when)
import Control.Monad.Freer (Eff, sendM)
import Control.Monad.Freer.Reader (Reader, ask)
import Data.Maybe (fromMaybe)
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
import Tidepool.ClaudeCode.Executor (runClaudeCodeRequest)
import Tidepool.ClaudeCode.Types qualified as CC
import Tidepool.Effect.ClaudeCode (ClaudeCodeExec)
import Tidepool.Effects.Worktree (Worktree, createWorktree, deleteWorktree, mergeWorktree, WorktreeSpec(..), WorktreePath(..), MergeResult(..), WorktreeError(..))
import Tidepool.Graph.Generic (AsHandler)
import Tidepool.Graph.Goto (GotoChoice, To, ClaudeCodeLLMHandler(..), ClaudeCodeResult(..), gotoChoice, gotoExit)
import Tidepool.Graph.Template (runTypedTemplate, TypedTemplate)
import Text.Parsec.Pos (SourcePos)
import Tidepool.Graph.Types (ModelChoice(..), Exit)
import Tidepool.Schema (schemaToValue)
import Tidepool.StructuredOutput (StructuredOutput(..), formatDiagnostic)

import TypesFirstDev.Context (TypesContext(..), TestsContext(..), ImplContext(..), SkeletonContext(..), StubsContext(..), TestsContextV3(..))
import TypesFirstDev.Graph (TypesFirstGraph(..))
import TypesFirstDev.Types
  ( StackSpec(..)
  , ProjectType(..)
  , TypeDefinitions(..)
  , ForkInput(..)
  , SkeletonGenerated(..)
  , StubsGenerated(..)
  , StubsOutput(..)
  , FunctionSemantics(..)
  , ParallelResults(..)
  , TestsResult(..)
  , ImplResult(..)
  )
import TypesFirstDev.Templates
  ( typesCompiled, testsCompiled, implCompiled, implSkeletonCompiled, testSkeletonCompiled
  , servantTypesCompiled, servantTestsCompiled, servantImplCompiled
  , servantImplSkeletonCompiled, servantTestSkeletonCompiled
  , servantStubsCompiled, servantTestsV3Compiled
  )


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
-- - Reader ClaudeCodeConfig: Config for spawning Claude Code subprocesses
-- - Reader StackSpec: The original specification (for routing)
-- - ClaudeCodeExec: For spawning Claude Code subprocesses
-- - Worktree: For managing git worktrees (parallel isolation)
-- - IO: For system operations
type DevEffects = '[Reader ClaudeCodeConfig, Reader StackSpec, ClaudeCodeExec, Worktree, IO]


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
  , types = ClaudeCodeLLMHandler @'Haiku @'Nothing
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
routeToSkeleton
  :: ClaudeCodeResult TypeDefinitions
  -> Eff DevEffects (GotoChoice '[To "skeleton" ForkInput])
routeToSkeleton result = do
  spec <- ask @StackSpec
  pure $ gotoChoice @"skeleton" ForkInput
    { fiSessionId = fromMaybe "" result.ccrSessionId
    , fiTypeDefs = result.ccrParsedOutput
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

  -- Write skeleton files and commit to git (so worktrees have them)
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
    let gitignorePath = projectPath <> "/.gitignore"
    writeFile gitignorePath "dist-newstyle/\n*.hi\n*.o\n*.dyn_hi\n*.dyn_o\n"
    logMsg $ "Wrote .gitignore: " <> gitignorePath

    -- Validate skeleton compiles before committing
    logMsg "Validating skeleton compiles..."
    (exitCode, buildOut, buildErr) <- withCurrentDirectory projectPath $
      readProcessWithExitCode "cabal" ["build", "-v0", "all"] ""  -- -v0 for quiet output
    case exitCode of
      ExitSuccess -> logMsg "Skeleton validation passed"
      ExitFailure code -> do
        logError $ "Skeleton failed to compile (exit code " <> show code <> "):"
        logError buildErr
        logError buildOut
        error "Skeleton compilation failed - fix templates before proceeding"

    -- Commit skeletons so worktrees will have them
    logGit "Committing skeleton files" $ withCurrentDirectory projectPath $ do
      callProcess "git" ["add", implPath, testPath, gitignorePath]
      callProcess "git" ["commit", "-m", "Add skeleton files for " <> T.unpack input.fiModuleName]

    logMsg "Skeleton generation complete"

  -- Route to fork with generated paths
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
runAgentWithBuildValidation
  :: ClaudeCodeConfig
  -> WorktreePath
  -> Text  -- ^ Base prompt
  -> Maybe Aeson.Value  -- ^ JSON schema
  -> Text  -- ^ Agent name (for logging)
  -> IO (Either CC.ClaudeCodeError CC.ClaudeCodeResult)
runAgentWithBuildValidation ccConfig (WorktreePath wtPath) basePrompt schema agentName = go 0 Nothing
  where
    go :: Int -> Maybe Text -> IO (Either CC.ClaudeCodeError CC.ClaudeCodeResult)
    go attempt lastBuildError
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

          -- Run agent
          result <- runClaudeCodeRequest ccConfig Haiku (Just wtPath) fullPrompt schema Nothing Nothing False

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


-- | Run stubs agent with build validation (v3 workflow).
--
-- Like runAgentWithBuildValidation but works in a FilePath (not WorktreePath).
-- Used for the initial stubs phase before worktrees are created.
runStubsAgentWithBuildValidation
  :: ClaudeCodeConfig
  -> FilePath  -- ^ Project path
  -> Text  -- ^ Base prompt
  -> Maybe Aeson.Value  -- ^ JSON schema
  -> IO (Either CC.ClaudeCodeError CC.ClaudeCodeResult)
runStubsAgentWithBuildValidation ccConfig projectPath basePrompt schema = go 0 Nothing
  where
    go :: Int -> Maybe Text -> IO (Either CC.ClaudeCodeError CC.ClaudeCodeResult)
    go attempt lastBuildError
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

          -- Run agent in the project directory
          result <- runClaudeCodeRequest ccConfig Haiku (Just projectPath) fullPrompt schema Nothing Nothing False

          case result of
            Left err -> pure $ Left err
            Right ccResult -> do
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

  -- Run stubs agent with build validation
  result <- runStubsAgentWithBuildValidation config projectPath stubsPrompt stubsSchema

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
forkHandler
  :: SkeletonGenerated
  -> Eff DevEffects (GotoChoice '[To "merge" ParallelResults])
forkHandler input = do
  config <- ask @ClaudeCodeConfig
  spec <- ask @StackSpec

  sendM $ logPhase "FORK - Creating worktrees"

  -- Create worktrees for isolation
  testsWtResult <- createWorktree (WorktreeSpec "tests" Nothing)
  implWtResult <- createWorktree (WorktreeSpec "impl" Nothing)

  -- Handle worktree creation errors
  (testsWt, implWt) <- case (testsWtResult, implWtResult) of
    (Right t, Right i) -> pure (t, i)
    (Left err, _) -> sendM $ do
      logError $ "Failed to create tests worktree: " <> show err
      error "Worktree creation failed"
    (_, Left err) -> sendM $ do
      logError $ "Failed to create impl worktree: " <> show err
      error "Worktree creation failed"

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
  (testsResponse, implResponse) <- sendM $ do
    logMsg "Launching parallel agents with build validation..."
    concurrently
      (do
        logMsg $ "Tests agent starting in worktree: " <> testsWt.unWorktreePath
        runAgentWithBuildValidation config testsWt testsPrompt testsSchema "tests")
      (do
        logMsg $ "Impl agent starting in worktree: " <> implWt.unWorktreePath
        runAgentWithBuildValidation config implWt implPrompt implSchema "impl")

  sendM $ logPhase "FORK - Agents completed, parsing results"

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
    case testsResult.trBlocker of
      Nothing -> logMsg "  (no blocker)"
      Just b -> logDetail "BLOCKER" (T.unpack b)

    logMsg "=== Impl Agent Result ==="
    logDetail "buildPassed" (show implResult.irBuildPassed)
    logDetail "allFunctionsImplemented" (show implResult.irAllFunctionsImplemented)
    logDetail "commitMessage" (T.unpack implResult.irCommitMessage)
    logDetail "designNotes" (T.unpack implResult.irDesignNotes)
    case implResult.irBlocker of
      Nothing -> logMsg "  (no blocker)"
      Just b -> logDetail "BLOCKER" (T.unpack b)

  pure $ gotoChoice @"merge" ParallelResults
    { prTestsWorktree = testsWt
    , prImplWorktree = implWt
    , prTestsResult = testsResult
    , prImplResult = implResult
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
  error $ "Claude Code " <> show agentName <> " agent failed: " <> show err
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
      error $ "Claude Code " <> show agentName <> " agent returned no structured output"
    Just val -> case parseStructured val of
      Left diag -> do
        sendM $ do
          logError $ "JSON parse error for " <> T.unpack agentName <> " agent"
          logDetail "parseError" (T.unpack $ formatDiagnostic diag)
          logDetail "rawJSON" (take 1000 $ show val)
        error $ "Failed to parse " <> show agentName <> " response: " <> T.unpack (formatDiagnostic diag)
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
forkHandlerV3 input config spec = do
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

  -- Run both agents in parallel
  logMsg "Launching parallel agents with build validation..."
  (testsResponse, implResponse) <- concurrently
    (do
      logMsg $ "Tests agent starting in worktree: " <> testsWtPath
      runAgentWithBuildValidation config (WorktreePath testsWtPath) testsPrompt testsSchema "tests")
    (do
      logMsg $ "Impl agent starting in worktree: " <> implWtPath
      runAgentWithBuildValidation config (WorktreePath implWtPath) implPrompt implSchema "impl")

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
          logMsg "=== Tests Agent Result ==="
          logDetail "buildPassed" (show tr.trBuildPassed)
          logDetail "allPropertiesWritten" (show tr.trAllPropertiesWritten)
          logDetail "commitMessage" (T.unpack tr.trCommitMessage)

          logMsg "=== Impl Agent Result ==="
          logDetail "buildPassed" (show ir.irBuildPassed)
          logDetail "allFunctionsImplemented" (show ir.irAllFunctionsImplemented)
          logDetail "commitMessage" (T.unpack ir.irCommitMessage)

          pure $ Right ParallelResults
            { prTestsWorktree = WorktreePath testsWtPath
            , prImplWorktree = WorktreePath implWtPath
            , prTestsResult = tr
            , prImplResult = ir
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
