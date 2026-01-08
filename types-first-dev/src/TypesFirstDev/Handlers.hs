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
import System.Directory (createDirectoryIfMissing, withCurrentDirectory)
import System.FilePath (takeDirectory)
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
import Tidepool.Graph.Template (templateCompiled, runTypedTemplate)
import Tidepool.Graph.Types (ModelChoice(..), Exit)
import Tidepool.Schema (HasJSONSchema(..), schemaToValue)

import TypesFirstDev.Context (TypesContext(..), TestsContext(..), ImplContext(..), SkeletonContext(..))
import TypesFirstDev.Graph (TypesFirstGraph(..))
import TypesFirstDev.Schema
  ( StackSpec(..)
  , TypeDefinitions(..)
  , ForkInput(..)
  , SkeletonGenerated(..)
  , ParallelResults(..)
  , TestsResult(..)
  , ImplResult(..)
  )
import TypesFirstDev.Templates (TypesTpl, testsCompiled, implCompiled, implSkeletonCompiled, testSkeletonCompiled)


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
-- - types: Writes type signatures, routes to skeleton
-- - skeleton: Generates impl/test skeleton files
-- - fork: Creates worktrees, spawns parallel agents
-- - merge: Collects results, cleans up worktrees
typesFirstHandlers :: TypesFirstGraph (AsHandler DevEffects)
typesFirstHandlers = TypesFirstGraph
  { entry = Proxy @StackSpec

    -- Types handler: Writes type signatures, routes to skeleton
  , types = ClaudeCodeLLMHandler @'Haiku @'Nothing
      Nothing                              -- no system template
      (templateCompiled @TypesTpl)         -- user template
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
        }

      -- Render templates
      implCode = runTypedTemplate skeletonCtx implSkeletonCompiled
      testCode = runTypedTemplate skeletonCtx testSkeletonCompiled

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

  -- Build contexts for each agent
  let testsCtx = TestsContext
        { moduleName = input.sgModuleName
        , dataType = input.sgTypeDefs.tdDataType
        , signatures = input.sgTypeDefs.tdSignatures
        }
      implCtx = ImplContext
        { moduleName = input.sgModuleName
        , dataType = input.sgTypeDefs.tdDataType
        , signatures = input.sgTypeDefs.tdSignatures
        }

      -- Render templates
      testsPrompt = runTypedTemplate testsCtx testsCompiled
      implPrompt = runTypedTemplate implCtx implCompiled

      -- Build schemas (using new result types - no code, just metadata)
      testsSchema = Just $ schemaToValue (jsonSchema @TestsResult)
      implSchema = Just $ schemaToValue (jsonSchema @ImplResult)

  sendM $ do
    logPhase "FORK - Spawning parallel agents"
    logDetail "testsPromptLength" (show $ T.length testsPrompt)
    logDetail "implPromptLength" (show $ T.length implPrompt)

  -- Run both agents in parallel at IO level (freer-simple doesn't support parallel Eff)
  -- Note: Session forking disabled for now - each agent starts fresh
  (testsResponse, implResponse) <- sendM $ do
    logMsg "Launching parallel agents..."
    concurrently
      (do
        logMsg $ "Tests agent starting in worktree: " <> testsWt.unWorktreePath
        result <- runClaudeCodeRequest config Haiku (Just testsWt.unWorktreePath) testsPrompt testsSchema
          Nothing Nothing False  -- No session forking for now
        logMsg "Tests agent completed"
        pure result)
      (do
        logMsg $ "Impl agent starting in worktree: " <> implWt.unWorktreePath
        result <- runClaudeCodeRequest config Haiku (Just implWt.unWorktreePath) implPrompt implSchema
          Nothing Nothing False
        logMsg "Impl agent completed"
        pure result)

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
  :: (Aeson.FromJSON a)
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
    Just val -> case Aeson.fromJSON val of
      Aeson.Error msg -> do
        sendM $ do
          logError $ "JSON parse error for " <> T.unpack agentName <> " agent"
          logDetail "parseError" msg
          logDetail "rawJSON" (take 1000 $ show val)
        error $ "Failed to parse " <> show agentName <> " response: " <> msg
      Aeson.Success a -> do
        sendM $ logMsg $ T.unpack agentName <> " agent response parsed successfully"
        pure a


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

  if bothMergeSuccess
    then do
      sendM $ logMsg "Cleaning up worktrees..."
      deleteWorktree results.prTestsWorktree
      deleteWorktree results.prImplWorktree
      sendM $ logMsg "Worktrees deleted"
    else do
      sendM $ do
        logMsg "WARNING: Merge failed, preserving worktrees for debugging"
        logDetail "testsWorktree" results.prTestsWorktree.unWorktreePath
        logDetail "implWorktree" results.prImplWorktree.unWorktreePath

  -- Extract project path from worktree path (worktree is projectPath/.worktrees/name)
  let projectPath = takeDirectory (takeDirectory results.prImplWorktree.unWorktreePath)

  -- Log final git state (in the correct directory!)
  sendM $ do
    logPhase "MERGE COMPLETE"
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
