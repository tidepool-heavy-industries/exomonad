{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Handlers for the types-first development workflow graph.
--
-- Parallel version with fork/merge handlers for concurrent agent execution.
module TypesFirstDev.Handlers
  ( typesFirstHandlers

    -- * Effect Stack
  , DevEffects
  ) where

import Control.Concurrent.Async (concurrently)
import Control.Exception (SomeException, try, throwIO)
import Control.Monad.Freer (Eff, sendM)
import Control.Monad.Freer.Reader (Reader, ask)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO qualified as TIO
import qualified Data.Aeson as Aeson
import qualified System.Directory
import qualified System.Exit
import qualified System.Process

import Tidepool.ClaudeCode.Config (ClaudeCodeConfig)
import Tidepool.ClaudeCode.Executor (runClaudeCodeRequest)
import Tidepool.ClaudeCode.Types qualified as CC
import Tidepool.Effect.ClaudeCode (ClaudeCodeExec)
import Tidepool.Effects.Worktree
  ( Worktree
  , WorktreePath(..)
  , WorktreeSpec(..)
  , createWorktree
  , deleteWorktree
  )
import Tidepool.Graph.Generic (AsHandler)
import Tidepool.Graph.Goto (GotoChoice, To, ClaudeCodeLLMHandler(..), ClaudeCodeResult(..), gotoChoice, gotoExit)
import Tidepool.Graph.Template (templateCompiled, runTypedTemplate)
import Tidepool.Graph.Types (ModelChoice(..), Exit)
import Tidepool.Schema (HasJSONSchema(..), schemaToValue)

import TypesFirstDev.Context (TypesContext(..), TestsContext(..), ImplContext(..))
import TypesFirstDev.Graph (TypesFirstGraph(..))
import TypesFirstDev.Schema
  ( StackSpec(..)
  , TypeDefinitions(..)
  , ForkInput(..)
  , ParallelResults(..)
  , TestDefinitions(..)
  , ImplementationCode(..)
  )
import TypesFirstDev.Templates (TypesTpl, testsCompiled, implCompiled)


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
-- - types: Writes type signatures, routes to fork
-- - fork: Creates worktrees, spawns parallel agents
-- - merge: Collects results, cleans up worktrees
typesFirstHandlers :: TypesFirstGraph (AsHandler DevEffects)
typesFirstHandlers = TypesFirstGraph
  { entry = Proxy @StackSpec

    -- Types handler: Writes type signatures, routes to fork
  , types = ClaudeCodeLLMHandler @'Sonnet @'Nothing
      Nothing                              -- no system template
      (templateCompiled @TypesTpl)         -- user template
      buildTypesContext                    -- before: builds context
      routeToFork                          -- after: routes to fork with session ID

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
  }

-- | Route after types agent completes.
--
-- Routes to fork node with session ID for parallel agent forking.
routeToFork
  :: ClaudeCodeResult TypeDefinitions
  -> Eff DevEffects (GotoChoice '[To "fork" ForkInput])
routeToFork result = do
  spec <- ask @StackSpec
  pure $ gotoChoice @"fork" ForkInput
    { fiSessionId = fromMaybe "" result.ccrSessionId
    , fiTypeDefs = result.ccrParsedOutput
    , fiProjectPath = spec.ssProjectPath
    , fiModuleName = spec.ssModuleName
    }


-- ════════════════════════════════════════════════════════════════════════════
-- FORK NODE
-- ════════════════════════════════════════════════════════════════════════════

-- | Fork handler spawns parallel agents in separate worktrees.
--
-- Creates two worktrees, spawns tests and impl agents concurrently,
-- then collects results for the merge node. Worktrees are cleaned up
-- on any failure to prevent resource leaks.
forkHandler
  :: ForkInput
  -> Eff DevEffects (GotoChoice '[To "merge" ParallelResults])
forkHandler input = do
  config <- ask @ClaudeCodeConfig

  -- Create worktrees for isolation (with error handling)
  testsWtResult <- createWorktree (WorktreeSpec "tests" Nothing)
  testsWt <- case testsWtResult of
    Left err -> error $ "Failed to create tests worktree: " <> show err
    Right wt -> pure wt

  implWtResult <- createWorktree (WorktreeSpec "impl" Nothing)
  implWt <- case implWtResult of
    Left err -> do
      -- Cleanup tests worktree on failure
      _ <- deleteWorktree testsWt
      error $ "Failed to create impl worktree: " <> show err
    Right wt -> pure wt

  -- Build contexts for each agent
  let testsCtx = TestsContext
        { moduleName = input.fiModuleName
        , dataType = input.fiTypeDefs.tdDataType
        , signatures = input.fiTypeDefs.tdSignatures
        }
      implCtx = ImplContext
        { moduleName = input.fiModuleName
        , dataType = input.fiTypeDefs.tdDataType
        , signatures = input.fiTypeDefs.tdSignatures
        }

      -- Render templates
      testsPrompt = runTypedTemplate testsCtx testsCompiled
      implPrompt = runTypedTemplate implCtx implCompiled

      -- Build schemas
      testsSchema = Just $ schemaToValue (jsonSchema @TestDefinitions)
      implSchema = Just $ schemaToValue (jsonSchema @ImplementationCode)

  -- Run both agents in parallel at IO level with cleanup on failure.
  -- If anything throws, we clean up both worktrees before re-raising.
  let WorktreePath testsWtPath = testsWt
      WorktreePath implWtPath = implWt

      cleanupWorktrees :: IO ()
      cleanupWorktrees = do
        -- Best effort cleanup - ignore errors
        _ <- try @SomeException $ runWorktreeCleanup testsWt
        _ <- try @SomeException $ runWorktreeCleanup implWt
        pure ()

      -- Run agents with cleanup on failure
      runAgentsWithCleanup :: IO (TestDefinitions, ImplementationCode)
      runAgentsWithCleanup = do
        result <- try @SomeException $ do
          (testsResponse, implResponse) <- concurrently
            (runClaudeCodeRequest config Sonnet (Just testsWtPath) testsPrompt testsSchema
               Nothing Nothing False)
            (runClaudeCodeRequest config Sonnet (Just implWtPath) implPrompt implSchema
               Nothing Nothing False)
          testDefs <- parseOrErrorIO "tests" testsResponse
          implCode <- parseOrErrorIO "impl" implResponse
          pure (testDefs, implCode)
        case result of
          Left exc -> cleanupWorktrees >> throwIO exc
          Right r -> pure r

  (testDefs, implCode) <- sendM runAgentsWithCleanup

  pure $ gotoChoice @"merge" ParallelResults
    { prTestsWorktree = testsWt
    , prImplWorktree = implWt
    , prTestDefs = testDefs
    , prImplCode = implCode
    }

-- | Cleanup a worktree via git command (for use in IO cleanup).
-- Uses git worktree remove to properly clean up refs, falls back to
-- directory removal if git command fails.
runWorktreeCleanup :: WorktreePath -> IO ()
runWorktreeCleanup (WorktreePath path) = do
  -- Try git worktree remove first (properly cleans up refs)
  (exitCode, _, _) <- System.Process.readProcessWithExitCode
    "git" ["worktree", "remove", "--force", path] ""
  -- Fall back to directory removal if git command fails
  case exitCode of
    System.Exit.ExitSuccess -> pure ()
    System.Exit.ExitFailure _ -> do
      _ <- try @SomeException $ System.Directory.removeDirectoryRecursive path
      pure ()

-- | Parse ClaudeCode response or error (IO version for use in cleanup blocks).
parseOrErrorIO
  :: (Aeson.FromJSON a)
  => Text
  -> Either CC.ClaudeCodeError CC.ClaudeCodeResult
  -> IO a
parseOrErrorIO agentName (Left err) =
  error $ "Claude Code " <> show agentName <> " agent failed: " <> show err
parseOrErrorIO agentName (Right result) =
  case CC.ccrStructuredOutput result of
    Nothing -> error $ "Claude Code " <> show agentName <> " agent returned no structured output"
    Just val -> case Aeson.fromJSON val of
      Aeson.Error msg -> error $ "Failed to parse " <> show agentName <> " response: " <> msg
      Aeson.Success a -> pure a


-- ════════════════════════════════════════════════════════════════════════════
-- MERGE NODE
-- ════════════════════════════════════════════════════════════════════════════

-- | Merge handler writes files, cleans up worktrees, and exits with results.
mergeHandler
  :: ParallelResults
  -> Eff DevEffects (GotoChoice '[To Exit ParallelResults])
mergeHandler results = do
  spec <- ask @StackSpec

  -- Write generated code to project
  sendM $ writeGeneratedFiles spec results

  -- Clean up worktrees (best effort - ignore errors)
  _ <- deleteWorktree results.prTestsWorktree
  _ <- deleteWorktree results.prImplWorktree

  -- Exit with the results
  pure $ gotoExit results

-- | Write the generated implementation and test files to the project.
writeGeneratedFiles :: StackSpec -> ParallelResults -> IO ()
writeGeneratedFiles spec results = do
  let projectPath = spec.ssProjectPath
      -- Convert module name to path: "Data.Stack" -> "src/Data/Stack.hs"
      modulePath = T.unpack $ T.replace "." "/" spec.ssModuleName
      implPath = projectPath <> "/src/" <> modulePath <> ".hs"
      testPath = projectPath <> "/test/Main.hs"

  -- Write implementation
  TIO.writeFile implPath results.prImplCode.implModuleCode

  -- Write tests
  TIO.writeFile testPath results.prTestDefs.testModuleCode
