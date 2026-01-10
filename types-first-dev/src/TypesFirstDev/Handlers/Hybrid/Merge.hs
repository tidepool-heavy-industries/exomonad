{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Merge handlers for WS3 nodes of the hybrid TDD graph.
--
-- WS3 owns:
-- * hJoin (barrier collecting parallel results)
-- * hVerifyTDD (external TDD verification)
-- * hTestsReject (handle trivial tests, loop back)
-- * hMerge (cherry-pick tests+impl into merge worktree)
-- * hConflictResolve (LLM-based conflict resolution)
module TypesFirstDev.Handlers.Hybrid.Merge
  ( -- * Handlers
    hJoinHandler
  , hVerifyTDDHandler
  , hTestsRejectHandler
  , hMergeHandler
  , hConflictResolveHandler
  ) where

import Control.Monad.Freer (Eff, sendM)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import System.Directory (withCurrentDirectory)
import System.FilePath ((</>))

import Tidepool.Effects.Worktree (createWorktree, WorktreeSpec(..), WorktreePath(..))

import TypesFirstDev.Effect.Build (testWithDetails, buildAll)
import qualified TypesFirstDev.Effect.Build as Build
import Tidepool.Graph.Goto
  ( GotoChoice
  , To
  , ClaudeCodeLLMHandler(..)
  , ClaudeCodeResult(..)
  , gotoChoice
  )
import Tidepool.Graph.Memory (getMem, updateMem)
import Tidepool.Graph.Types (ModelChoice(..), HList(..))

import TypesFirstDev.Types.Hybrid
import TypesFirstDev.Templates.Hybrid (hConflictResolveCompiled)
import TypesFirstDev.Handlers.Hybrid.Effects (HybridEffects, SessionContext(..))


-- ════════════════════════════════════════════════════════════════════════════
-- JOIN HANDLER (Logic Node - Barrier)
-- ════════════════════════════════════════════════════════════════════════════

-- | Handler for hJoin node (BarrierNode).
-- Collects results from parallel tests and impl agents.
-- Receives HList from barrier, constructs BlindResults.
hJoinHandler
  :: HList '[TestsResult, ImplResult]
  -> Eff HybridEffects (GotoChoice '[To "hVerifyTDD" BlindResults])
hJoinHandler (testsResult ::: implResult ::: HNil) = do
  let blindResults = BlindResults
        { brTests = testsResult
        , brImpl = implResult
        }
  sendM $ putStrLn "[JOIN] Collected blind results from parallel agents"
  sendM $ putStrLn $ "  Tests worktree: " <> blindResults.brTests.testsWorktree
  sendM $ putStrLn $ "  Impl worktree: " <> blindResults.brImpl.implWorktree
  pure $ gotoChoice @"hVerifyTDD" blindResults


-- ════════════════════════════════════════════════════════════════════════════
-- VERIFY TDD HANDLER (Logic Node - External Verification)
-- ════════════════════════════════════════════════════════════════════════════

-- | Handler for hVerifyTDD node.
-- External TDD verification: re-runs tests on SKELETON to confirm they fail.
-- This is the "belt and suspenders" check - we don't trust agent self-reports.
--
-- If tests pass on skeleton → tests are trivial/broken → route to hTestsReject
-- If tests fail on skeleton → good TDD! → route to hMerge
hVerifyTDDHandler
  :: BlindResults
  -> Eff HybridEffects (GotoChoice '[To "hMerge" VerifiedResults, To "hTestsReject" TrivialTestsError])
hVerifyTDDHandler blindResults = do
  sendM $ putStrLn "[VERIFY_TDD] External TDD verification - running tests on skeleton"

  -- Get GatedState from memory stash (set by hFork in WS2)
  ctx <- getMem @SessionContext
  case ctx.scGatedStash of
    Nothing -> do
      sendM $ putStrLn "[VERIFY_TDD] WARNING: No GatedState in stash, using tests worktree"
      -- Fallback: use tests worktree path (should have skeleton)
      verifyWithPath blindResults (blindResults.brTests.testsWorktree) 1

    Just gatedState -> do
      let projectPath = gatedState.gsSkeleton.ssProjectPath
      sendM $ putStrLn $ "[VERIFY_TDD] Running tests on skeleton at: " <> projectPath
      verifyWithPath blindResults projectPath 1

  where
    verifyWithPath
      :: BlindResults
      -> FilePath
      -> Int  -- attempt number
      -> Eff HybridEffects (GotoChoice '[To "hMerge" VerifiedResults, To "hTestsReject" TrivialTestsError])
    verifyWithPath results projectPath attempt = do
      -- Run tests on the skeleton (impl stubs are undefined)
      testResult <- testWithDetails projectPath

      if Build.trSuccess testResult
        then do
          -- Tests passed on skeleton - BAD! Tests are trivial/broken
          sendM $ putStrLn "[VERIFY_TDD] FAILED - Tests passed on skeleton (they should fail)"
          sendM $ putStrLn $ "  Output: " <> T.unpack (T.take 500 (Build.trOutput testResult))

          let feedback = TrivialTestsFeedback
                { whyRejected = "Tests passed on skeleton with undefined stubs. Tests must fail on skeleton to verify they actually exercise the implementation."
                , propertiesWrote = results.brTests.testsOutput.propertiesWritten
                , suggestion = "Write tests that call the actual functions and check their behavior. Properties like `prop_pushPopInverse` should fail when push/pop are undefined."
                }
              trivialError = TrivialTestsError
                { tteBlindResults = results
                , tteMessage = "Tests passed on skeleton - trivial tests detected"
                , tteAttempt = attempt
                , tteFeedback = feedback
                }

          pure $ gotoChoice @"hTestsReject" trivialError

        else do
          -- Tests failed on skeleton - GOOD! TDD verified
          sendM $ putStrLn "[VERIFY_TDD] SUCCESS - Tests fail on skeleton (as expected)"
          sendM $ putStrLn $ "  Error output (expected): " <> T.unpack (T.take 200 (Build.trErrors testResult))

          let verifiedResults = VerifiedResults
                { vrBlindResults = results
                , vrExternalVerified = True
                }

          pure $ gotoChoice @"hMerge" verifiedResults


-- ════════════════════════════════════════════════════════════════════════════
-- TESTS REJECT HANDLER (Logic Node - Feedback Loop)
-- ════════════════════════════════════════════════════════════════════════════

-- | Handler for hTestsReject node.
-- Handles trivial tests that passed on skeleton.
-- Routes back to hFork with feedback for the tests agent to retry.
hTestsRejectHandler
  :: TrivialTestsError
  -> Eff HybridEffects (GotoChoice '[To "hFork" GatedState])
hTestsRejectHandler trivialError = do
  sendM $ putStrLn "[TESTS_REJECT] Handling trivial tests"
  sendM $ putStrLn $ "  Attempt: " <> show trivialError.tteAttempt
  sendM $ putStrLn $ "  Message: " <> T.unpack trivialError.tteMessage
  sendM $ putStrLn $ "  Why rejected: " <> T.unpack trivialError.tteFeedback.whyRejected

  -- Retrieve GatedState from memory to route back to hFork
  ctx <- getMem @SessionContext
  case ctx.scGatedStash of
    Nothing -> error "BUG: No GatedState in stash for hTestsReject"
    Just gatedState -> do
      sendM $ putStrLn "[TESTS_REJECT] Routing back to hFork with feedback"
      -- The feedback is in the GatedState via TestsTemplateCtx.ttcPriorFeedback
      -- WS2's hFork will need to propagate this
      pure $ gotoChoice @"hFork" gatedState


-- ════════════════════════════════════════════════════════════════════════════
-- MERGE HANDLER (Logic Node - Cherry-Pick Strategy)
-- ════════════════════════════════════════════════════════════════════════════

-- | Handler for hMerge node.
-- Creates a fresh merge worktree and cherry-picks tests + impl commits.
-- Cherry-pick order is critical: tests first, then impl.
hMergeHandler
  :: VerifiedResults
  -> Eff HybridEffects (GotoChoice '[To "hValidate" MergedState, To "hConflictResolve" ConflictState])
hMergeHandler verifiedResults = do
  sendM $ putStrLn "[MERGE] Creating merge worktree and cherry-picking commits"

  let testsCommit = verifiedResults.vrBlindResults.brTests.testsCommitHash
      implCommit = verifiedResults.vrBlindResults.brImpl.implCommitHash

  sendM $ putStrLn $ "  Tests commit: " <> T.unpack testsCommit
  sendM $ putStrLn $ "  Impl commit: " <> T.unpack implCommit

  -- Create fresh merge worktree
  mergeResult <- createWorktree (WorktreeSpec "merge" Nothing)
  case mergeResult of
    Left err -> do
      sendM $ putStrLn $ "[MERGE] ERROR: Failed to create merge worktree: " <> show err
      error $ "Failed to create merge worktree: " <> show err

    Right (WorktreePath mergePath) -> do
      sendM $ putStrLn $ "  Merge worktree: " <> mergePath

      -- Cherry-pick tests commit FIRST
      sendM $ putStrLn "[MERGE] Cherry-picking tests commit..."
      (testsPickCode, _, testsPickErr) <- sendM $ withCurrentDirectory mergePath $
        readProcessWithExitCode "git" ["cherry-pick", T.unpack testsCommit] ""

      case testsPickCode of
        ExitSuccess -> do
          sendM $ putStrLn "  Tests cherry-pick: SUCCESS"

          -- Cherry-pick impl commit SECOND
          sendM $ putStrLn "[MERGE] Cherry-picking impl commit..."
          (implPickCode, _, implPickErr) <- sendM $ withCurrentDirectory mergePath $
            readProcessWithExitCode "git" ["cherry-pick", T.unpack implCommit] ""

          case implPickCode of
            ExitSuccess -> do
              sendM $ putStrLn "  Impl cherry-pick: SUCCESS"

              -- Both cherry-picks succeeded - verify build
              sendM $ putStrLn "[MERGE] Verifying merged code builds..."
              buildResult <- buildAll mergePath

              if Build.brSuccess buildResult
                then do
                  sendM $ putStrLn "[MERGE] Build passed - routing to validation"
                  let mergedState = MergedState
                        { msVerifiedResults = verifiedResults
                        , msMergeWorktree = mergePath
                        , msUnderstanding = initialUnderstanding
                        }
                  pure $ gotoChoice @"hValidate" mergedState

                else do
                  sendM $ putStrLn $ "[MERGE] Build failed after merge: " <> T.unpack (Build.brErrors buildResult)
                  -- Build failure after clean merge - treat as conflict
                  routeToConflictResolve verifiedResults mergePath []

            ExitFailure _ -> do
              -- Impl cherry-pick had conflicts
              sendM $ putStrLn $ "  Impl cherry-pick: CONFLICT - " <> implPickErr
              conflictedFiles <- getConflictedFiles mergePath
              routeToConflictResolve verifiedResults mergePath conflictedFiles

        ExitFailure _ -> do
          -- Tests cherry-pick had conflicts (rare)
          sendM $ putStrLn $ "  Tests cherry-pick: CONFLICT - " <> testsPickErr
          conflictedFiles <- getConflictedFiles mergePath
          routeToConflictResolve verifiedResults mergePath conflictedFiles

  where
    routeToConflictResolve
      :: VerifiedResults
      -> FilePath
      -> [FilePath]
      -> Eff HybridEffects (GotoChoice '[To "hValidate" MergedState, To "hConflictResolve" ConflictState])
    routeToConflictResolve results mergePath conflictPaths = do
      sendM $ putStrLn $ "[MERGE] Routing to conflict resolution with " <> show (length conflictPaths) <> " files"

      -- Read conflict markers from each file
      conflictedWithContent <- sendM $ mapM (readConflictedFile mergePath) conflictPaths

      -- Use design notes from impl agent (simplified schema)
      let implContext = results.vrBlindResults.brImpl.implOutput.designNotes
          -- Tests context is just a summary of what was written (simplified schema)
          testsProps = results.vrBlindResults.brTests.testsOutput.propertiesWritten
          testsContext = "Properties written: " <> T.intercalate ", " testsProps

      let conflictState = ConflictState
            { csVerifiedResults = results
            , csMergeWorktree = mergePath
            , csConflictedFiles = conflictedWithContent
            , csTestsContext = testsContext
            , csImplContext = implContext
            }

      -- Stash for the LLM handler
      updateMem (\ctx -> ctx { scConflictStash = Just conflictState })

      pure $ gotoChoice @"hConflictResolve" conflictState


-- | Get list of conflicted files in a directory.
getConflictedFiles :: FilePath -> Eff HybridEffects [FilePath]
getConflictedFiles dir = do
  (_, out, _) <- sendM $ withCurrentDirectory dir $
    readProcessWithExitCode "git" ["diff", "--name-only", "--diff-filter=U"] ""
  pure $ filter (not . null) $ lines out


-- | Read a conflicted file's content including conflict markers.
readConflictedFile :: FilePath -> FilePath -> IO (FilePath, Text)
readConflictedFile dir relPath = do
  content <- TIO.readFile (dir </> relPath)
  pure (relPath, content)


-- ════════════════════════════════════════════════════════════════════════════
-- CONFLICT RESOLVE HANDLER (LLM Node)
-- ════════════════════════════════════════════════════════════════════════════

-- | Handler for hConflictResolve node.
-- Uses ClaudeCode to resolve git merge conflicts with context from both agents.
hConflictResolveHandler
  :: ClaudeCodeLLMHandler
       'Haiku                                    -- model
       ConflictState                             -- needs
       ConflictResolveOutput                     -- schema
       '[To "hValidate" MergedState]             -- targets
       HybridEffects                             -- effs
       ConflictResolveTemplateCtx                -- tpl
hConflictResolveHandler = ClaudeCodeLLMHandler @'Haiku
  Nothing                      -- no system template
  hConflictResolveCompiled     -- user template
  buildConflictResolveContext  -- before: builds context
  routeAfterConflictResolve    -- after: routes to validate
  where
    buildConflictResolveContext :: ConflictState -> Eff HybridEffects ConflictResolveTemplateCtx
    buildConflictResolveContext conflictState = do
      -- Stash conflict state for after-handler
      updateMem (\ctx -> ctx { scConflictStash = Just conflictState })
      pure ConflictResolveTemplateCtx
        { conflictedFiles = conflictState.csConflictedFiles
        , testsContext = conflictState.csTestsContext
        , implContext = conflictState.csImplContext
        , mergeWorktree = conflictState.csMergeWorktree
        }

    routeAfterConflictResolve :: ClaudeCodeResult ConflictResolveOutput -> Eff HybridEffects (GotoChoice '[To "hValidate" MergedState])
    routeAfterConflictResolve ccResult = do
      -- Retrieve stashed conflict state
      ctx <- getMem @SessionContext
      case ctx.scConflictStash of
        Nothing -> error "BUG: ConflictState not stashed before LLM call"
        Just conflictState -> do
          let output = ccResult.ccrParsedOutput
          sendM $ putStrLn $ "[CONFLICT_RESOLVE] Resolved " <> show (length output.croFilesResolved) <> " files"
          -- Note: Build verification is done mechanically by handler, not claimed by LLM
          sendM $ putStrLn $ "[CONFLICT_RESOLVE] Notes: " <> T.unpack output.croResolutionNotes

          -- Check for blocker
          case output.croBlocker of
            Just blocker -> do
              sendM $ putStrLn $ "[CONFLICT_RESOLVE] WARNING: Blocker reported: " <> T.unpack blocker
              -- TODO: Could implement retry logic here
            Nothing -> pure ()

          let mergedState = MergedState
                { msVerifiedResults = conflictState.csVerifiedResults
                , msMergeWorktree = conflictState.csMergeWorktree
                , msUnderstanding = initialUnderstanding
                }

          pure $ gotoChoice @"hValidate" mergedState
