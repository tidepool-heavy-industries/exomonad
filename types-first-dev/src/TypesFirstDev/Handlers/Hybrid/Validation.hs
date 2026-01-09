{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Validation handlers for WS4 of the hybrid TDD graph.
--
-- WS4 owns the validation loop:
-- * hValidate - Run tests, route to fix or post-validate
-- * hFix - Fix impl based on test failures (organic learning loop)
-- * hPostValidate - Route to mutation adversary
module TypesFirstDev.Handlers.Hybrid.Validation
  ( -- * Handlers
    hValidateHandler
  , hFixHandler
  , hPostValidateHandler

    -- * Utilities
  , parseTestOutput
  , extractFailurePatterns
  , checkExitConditions
  ) where

import Control.Monad (unless)
import Control.Monad.Freer (Eff, sendM)
import Control.Monad.Freer.Error (Error, throwError)
import Control.Monad.Freer.Reader (Reader, ask)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T

import Tidepool.Effect.ClaudeCode (ClaudeCodeExec)
import Tidepool.Effects.Worktree (Worktree)
import Tidepool.Graph.Goto (GotoChoice, To, gotoChoice)
import Tidepool.Graph.Memory (Memory, getMem, updateMem)

import TypesFirstDev.Types.Hybrid
import TypesFirstDev.Handlers.Hybrid.Effects (HybridEffects, SessionContext(..), WorkflowError(..))


-- ════════════════════════════════════════════════════════════════════════════
-- VALIDATE HANDLER (Logic Node)
-- ════════════════════════════════════════════════════════════════════════════

-- | Handler for hValidate node.
-- Runs tests on merged code, routes based on results.
hValidateHandler
  :: MergedState
  -> Eff HybridEffects (GotoChoice '[To "hPostValidate" ValidatedState, To "hFix" ValidationFailure])
hValidateHandler mergedState = do
  spec <- ask @StackSpec
  let worktree = msMergeWorktree mergedState

  -- Run cabal test in the merge worktree
  sendM $ putStrLn $ "[VALIDATE] Running tests in " <> worktree

  -- TODO: Actually run cabal test via Worktree effect
  -- For now, simulate test execution
  testOutput <- runTests worktree

  case testOutput of
    TestsPassed count -> do
      sendM $ putStrLn $ "[VALIDATE] All " <> show count <> " tests passed!"
      let validatedState = ValidatedState
            { vsMergedState = mergedState
            , vsTestsPassed = count
            }
      pure $ gotoChoice @"hPostValidate" validatedState

    TestsFailed failures -> do
      sendM $ putStrLn $ "[VALIDATE] " <> show (length failures) <> " test failures"

      -- Extract patterns from failures
      let newPatterns = extractFailurePatterns failures
          understanding = mergedState.msUnderstanding
          updatedPatterns = mergePatterns (usFailuresSeen understanding) newPatterns
          updatedUnderstanding = understanding
            { usFailuresSeen = updatedPatterns
            }
          updatedMerged = mergedState
            { msUnderstanding = updatedUnderstanding
            }

      let validationFailure = ValidationFailure
            { vfMergedState = updatedMerged
            , vfFailures = failures
            , vfNewPatterns = newPatterns
            }

      pure $ gotoChoice @"hFix" validationFailure


-- | Test execution result
data TestResult
  = TestsPassed Int  -- number of tests
  | TestsFailed [StructuredFailure]

-- | Run tests in the given worktree
-- TODO: Implement actual test execution via Worktree effect
runTests :: FilePath -> Eff HybridEffects TestResult
runTests _worktree = do
  -- Placeholder: In reality, would run `cabal test` and parse output
  -- For now, simulate success
  pure $ TestsPassed 10


-- ════════════════════════════════════════════════════════════════════════════
-- FIX HANDLER (Logic Node with internal ClaudeCode call)
-- ════════════════════════════════════════════════════════════════════════════

-- | Handler for hFix node.
-- Checks exit conditions, fixes implementation based on test failures.
--
-- Note: Although this is a LogicNode in the graph, it internally calls
-- ClaudeCode to perform the fix. This gives us control over exit conditions
-- before and after the LLM call.
hFixHandler
  :: ValidationFailure
  -> Eff HybridEffects (GotoChoice '[To "hValidate" MergedState])
hFixHandler validationFailure = do
  spec <- ask @StackSpec
  let mergedState = vfMergedState validationFailure
      understanding = mergedState.msUnderstanding
      maxAttempts = spec.specStrictness.scMaxFixAttempts

  -- Check exit conditions BEFORE calling LLM
  checkExitConditions understanding

  -- Count current attempt
  let attemptNum = length (usFixesApplied understanding) + 1
  sendM $ putStrLn $ "[FIX] Attempt " <> show attemptNum <> "/" <> show maxAttempts

  -- Check max attempts
  unless (attemptNum <= maxAttempts) $
    throwError $ MaxAttemptsExceeded attemptNum

  -- Build context for fix template
  let fixCtx = FixTemplateCtx
        { failures = vfFailures validationFailure
        , understanding = understanding
        , fixFunctions = getFunctionsFromMergedState mergedState
        , worktreePath = msMergeWorktree mergedState
        }

  -- TODO: Call ClaudeCode with fix template
  -- For now, simulate a fix
  sendM $ putStrLn "[FIX] Calling ClaudeCode to analyze and fix failures..."

  -- Simulate fix result
  let fixResult = FixAgentOutput
        { fixChanges = []  -- Would come from LLM
        , fixBuildPassed = True
        , fixCommitMsg = "fix: address test failures"
        , fixBlocker = Nothing
        }

  -- Update understanding with applied fixes
  let updatedUnderstanding = understanding
        { usFixesApplied = usFixesApplied understanding ++ fixChanges fixResult
        , usLearnings = usLearnings understanding ++ ["Applied fix attempt " <> T.pack (show attemptNum)]
        , usConverging = checkConvergence understanding (vfNewPatterns validationFailure)
        }
      updatedMerged = mergedState
        { msUnderstanding = updatedUnderstanding
        }

  pure $ gotoChoice @"hValidate" updatedMerged


-- | Check exit conditions before attempting fix
checkExitConditions :: UnderstandingState -> Eff HybridEffects ()
checkExitConditions understanding = do
  -- Check for stuck pattern (same signature 3+ times)
  let stuckPatterns = filter (\p -> fpOccurrences p >= 3) (usFailuresSeen understanding)
  unless (null stuckPatterns) $ do
    sendM $ putStrLn $ "[FIX] STUCK: Same failure patterns seen 3+ times"
    throwError $ StuckOnPattern (T.intercalate ", " $ map fpSignature stuckPatterns)

  -- Check convergence
  unless (usConverging understanding) $ do
    sendM $ putStrLn "[FIX] NOT CONVERGING: Fixes are not making progress"
    throwError $ NotConverging


-- | Check if fixes are converging (making progress)
-- Compares old accumulated patterns with new accumulated patterns after merging.
checkConvergence :: UnderstandingState -> [FailurePattern] -> Bool
checkConvergence oldUnderstanding newPatterns =
  -- Converging if the accumulated set of unique failure signatures is not growing.
  -- We compare the number of distinct patterns before and after merging.
  let oldSignatures = map fpSignature (usFailuresSeen oldUnderstanding)
      newSignatures = map fpSignature newPatterns
      combinedSignatures = nub (oldSignatures ++ newSignatures)
  in length combinedSignatures <= length oldSignatures


-- | Extract function specs from merged state
getFunctionsFromMergedState :: MergedState -> [FunctionSpec]
getFunctionsFromMergedState mergedState =
  -- Get from the tests output which has the function specs
  let testsOutput = mergedState.msVerifiedResults.vrBlindResults.brTests.testsOutput
  in []  -- TODO: Extract from testsOutput or StackSpec


-- ════════════════════════════════════════════════════════════════════════════
-- POST-VALIDATE HANDLER (Logic Node)
-- ════════════════════════════════════════════════════════════════════════════

-- | Handler for hPostValidate node.
-- Stashes state for witness, routes to mutation adversary.
hPostValidateHandler
  :: ValidatedState
  -> Eff HybridEffects (GotoChoice '[To "hMutationAdversary" MutationTemplateCtx])
hPostValidateHandler validatedState = do
  spec <- ask @StackSpec

  sendM $ putStrLn "[POST-VALIDATE] Tests passed, spawning mutation adversary"

  -- Stash validated state for witness to retrieve later
  updateMem (\ctx -> ctx { scValidatedStash = Just validatedState })

  -- Build context for mutation adversary
  let mergedState = vsMergedState validatedState
      mutationCtx = MutationTemplateCtx
        { mtcImplPath = spec.specImplPath
        , mtcTestPath = spec.specTestPath
        , mtcFunctions = getFunctionsFromMergedState mergedState
        , mtcScopeLevel = Leaf  -- Single module scope
        }

  pure $ gotoChoice @"hMutationAdversary" mutationCtx


-- ════════════════════════════════════════════════════════════════════════════
-- UTILITIES
-- ════════════════════════════════════════════════════════════════════════════

-- | Parse test output into structured failures
-- TODO: Implement actual QuickCheck output parsing
parseTestOutput :: Text -> [StructuredFailure]
parseTestOutput _output = []


-- | Extract failure patterns from structured failures
extractFailurePatterns :: [StructuredFailure] -> [FailurePattern]
extractFailurePatterns failures =
  let -- Group by property name
      grouped = groupBy sfPropertyName failures
  in map toPattern grouped
  where
    groupBy :: (a -> Text) -> [a] -> [(Text, [a])]
    groupBy f xs = [(k, filter ((== k) . f) xs) | k <- nub (map f xs)]

    toPattern :: (Text, [StructuredFailure]) -> FailurePattern
    toPattern (propName, fs) = FailurePattern
      { fpSignature = propName
      , fpAffectedFns = []  -- TODO: Extract from failure messages
      , fpCategory = maybe ParseError sfFailureType (listToMaybe fs)
      , fpOccurrences = length fs
      }

    listToMaybe [] = Nothing
    listToMaybe (x:_) = Just x


-- | Merge old and new patterns, incrementing occurrence counts
mergePatterns :: [FailurePattern] -> [FailurePattern] -> [FailurePattern]
mergePatterns old new =
  let oldSigs = map fpSignature old
      newSigs = map fpSignature new
      -- Update existing patterns
      updated = map (updatePattern new) old
      -- Add truly new patterns
      added = filter (\p -> fpSignature p `notElem` oldSigs) new
  in updated ++ added
  where
    updatePattern :: [FailurePattern] -> FailurePattern -> FailurePattern
    updatePattern newPatterns oldPattern =
      case filter ((== fpSignature oldPattern) . fpSignature) newPatterns of
        [] -> oldPattern  -- Pattern no longer occurring
        (np:_) -> oldPattern { fpOccurrences = fpOccurrences oldPattern + fpOccurrences np }
