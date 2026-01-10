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
import Control.Monad.Freer.Error (throwError)
import Control.Monad.Freer.Reader (ask)
import Data.Char (isAlphaNum)
import Data.List (nub)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Tidepool.Effect.Session (SessionId, SessionOperation(..))
import Tidepool.Graph.Goto
  ( GotoChoice
  , To
  , ClaudeCodeLLMHandler(..)
  , ClaudeCodeResult(..)
  , gotoChoice
  )
import Tidepool.Graph.Memory (getMem, updateMem)
import Tidepool.Graph.Types (ModelChoice(..))

import TypesFirstDev.Effect.Build (testWithDetails)
import qualified TypesFirstDev.Effect.Build as Build
import TypesFirstDev.Types.Hybrid
import TypesFirstDev.Handlers.Hybrid.Effects (HybridEffects, SessionContext(..), WorkflowError(..))
import TypesFirstDev.Templates.Hybrid (hFixCompiled)


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

  -- Run tests via Build effect
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


-- | Local test result type for routing decisions.
data LocalTestResult
  = TestsPassed Int  -- number of tests
  | TestsFailed [StructuredFailure]

-- | Run tests in the given worktree using Build effect.
runTests :: FilePath -> Eff HybridEffects LocalTestResult
runTests worktree = do
  sendM $ putStrLn $ "[VALIDATE] Running: cabal test --test-show-details=always"

  result <- testWithDetails worktree

  if Build.trSuccess result
    then do
      sendM $ putStrLn "[VALIDATE] Test run succeeded"
      pure $ TestsPassed (Build.trPassedCount result)
    else do
      sendM $ putStrLn $ "[VALIDATE] Test run failed (" <> show (Build.trFailedCount result) <> " failures)"
      -- Convert Build.TestFailure to StructuredFailure
      let failures = map convertFailure (Build.trFailures result)
      if null failures
        then do
          -- Build failure, not test failure - create a parse error
          sendM $ putStrLn "[VALIDATE] No test failures parsed, likely build error"
          pure $ TestsFailed [StructuredFailure
            { sfPropertyName = "build"
            , sfFailureType = ParseError
            , sfCounterexample = Nothing
            , sfExpected = Nothing
            , sfActual = Nothing
            , sfMessage = T.take 500 (Build.trOutput result <> Build.trErrors result)
            }]
        else pure $ TestsFailed failures

-- | Convert Build.TestFailure to StructuredFailure
convertFailure :: Build.TestFailure -> StructuredFailure
convertFailure bf = StructuredFailure
  { sfPropertyName = Build.tfName bf
  , sfFailureType = PropertyFailed  -- Default; Build effect doesn't distinguish
  , sfCounterexample = Build.tfCounterexample bf
  , sfExpected = Nothing
  , sfActual = Nothing
  , sfMessage = Build.tfMessage bf
  }


-- ════════════════════════════════════════════════════════════════════════════
-- FIX HANDLER (LLM Node with ClaudeCode)
-- ════════════════════════════════════════════════════════════════════════════

-- | Handler for hFix node.
-- Checks exit conditions, fixes implementation based on test failures.
--
-- Uses ClaudeCodeLLMHandler pattern:
-- - Before: Check exit conditions, stash ValidationFailure
-- - LLM: ClaudeCode analyzes and fixes failures
-- - After: Update understanding state, route back to validate
hFixHandler
  :: ClaudeCodeLLMHandler
       'Haiku                                -- model
       ValidationFailure                     -- needs
       FixAgentOutput                        -- schema
       '[To "hValidate" MergedState]         -- targets
       HybridEffects                         -- effs
       FixTemplateCtx                        -- tpl
hFixHandler = ClaudeCodeLLMHandler @'Haiku
  Nothing         -- no system template
  hFixCompiled    -- user template
  buildFixContext -- before: checks exit conditions, builds context
  routeAfterFix   -- after: updates understanding, routes back
  where
    buildFixContext :: ValidationFailure -> Eff HybridEffects (FixTemplateCtx, SessionOperation)
    buildFixContext validationFailure = do
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

      -- Stash ValidationFailure for after-handler to retrieve
      updateMem (\ctx -> ctx { scValidationFailureStash = Just validationFailure })

      let templateCtx = FixTemplateCtx
            { failures = vfFailures validationFailure
            , understanding = understanding
            , fixFunctions = getFunctionsFromMergedState mergedState
            , worktreePath = msMergeWorktree mergedState
            }
      pure (templateCtx, StartFresh "validation/fix")

    routeAfterFix :: (ClaudeCodeResult FixAgentOutput, SessionId) -> Eff HybridEffects (GotoChoice '[To "hValidate" MergedState])
    routeAfterFix (ccResult, _sid) = do
      -- Retrieve stashed ValidationFailure
      ctx <- getMem @SessionContext
      case ctx.scValidationFailureStash of
        Nothing -> error "BUG: ValidationFailure not stashed before LLM call"
        Just validationFailure -> do
          let mergedState = vfMergedState validationFailure
              understanding = mergedState.msUnderstanding
              output = ccResult.ccrParsedOutput
              attemptNum = length (usFixesApplied understanding) + 1

          -- Update understanding with applied fixes
          let updatedUnderstanding = understanding
                { usFixesApplied = usFixesApplied understanding ++ fixChanges output
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

-- | Parse test output into structured failures.
-- Handles QuickCheck/HSpec output format.
--
-- QuickCheck failure format:
-- @
-- *** Failed! Falsified (after 5 tests):
-- [1,2,3]
-- prop_pushPopInverse:                             FAIL
-- @
parseTestOutput :: Text -> [StructuredFailure]
parseTestOutput output =
  let blocks = T.splitOn "*** Failed!" output
      -- Skip the first block (before any failures)
      failureBlocks = drop 1 blocks
  in mapMaybe parseFailureBlock failureBlocks

-- | Parse a single failure block into a StructuredFailure
parseFailureBlock :: Text -> Maybe StructuredFailure
parseFailureBlock block =
  let lines' = T.lines block
      -- First line contains the failure message (e.g., "Falsified (after 5 tests):")
      msg = maybe "" T.strip (listToMaybe lines')
      -- Extract property name from the block
      propName = extractPropertyName block
      -- Extract counterexample from lines after the message
      counterex = extractCounterexample (drop 1 lines')
      -- Determine failure type from message
      failType = detectFailureType msg
  in if T.null propName
     then Nothing
     else Just StructuredFailure
       { sfPropertyName = propName
       , sfFailureType = failType
       , sfCounterexample = counterex
       , sfExpected = Nothing
       , sfActual = Nothing
       , sfMessage = T.take 200 msg
       }

-- | Extract property name from failure block.
-- Looks for patterns like "prop_pushPopInverse" or "property: pushPopInverse"
extractPropertyName :: Text -> Text
extractPropertyName block =
  -- Look for prop_ prefix
  case T.breakOn "prop_" block of
    (_, rest) | not (T.null rest) ->
      let propWithPrefix = T.takeWhile isAlphaNumOrUnderscore rest
      in if T.null propWithPrefix then fallbackExtract block else propWithPrefix
    _ -> fallbackExtract block
  where
    isAlphaNumOrUnderscore c = isAlphaNum c || c == '_'

    -- Fallback: look for ":property" or "Test:" patterns
    fallbackExtract blk =
      let lines' = T.lines blk
          testLines = filter hasTestName lines'
      in maybe "unknown" extractTestName (listToMaybe testLines)

    hasTestName line =
      "FAIL" `T.isInfixOf` line ||
      "Test:" `T.isInfixOf` line

    extractTestName line =
      -- Try to extract the test name from patterns like "test_name: FAIL"
      let stripped = T.strip line
          colonParts = T.splitOn ":" stripped
      in maybe "unknown" T.strip (listToMaybe colonParts)

-- | Extract counterexample from failure output lines.
-- QuickCheck shows the counterexample on lines after the failure message.
extractCounterexample :: [Text] -> Maybe Text
extractCounterexample lines' =
  let -- Filter out non-counterexample lines
      counterexLines = filter isCounterexampleLine lines'
      combined = T.intercalate "\n" (take 3 counterexLines)
  in if T.null combined then Nothing else Just combined
  where
    isCounterexampleLine line =
      let stripped = T.strip line
      in not (T.null stripped) &&
         not ("prop_" `T.isPrefixOf` stripped) &&
         not ("***" `T.isPrefixOf` stripped) &&
         not ("====" `T.isPrefixOf` stripped) &&
         not ("FAIL" `T.isSuffixOf` stripped) &&
         not ("PASS" `T.isSuffixOf` stripped) &&
         not ("Use --seed" `T.isPrefixOf` stripped)

-- | Detect failure type from message.
detectFailureType :: Text -> FailureType
detectFailureType msg
  | "Falsified" `T.isInfixOf` msg = PropertyFailed
  | "Exception" `T.isInfixOf` msg = ExceptionThrown
  | "error" `T.isInfixOf` T.toLower msg = ExceptionThrown
  | "Timeout" `T.isInfixOf` msg = Timeout
  | "undefined" `T.isInfixOf` T.toLower msg = UndefinedHit
  | "Prelude.undefined" `T.isInfixOf` msg = UndefinedHit
  | otherwise = PropertyFailed  -- Default to PropertyFailed for QuickCheck

-- | Safe head
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x


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
