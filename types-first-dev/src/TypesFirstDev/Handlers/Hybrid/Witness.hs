{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Witness handler for WS4 of the hybrid TDD graph.
--
-- The witness observes the entire workflow and produces a coherent
-- narrative of what happened. It also assembles the final HybridResult.
module TypesFirstDev.Handlers.Hybrid.Witness
  ( -- * Handler
    hWitnessHandler

    -- * Utilities
  , buildNarrative
  , assembleHybridResult
  ) where

import Control.Monad.Freer (Eff, sendM)
import Control.Monad.Freer.Reader (Reader, ask)
import Data.Text (Text)
import qualified Data.Text as T

import Tidepool.Graph.Goto (GotoChoice, To, gotoExit)
import Tidepool.Graph.Memory (Memory, getMem)
import Tidepool.Graph.Types (Exit)

import TypesFirstDev.Types.Hybrid
import TypesFirstDev.Handlers.Hybrid.Effects (HybridEffects, SessionContext(..))


-- | Create a stub coverage report.
-- With simplified schema, we no longer have structured property data.
-- TODO: Restore coverage computation when schema is re-enriched.
stubCoverageReport :: CoverageReport
stubCoverageReport = CoverageReport
  { crFunctionsCovered = []
  , crFunctionsUncovered = []
  , crExamplesCovered = 0
  , crExamplesTotal = 0
  , crSketchesCovered = []
  , crSketchesUncovered = []
  }


-- ════════════════════════════════════════════════════════════════════════════
-- WITNESS HANDLER (Logic Node)
-- ════════════════════════════════════════════════════════════════════════════

-- | Handler for hWitness node.
-- Compiles the final narrative and assembles HybridResult.
--
-- The witness observes all LLM nodes and provides:
-- - Coherent narrative of what happened
-- - Aggregated concerns
-- - Actionable suggestions
hWitnessHandler
  :: WitnessReport
  -> Eff HybridEffects (GotoChoice '[To Exit HybridResult])
hWitnessHandler partialReport = do
  spec <- ask @StackSpec
  ctx <- getMem @SessionContext

  sendM $ putStrLn "[WITNESS] Compiling final result..."

  -- Retrieve stashed state
  let maybeValidated = scValidatedStash ctx
      maybeMutation = scMutationResultStash ctx

  case maybeValidated of
    Nothing -> error "BUG: ValidatedState not stashed before witness"
    Just validatedState -> do
      -- Build the complete narrative
      let narrative = buildNarrative validatedState maybeMutation partialReport

      -- Enhance the witness report with narrative
      let finalReport = partialReport
            { wrNarrative = narrative
            }

      -- Assemble the final result
      let result = assembleHybridResult spec validatedState maybeMutation finalReport

      sendM $ putStrLn $ "[WITNESS] Workflow " <> if hrSuccess result then "SUCCEEDED" else "FAILED"
      sendM $ putStrLn $ "[WITNESS] Total cost: $" <> show (hrTotalCost result)

      pure $ gotoExit result


-- | Build a coherent narrative of the workflow
buildNarrative :: ValidatedState -> Maybe MutationAdversaryResult -> WitnessReport -> Text
buildNarrative validatedState maybeMutation partialReport =
  T.unlines
    [ "## Workflow Summary"
    , ""
    , "### Validation"
    , "All " <> T.pack (show $ vsTestsPassed validatedState) <> " tests passed after validation."
    , ""
    , "### Understanding Accumulated"
    , formatUnderstanding (msUnderstanding $ vsMergedState validatedState)
    , ""
    , "### Mutation Testing"
    , formatMutationResults maybeMutation
    , ""
    , "### Concerns"
    , if null (wrConcerns partialReport)
        then "No significant concerns."
        else T.unlines (map ("- " <>) $ wrConcerns partialReport)
    , ""
    , "### Suggestions"
    , if null (wrSuggestions partialReport)
        then "No suggestions at this time."
        else T.unlines (map ("- " <>) $ wrSuggestions partialReport)
    ]


-- | Format understanding state for narrative
formatUnderstanding :: UnderstandingState -> Text
formatUnderstanding us
  | null (usFixesApplied us) && null (usLearnings us) = "No fixes were needed; tests passed on first attempt."
  | otherwise = T.unlines
      [ "Fixes applied: " <> T.pack (show $ length $ usFixesApplied us)
      , "Learnings: " <> T.intercalate "; " (usLearnings us)
      ]


-- | Format mutation results for narrative
formatMutationResults :: Maybe MutationAdversaryResult -> Text
formatMutationResults Nothing = "Mutation testing was not performed."
formatMutationResults (Just result) =
  let output = marOutput result
      verdict = marVerdict result
      mutationsAttempted = length (mutationTypesAttempted output)
  in T.unlines
      [ "Mutations attempted: " <> T.pack (show mutationsAttempted)
      , "Survivors: " <> T.pack (show $ length $ mutSurvivors output)
      , "Verdict: " <> formatVerdict verdict
      ]


-- | Format verdict for display
formatVerdict :: TestSuiteVerdict -> Text
formatVerdict = \case
  TestSuiteRobust -> "ROBUST - Test suite caught all mutations"
  TestSuiteHasGaps -> "HAS GAPS - Some mutations survived"
  TestSuiteWeak -> "WEAK - Many mutations survived (>50%)"


-- | Assemble the final HybridResult
assembleHybridResult
  :: StackSpec
  -> ValidatedState
  -> Maybe MutationAdversaryResult
  -> WitnessReport
  -> HybridResult
assembleHybridResult spec validatedState maybeMutation witnessReport =
  let mergedState = vsMergedState validatedState
      verifiedResults = msVerifiedResults mergedState
      blindResults = vrBlindResults verifiedResults
      testsResult = brTests blindResults
      testsAgentOut = TypesFirstDev.Types.Hybrid.testsOutput testsResult

      -- Coverage is stubbed until schema is re-enriched with structured property data
      coverage = stubCoverageReport

      -- Determine success based on strictness config
      success = case maybeMutation of
        Nothing -> True  -- No mutation testing = success
        Just result ->
          if scMutationBlocking (specStrictness spec)
            then marVerdict result == TestSuiteRobust  -- Blocking mode: need 0 survivors
            else True  -- Advisory mode: always succeed

  in HybridResult
      { hrSuccess = success
      , hrSpec = []  -- TODO: Get from original StackSpec or testsOutput
      , hrTestsCoverage = coverage
      , hrUnderstanding = msUnderstanding mergedState
      , hrTypeAdversary = Nothing  -- Would come from earlier phase
      , hrMutationAdversary = maybeMutation
      , hrWitness = witnessReport
      , hrTotalCost = 0.0  -- TODO: Track cost through context
      }
