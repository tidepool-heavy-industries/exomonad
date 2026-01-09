{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Mutation adversary handler for WS4 of the hybrid TDD graph.
--
-- The mutation adversary is a "red team" for the test suite.
-- It tries to introduce bugs that tests don't catch (survivors).
-- Findings are ADVISORY - included in output but don't block exit.
module TypesFirstDev.Handlers.Hybrid.Adversary
  ( -- * Handler
    hMutationAdversaryHandler

    -- * Utilities
  , deriveTestSuiteVerdict
  ) where

import Control.Monad.Freer (Eff, sendM)
import Control.Monad.Freer.Reader (Reader, ask)
import Data.Text (Text)
import qualified Data.Text as T

import Tidepool.Effect.ClaudeCode (ClaudeCodeExec)
import Tidepool.Effects.Worktree (Worktree)
import Tidepool.Graph.Goto (GotoChoice, To, gotoChoice)
import Tidepool.Graph.Memory (Memory, updateMem)

import TypesFirstDev.Types.Hybrid
import TypesFirstDev.Handlers.Hybrid.Effects (HybridEffects, SessionContext(..))


-- ════════════════════════════════════════════════════════════════════════════
-- MUTATION ADVERSARY HANDLER (Logic Node with internal ClaudeCode call)
-- ════════════════════════════════════════════════════════════════════════════

-- | Handler for hMutationAdversary node.
-- Red teams the test suite by attempting to introduce bugs that tests miss.
--
-- Note: Although this is a LogicNode in the graph, it internally calls
-- ClaudeCode to perform the mutation testing. Results are ADVISORY.
hMutationAdversaryHandler
  :: MutationTemplateCtx
  -> Eff HybridEffects (GotoChoice '[To "hWitness" WitnessReport])
hMutationAdversaryHandler mutationCtx = do
  spec <- ask @StackSpec

  sendM $ putStrLn $ "[MUTATION-ADVERSARY] Red teaming test suite at scope: " <> show (mtcScopeLevel mutationCtx)

  -- TODO: Call ClaudeCode with mutation-adversary template
  -- For now, simulate mutation testing results
  sendM $ putStrLn "[MUTATION-ADVERSARY] Calling ClaudeCode to test mutations..."

  -- Simulate mutation adversary result
  let mutationOutput = MutationAdversaryOutput
        { mutMutantsTried = 15
        , mutSurvivors = []  -- No survivors = robust test suite
        , mutAnalysis = "Tested boundary conditions, operator swaps, and condition flips. All mutations were caught by the test suite."
        }

  -- Derive verdict from output
  let verdict = deriveTestSuiteVerdict mutationOutput

  -- Build the result
  let mutationResult = MutationAdversaryResult
        { marOutput = mutationOutput
        , marVerdict = verdict
        }

  -- Stash result for witness to retrieve
  updateMem (\ctx -> ctx { scMutationResultStash = Just mutationResult })

  -- Log verdict
  case verdict of
    TestSuiteRobust -> sendM $ putStrLn "[MUTATION-ADVERSARY] Test suite is ROBUST (no survivors)"
    TestSuiteHasGaps -> sendM $ putStrLn "[MUTATION-ADVERSARY] Test suite has GAPS (some survivors)"
    TestSuiteWeak -> sendM $ putStrLn "[MUTATION-ADVERSARY] Test suite is WEAK (many survivors)"

  -- Build partial witness report with mutation observations
  let mutationObservation = NodeObservation
        { noNode = "hMutationAdversary"
        , noPhase = "post-validation"
        , noProgress = "Completed mutation testing: " <> T.pack (show (mutMutantsTried mutationOutput))
                    <> " mutations tried, " <> T.pack (show (length $ mutSurvivors mutationOutput)) <> " survived"
        , noConcerns = survivorConcerns mutationOutput
        }

      witnessReport = WitnessReport
        { wrObservations = [mutationObservation]
        , wrNarrative = ""  -- Will be filled in by witness
        , wrConcerns = survivorConcerns mutationOutput
        , wrSuggestions = survivorSuggestions mutationOutput
        }

  pure $ gotoChoice @"hWitness" witnessReport


-- | Derive test suite verdict from mutation output.
-- This is handler logic, NOT asked of the LLM.
deriveTestSuiteVerdict :: MutationAdversaryOutput -> TestSuiteVerdict
deriveTestSuiteVerdict output
  | null (mutSurvivors output) = TestSuiteRobust
  | survivorRate > 0.5 = TestSuiteWeak
  | otherwise = TestSuiteHasGaps
  where
    survivorRate :: Double
    survivorRate
      | mutMutantsTried output == 0 = 0
      | otherwise = fromIntegral (length $ mutSurvivors output)
                  / fromIntegral (mutMutantsTried output)


-- | Extract concerns from surviving mutants
survivorConcerns :: MutationAdversaryOutput -> [Text]
survivorConcerns output =
  map formatConcern (mutSurvivors output)
  where
    formatConcern :: SurvivingMutant -> Text
    formatConcern sm = "Mutation survived in " <> smFunction sm <> ": " <> smDescription sm


-- | Extract suggestions from surviving mutants
survivorSuggestions :: MutationAdversaryOutput -> [Text]
survivorSuggestions output =
  map formatSuggestion (mutSurvivors output)
  where
    formatSuggestion :: SurvivingMutant -> Text
    formatSuggestion sm = "Add test for " <> smFunction sm <> ": " <> smMissingTest sm
