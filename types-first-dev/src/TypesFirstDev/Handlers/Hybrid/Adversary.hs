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

import Tidepool.Effects.Worktree (Worktree)
import Tidepool.Graph.Goto
  ( GotoChoice
  , To
  , ClaudeCodeLLMHandler(..)
  , ClaudeCodeResult(..)
  , gotoChoice
  )
import Tidepool.Graph.Memory (Memory, getMem, updateMem)
import Tidepool.Graph.Types (ModelChoice(..))

import TypesFirstDev.Types.Hybrid
import TypesFirstDev.Handlers.Hybrid.Effects (HybridEffects, SessionContext(..))
import TypesFirstDev.Templates.Hybrid (hMutationAdversaryCompiled)


-- ════════════════════════════════════════════════════════════════════════════
-- MUTATION ADVERSARY HANDLER (LLM Node with ClaudeCode)
-- ════════════════════════════════════════════════════════════════════════════

-- | Handler for hMutationAdversary node.
-- Red teams the test suite by attempting to introduce bugs that tests miss.
--
-- Uses ClaudeCodeLLMHandler pattern:
-- - Before: Stash context (passthrough - input IS template context)
-- - LLM: ClaudeCode performs mutation testing
-- - After: Derive verdict, stash result, build witness report
--
-- Results are ADVISORY - included in output but don't block exit.
hMutationAdversaryHandler
  :: ClaudeCodeLLMHandler
       'Haiku                                -- model
       MutationTemplateCtx                   -- needs
       MutationAdversaryOutput               -- schema
       '[To "hWitness" WitnessReport]        -- targets
       HybridEffects                         -- effs
       MutationTemplateCtx                   -- tpl
hMutationAdversaryHandler = ClaudeCodeLLMHandler @'Haiku
  Nothing                      -- no system template
  hMutationAdversaryCompiled   -- user template
  buildMutationContext         -- before: passthrough with stash
  routeAfterMutation           -- after: build witness report
  where
    buildMutationContext :: MutationTemplateCtx -> Eff HybridEffects MutationTemplateCtx
    buildMutationContext mutationCtx = do
      sendM $ putStrLn $ "[MUTATION-ADVERSARY] Red teaming test suite at scope: " <> show (mtcScopeLevel mutationCtx)

      -- Stash context for after-handler to retrieve
      updateMem (\ctx -> ctx { scMutationCtxStash = Just mutationCtx })

      -- Passthrough: input IS the template context
      pure mutationCtx

    routeAfterMutation :: ClaudeCodeResult MutationAdversaryOutput -> Eff HybridEffects (GotoChoice '[To "hWitness" WitnessReport])
    routeAfterMutation ccResult = do
      let mutationOutput = ccResult.ccrParsedOutput

      -- Derive verdict from output (handler logic, NOT asked of LLM)
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
      -- Note: mutationTypesAttempted is a list of mutation types, length gives count
      let mutationsTried = length (mutationTypesAttempted mutationOutput)
      let mutationObservation = NodeObservation
            { noNode = "hMutationAdversary"
            , noPhase = "post-validation"
            , noProgress = "Completed mutation testing: " <> T.pack (show mutationsTried)
                        <> " mutation types, " <> T.pack (show (length $ mutSurvivors mutationOutput)) <> " survived"
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
    -- Handler derives count from list length (value-neutral)
    mutantsTried = length (mutationTypesAttempted output)
    survivorRate :: Double
    survivorRate
      | mutantsTried == 0 = 0
      | otherwise = fromIntegral (length $ mutSurvivors output)
                  / fromIntegral mutantsTried


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
