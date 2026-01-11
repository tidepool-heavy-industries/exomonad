{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Impl node - makes failing tests pass, self-loops on retry.
module TypesFirstDev.Handlers.Impl
  ( ImplInput(..)
  , ImplExit(..)
  , implBefore
  , implAfter
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import Tidepool.Effect.Freer (Eff, Member)
import Tidepool.Effect.Session (Session, SessionOperation(..), SessionId)
import Tidepool.Graph.Goto (To, GotoChoice, gotoChoice, gotoExit, gotoSelf)
import Tidepool.Graph.Memory (Memory, getMem, updateMem)
import Tidepool.StructuredOutput (StructuredOutput)
import Tidepool.StructuredOutput.ClaudeCodeSchema (ClaudeCodeSchema(..))
import Tidepool.StructuredOutput.DecisionTools (ToDecisionTools(..))

import TypesFirstDev.Context (ImplTemplateCtx(..))
import TypesFirstDev.Types.Core (Spec)
import TypesFirstDev.Types.Shared (Critique)
import TypesFirstDev.Types.Payloads (InitWorkPayload, TestsReadyPayload, ImplResult(..), MergeComplete)
import TypesFirstDev.Types.Memory (ImplMem(..))
import TypesFirstDev.Handlers.TDDReviewImpl (TDDReviewImplInput)

-- ════════════════════════════════════════════════════════════════════════════
-- TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Impl node input.
data ImplInput = ImplInput
  { iiSpec :: Spec
    -- ^ Work specification
  , iiScaffold :: InitWorkPayload
    -- ^ Scaffold output (interface + contract suite)
  , iiTestsReady :: TestsReadyPayload
    -- ^ TDD write tests result
  , iiChildMerges :: Maybe [MergeComplete]
    -- ^ Merged child results if any
  , iiAttemptCount :: Int
    -- ^ Current retry attempt (1-5)
  , iiCritiqueList :: Maybe [Critique]
    -- ^ TDD review critiques to address
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Impl node output (oneOf).
data ImplExit
  = ImplTestsPassed
      { ieCommitHash :: Text
        -- ^ Commit hash after all tests pass
      , ieIterations :: Int
        -- ^ Number of iterations to reach passing state
      , iePassedTests :: [Text]
        -- ^ Tests that now pass
      }
  | ImplRequestRetry
      { ieDiagnosis :: Text
        -- ^ Why tests are still failing
      , ieStrategyFrom :: Text
        -- ^ Current approach
      , ieStrategyTo :: Text
        -- ^ Suggested new approach
      , ieFailingTests :: [Text]
        -- ^ Which tests still fail
      }
  | ImplBlockedDependency
      { ieMissingSymbol :: Text
        -- ^ Symbol that needs to be imported
      , ieExpectedImportPath :: FilePath
        -- ^ Where we expect to find it
      }
  | ImplSpecAmbiguity
      { ieSpecSentence :: Text
        -- ^ Confusing sentence from spec
      , ieContradictionTrace :: Text
        -- ^ How it conflicts with other parts
      , ieQuestion :: Text
        -- ^ What clarification is needed
      }
  | ImplStuck
      { ieStuckDiagnosis :: Text
        -- ^ Why we're stuck
      , ieStuckRecommendation :: Text
        -- ^ Suggested next step
      , ieStuckAttempts :: Int
        -- ^ How many attempts made
      }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput, ToDecisionTools)

instance ClaudeCodeSchema ImplExit where
  ccDecisionTools = Just (toDecisionTools @ImplExit)
  ccParseToolCall = parseToolCall @ImplExit

-- ════════════════════════════════════════════════════════════════════════════
-- HANDLERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Before handler: build context, manage private memory.
implBefore
  :: (Member Session es, Member (Memory ImplMem) es)
  => ImplInput
  -> Eff es (ImplTemplateCtx, SessionOperation)
implBefore input = do
  mem <- getMem @ImplMem
  let ctx = ImplTemplateCtx
        { iiSpec = input.iiSpec
        , iiScaffold = input.iiScaffold
        , iiTestsReady = input.iiTestsReady
        , iiChildMerges = input.iiChildMerges
        , iiAttemptCount = input.iiAttemptCount
        , iiCritiqueList = input.iiCritiqueList
        }
  let sessionOp = case mem.implSessionId of
        Just sid -> ContinueFrom sid
        Nothing -> StartFresh "v3/impl"
  pure (ctx, sessionOp)

-- | After handler: self-loop on retry, route to review or exit.
-- Max 5 retry attempts.
implAfter
  :: (Member Session es, Member (Memory ImplMem) es)
  => (ImplExit, SessionId)
  -> Eff es (GotoChoice '[To Self ImplInput, To "v3TDDReviewImpl" TDDReviewImplInput, Goto Exit ImplExit])
implAfter (exit, sid) input = do
  updateMem @ImplMem $ \m -> m { implSessionId = Just sid }
  case exit of
    ImplTestsPassed commit iterations tests -> do
      let reviewInput = TDDReviewImplInput
            { triSpec = input.iiSpec
            , triScaffold = input.iiScaffold
            , triImplResult = ImplResult commit iterations tests
            , triDiff = ""
            }
      pure $ gotoChoice @"v3TDDReviewImpl" reviewInput

    ImplRequestRetry diagnosis _from _to _failing
      | input.iiAttemptCount >= 5 ->
          pure $ gotoExit (ImplStuck diagnosis "Max retries exceeded" 5)
      | otherwise -> do
          updateMem @ImplMem $ \m -> m { implAttempt = m.implAttempt + 1 }
          let retryInput = input { iiAttemptCount = input.iiAttemptCount + 1 }
          pure $ gotoSelf retryInput

    ImplBlockedDependency _missing _path ->
      error "TODO: Route back to Scaffold for clarification"

    ImplSpecAmbiguity _spec _contradiction _q ->
      error "TODO: Route back to Scaffold for clarification"

    ImplStuck diagnosis recommendation attempts ->
      pure $ gotoExit (ImplStuck diagnosis recommendation attempts)
