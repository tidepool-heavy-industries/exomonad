{-# LANGUAGE RecordWildCards #-}

-- | Baseline runner for types-first-dev workflow.
--
-- Runs the full workflow, captures statistics, and persists results
-- to ~/tidepool-labs/dev-runs/baseline/.
module TypesFirstDev.Baseline
  ( -- * Running
    runBaseline
  , runBaselineWithSpec

    -- * Result Construction
  , resultsToMetadata
  ) where

import Control.Monad.Freer (runM)
import Control.Monad.Freer.Reader (runReader)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)

import Tidepool.ClaudeCode.Config (mkClaudeCodeConfig, ClaudeCodeConfig(..))
import Tidepool.ClaudeCode.Effect (runClaudeCodeExecIO)
import Tidepool.Graph.Execute (callHandler, dispatchGoto)
import Tidepool.Worktree.Executor (runWorktreeIO, defaultWorktreeConfig)

import TypesFirstDev.DevRuns (persistRunMetadata)
import TypesFirstDev.Graph (TypesFirstGraph(..))
import TypesFirstDev.Handlers (typesFirstHandlers)
import TypesFirstDev.Stats
import TypesFirstDev.Types (StackSpec(..), ProjectType(..), ParallelResults(..), TestsResult(..), ImplResult(..))


-- | Run the baseline workflow with the default Stack spec.
runBaseline :: Text -> FilePath -> IO RunMetadata
runBaseline sessionName projectPath = do
  let spec = defaultStackSpec projectPath
  runBaselineWithSpec sessionName spec


-- | Run the baseline workflow with a custom spec.
runBaselineWithSpec :: Text -> StackSpec -> IO RunMetadata
runBaselineWithSpec sessionName spec = do
  startTime <- getCurrentTime

  let claudeConfig = (mkClaudeCodeConfig sessionName)
        { ccDefaultTimeout = 600 }
      worktreeConfig = defaultWorktreeConfig spec.ssProjectPath

  -- Run the workflow
  result <- runM
    . runWorktreeIO worktreeConfig
    . runClaudeCodeExecIO claudeConfig
    . runReader spec
    . runReader claudeConfig
    $ do
        let handlers = typesFirstHandlers spec
        choice <- callHandler (types handlers) spec
        dispatchGoto handlers choice

  endTime <- getCurrentTime
  let duration = realToFrac (diffUTCTime endTime startTime)

  -- Convert to metadata
  let meta = resultsToMetadata startTime duration spec result

  -- Persist to disk
  _ <- persistRunMetadata meta

  pure meta


-- | Convert ParallelResults to RunMetadata.
--
-- Extracts success/failure and stats from the workflow results.
-- Now includes actual costs from ClaudeCodeResult.
resultsToMetadata
  :: UTCTime         -- ^ Start timestamp
  -> Double          -- ^ Duration in seconds
  -> StackSpec       -- ^ Input spec
  -> ParallelResults -- ^ Workflow results
  -> RunMetadata
resultsToMetadata startTime duration spec ParallelResults{..} =
  let implSuccess = prImplResult.irBuildPassed && prImplResult.irAllFunctionsImplemented
      testsSuccess = prTestsResult.trBuildPassed && prTestsResult.trAllPropertiesWritten
      overallSuccess = implSuccess && testsSuccess

      -- Total cost from both agents
      totalCost = prTestsCost + prImplCost

      -- Create stats with actual costs from ClaudeCodeResult
      agentStats = Map.fromList
        [ ("impl", implResultToStats prImplResult prImplCost)
        , ("tests", testsResultToStats prTestsResult prTestsCost)
        ]

  in RunMetadata
    { rmRunId = generateRunId startTime
    , rmTimestamp = startTime
    , rmDurationSeconds = duration
    , rmSuccess = overallSuccess
    , rmSpec = spec
    , rmAgentStats = agentStats
    , rmTotalCost = totalCost
    , rmTotalTokens = 0  -- Token aggregation requires executor instrumentation
    , rmExperiment = Nothing
    }


-- | Convert ImplResult to AgentStats.
--
-- Includes cost from ClaudeCodeResult. Tokens require executor instrumentation.
implResultToStats :: ImplResult -> Double -> AgentStats
implResultToStats ImplResult{..} cost = AgentStats
  { asNodeName = "impl"
  , asDurationSeconds = 0  -- Not measured yet
  , asInputTokens = 0
  , asOutputTokens = 0
  , asCost = cost
  , asRetries = 0
  , asSuccess = irBuildPassed && irAllFunctionsImplemented
  , asErrorMessage = irBlocker
  }


-- | Convert TestsResult to AgentStats.
--
-- Includes cost from ClaudeCodeResult. Tokens require executor instrumentation.
testsResultToStats :: TestsResult -> Double -> AgentStats
testsResultToStats TestsResult{..} cost = AgentStats
  { asNodeName = "tests"
  , asDurationSeconds = 0
  , asInputTokens = 0
  , asOutputTokens = 0
  , asCost = cost
  , asRetries = 0
  , asSuccess = trBuildPassed && trAllPropertiesWritten
  , asErrorMessage = trBlocker
  }


-- | Default Stack experiment spec.
defaultStackSpec :: FilePath -> StackSpec
defaultStackSpec projectPath = StackSpec
  { ssProjectPath = projectPath
  , ssModuleName = "Data.Stack"
  , ssDescription = "A LIFO (last-in-first-out) stack data structure with operations: \
                    \push (add element to top), pop (remove and return top element), \
                    \peek (view top element without removing), isEmpty (check if empty)"
  , ssAcceptanceCriteria =
      [ "LIFO order: push a,b,c then pop returns c,b,a"
      , "push then pop is identity: pop (push x s) == Just (x, s)"
      , "empty stack invariants: isEmpty empty, pop empty == Nothing"
      ]
  , ssProjectType = PureLibrary
  }


-- Re-export from Types
-- (These are already exported but needed for the spec)
