-- | Types-first development workflow runner (parallel version).
--
-- This runner executes the full graph:
--   Entry(StackSpec) → types(ClaudeCode) → fork → (tests || impl) → merge → Exit
--
-- It demonstrates parallel agent execution with git worktrees for isolation.
--
-- Usage:
--   types-first-dev                    # Run default Stack experiment
--   types-first-dev --experiment url-shortener  # Run URL shortener experiment
--   types-first-dev baseline           # Run baseline and save to dev-runs/
--   types-first-dev baseline --spec stack  # Run specific spec as baseline
--
-- Environment variables:
--   PROJECT_PATH: Path to the project directory
--   PROJECT_TYPE: PureLibrary | ServantServer | CLIApp
--   MODULE_NAME: Module name to generate
--   ZELLIJ_SESSION: Zellij session name
module Main where

import Control.Monad (unless)
import Control.Monad.Freer (runM)
import Control.Monad.Freer.Error (runError)
import Control.Monad.Freer.Reader (runReader)
import Tidepool.Graph.Memory (evalMemory)
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import System.Process (readProcess)

import Tidepool.ClaudeCode.Config (mkClaudeCodeConfig, ClaudeCodeConfig(..))
import Tidepool.ClaudeCode.Effect (runClaudeCodeExecIO)
import Tidepool.Graph.Execute (callHandler, dispatchGoto)
import Tidepool.Worktree.Executor (runWorktreeIO, defaultWorktreeConfig)

-- Old graph imports
import TypesFirstDev.Baseline (runBaselineWithSpec)
import TypesFirstDev.DevRuns (runDirectory)
import TypesFirstDev.Graph (TypesFirstGraph(..))
import TypesFirstDev.Handlers (typesFirstHandlers)
import TypesFirstDev.Stats (RunMetadata(..))
import TypesFirstDev.Types (ProjectType(..), ParallelResults(..), TestsResult(..), ImplResult(..), Blocker(..))
import qualified TypesFirstDev.Types as Old (StackSpec(..), WorkflowError(..), emptySessionContext, ResumeStrategy(..))

-- Hybrid graph imports
import TypesFirstDev.Graph.Hybrid (TypesFirstGraphHybrid(..))
import TypesFirstDev.Handlers.Hybrid.Genesis (hybridGenesisHandlers)
import TypesFirstDev.Handlers.Hybrid.Effects (WorkflowError(..), initialSessionContext)
import TypesFirstDev.Types.Hybrid (StackSpec(..), HybridResult(..), MutationAdversaryResult(..), WitnessReport(..), defaultStrictness)
import TypesFirstDev.Effect.Build (runBuildIO)


-- | Main entry point.
--
-- Runs the types-first workflow to generate type definitions.
-- Supports multiple experiments via command-line args or env vars.
--
-- Modes:
--   types-first-dev                        # Run old sequential graph (default)
--   types-first-dev --hybrid               # Run new hybrid TDD graph
--   types-first-dev --experiment name      # Run specific experiment
--   types-first-dev baseline               # Run baseline evaluation
main :: IO ()
main = do
  args <- getArgs
  case args of
    ("baseline":rest) -> runBaselineCommand rest
    ("--hybrid":rest) -> runHybridCommand rest
    _ -> runExperimentCommand args


-- | Run the baseline command - saves results to dev-runs/.
runBaselineCommand :: [String] -> IO ()
runBaselineCommand args = do
  putStrLn "Types-First Development Workflow - Baseline Run"
  putStrLn "================================================"
  putStrLn ""

  -- Parse spec name from args
  let specName = parseSpecName args

  -- Get session and project path
  sessionName <- maybe "types-first-dev" T.pack <$> lookupEnv "ZELLIJ_SESSION"
  projectPath <- maybe getCurrentDirectory pure =<< lookupEnv "PROJECT_PATH"

  -- Validate zellij session exists
  validateZellijSession sessionName

  -- Get spec
  spec <- getSpec specName projectPath

  putStrLn $ "Spec: " <> specName
  putStrLn $ "Project: " <> projectPath
  putStrLn $ "Module: " <> T.unpack spec.ssModuleName
  putStrLn $ "Session: " <> T.unpack sessionName
  putStrLn ""
  putStrLn "Starting baseline run..."
  putStrLn ""

  -- Run and persist
  meta <- runBaselineWithSpec sessionName spec

  -- Print summary
  putStrLn ""
  putStrLn "=== Baseline Complete ==="
  putStrLn $ "Run ID: " <> T.unpack meta.rmRunId
  putStrLn $ "Duration: " <> show meta.rmDurationSeconds <> "s"
  putStrLn $ "Success: " <> show meta.rmSuccess
  putStrLn $ "Output: " <> runDirectory meta


-- | Parse spec name from baseline args.
parseSpecName :: [String] -> String
parseSpecName ("--spec":name:_) = name
parseSpecName ("-s":name:_) = name
parseSpecName _ = "stack"


-- | Run the normal experiment command.
runExperimentCommand :: [String] -> IO ()
runExperimentCommand args = do
  putStrLn "Types-First Development Workflow (Parallel)"
  putStrLn "============================================"
  putStrLn ""

  -- Parse experiment from command line
  let experiment = parseExperiment args

  -- Get zellij session from env or use default
  sessionName <- maybe "types-first-dev" T.pack <$> lookupEnv "ZELLIJ_SESSION"

  -- Validate that the Zellij session exists (fail fast instead of hanging)
  validateZellijSession sessionName

  let claudeConfig = (mkClaudeCodeConfig sessionName)
        { ccDefaultTimeout = 600 }  -- 10 min timeout

  -- Get project path from env or use current directory
  projectPath <- maybe getCurrentDirectory pure =<< lookupEnv "PROJECT_PATH"
  let worktreeConfig = defaultWorktreeConfig projectPath

  -- Get spec based on experiment
  spec <- getSpec experiment projectPath

  putStrLn $ "Experiment: " <> experiment
  putStrLn $ "Project: " <> projectPath
  putStrLn $ "Module: " <> T.unpack spec.ssModuleName
  putStrLn $ "Type: " <> show spec.ssProjectType
  putStrLn $ "Session: " <> T.unpack sessionName
  putStrLn ""
  putStrLn "Starting Claude Code execution..."
  putStrLn ""

  -- Run the graph with full effect stack
  -- Note: Interpreter order must match DevEffects = '[Error WorkflowError, Memory SessionContext, Reader ClaudeCodeConfig, ...]
  -- Innermost interpreter handles first effect in the list.
  result <- runM
    . runWorktreeIO worktreeConfig
    . runClaudeCodeExecIO claudeConfig
    . runReader spec              -- Reader StackSpec
    . runReader claudeConfig      -- Reader ClaudeCodeConfig
    . evalMemory Old.emptySessionContext  -- Memory SessionContext
    . runError @Old.WorkflowError -- Error WorkflowError (first in DevEffects = innermost)
    $ do
        let handlers = typesFirstHandlers spec
        choice <- callHandler (types handlers) spec
        dispatchGoto handlers choice

  case result of
    Left err -> printOldError err
    Right res -> printResult res


-- | Parse experiment name from command-line args.
parseExperiment :: [String] -> String
parseExperiment ("--experiment":name:_) = name
parseExperiment ("-e":name:_) = name
parseExperiment _ = "stack"  -- default


-- ════════════════════════════════════════════════════════════════════════════
-- HYBRID GRAPH RUNNER
-- ════════════════════════════════════════════════════════════════════════════

-- | Run the hybrid TDD graph.
runHybridCommand :: [String] -> IO ()
runHybridCommand args = do
  putStrLn "Types-First Development Workflow (Hybrid TDD Graph)"
  putStrLn "===================================================="
  putStrLn ""

  let experiment = parseExperiment args

  sessionName <- maybe "types-first-dev" T.pack <$> lookupEnv "ZELLIJ_SESSION"
  validateZellijSession sessionName

  let claudeConfig = (mkClaudeCodeConfig sessionName)
        { ccDefaultTimeout = 600 }  -- 10 min timeout

  projectPath <- maybe getCurrentDirectory pure =<< lookupEnv "PROJECT_PATH"
  let worktreeConfig = defaultWorktreeConfig projectPath

  spec <- getHybridSpec experiment projectPath

  putStrLn $ "Experiment: " <> experiment
  putStrLn $ "Project: " <> projectPath
  putStrLn $ "Module: " <> T.unpack (specModuleName spec)
  putStrLn $ "Session: " <> T.unpack sessionName
  putStrLn ""
  putStrLn "Starting hybrid TDD workflow..."
  putStrLn ""

  -- Run the hybrid graph with full effect stack
  -- Effect order matches HybridEffects = '[Error, Memory, Reader, Build, ClaudeCodeExec, Worktree, IO]
  result <- runM
    . runWorktreeIO worktreeConfig
    . runClaudeCodeExecIO claudeConfig
    . runBuildIO
    . runReader spec
    . evalMemory (initialSessionContext sessionName)
    . runError @WorkflowError
    $ do
        let handlers = hybridGenesisHandlers
        -- Entry point is hTypes, which receives the spec
        -- dispatchGoto continues until Exit is reached, returning HybridResult
        choice <- callHandler (hTypes handlers) spec
        dispatchGoto handlers choice

  case result of
    Left err -> printHybridError err
    Right res -> printHybridResult res


-- | Get the hybrid StackSpec for a given experiment.
getHybridSpec :: String -> FilePath -> IO StackSpec
getHybridSpec "stack" projectPath = pure $ StackSpec
  { specModuleName = "Data.Stack"
  , specDescription = "A LIFO (last-in-first-out) stack data structure with operations: \
                      \push (add element to top), pop (remove and return top element), \
                      \peek (view top element without removing), isEmpty (check if empty)"
  , specAcceptanceCriteria =
      [ "LIFO order: push a,b,c then pop returns c,b,a"
      , "push then pop is identity: pop (push x s) == Just (x, s)"
      , "empty stack invariants: isEmpty empty, pop empty == Nothing"
      ]
  , specImplPath = projectPath <> "/src/Data/Stack.hs"
  , specTestPath = projectPath <> "/test/Data/StackSpec.hs"
  , specStrictness = defaultStrictness
  }

getHybridSpec "url-shortener" projectPath = pure $ StackSpec
  { specModuleName = "UrlShortener"
  , specDescription = T.unlines
      [ "A URL shortening service with three endpoints:"
      , "- POST /shorten: Accept a JSON body with 'url' field, return a short URL"
      , "- GET /:code: Expand a short code to the original URL"
      , "- GET /:code/stats: Get usage statistics (hit count) for a short code"
      , ""
      , "The service should store mappings in memory using an IORef with a Map."
      , "Short codes should be generated deterministically (e.g., hash-based) so that"
      , "the same URL always produces the same short code."
      ]
  , specAcceptanceCriteria =
      [ "Roundtrip: POST /shorten with a URL, then GET /:code returns the original URL"
      , "Idempotent: POSTing the same URL twice returns the same short code"
      , "Stats increment: Each GET /:code request increments the hit count in stats"
      , "Not found: GET with an invalid code returns HTTP 404"
      , "Validation: POST with invalid/empty URL returns HTTP 400"
      ]
  , specImplPath = projectPath <> "/src/UrlShortener.hs"
  , specTestPath = projectPath <> "/test/Main.hs"
  , specStrictness = defaultStrictness
  }

getHybridSpec experiment _ = do
  putStrLn $ "ERROR: Unknown experiment: " <> experiment
  putStrLn "Available experiments: stack, url-shortener"
  exitFailure


-- | Print a hybrid workflow error.
printHybridError :: WorkflowError -> IO ()
printHybridError err = do
  putStrLn ""
  putStrLn "=== Hybrid Workflow Error ==="
  putStrLn ""
  case err of
    BuildFailed msg -> do
      putStrLn "Build failed:"
      putStrLn $ T.unpack msg
    TypeCheckFailed msg -> do
      putStrLn "Type check failed:"
      putStrLn $ T.unpack msg
    MaxAttemptsExceeded n -> do
      putStrLn $ "Max fix attempts exceeded: " <> show n
    HoleFixFailed msg -> do
      putStrLn "Hole fix failed:"
      putStrLn $ T.unpack msg
    StuckOnPattern msg -> do
      putStrLn "Stuck on failure pattern:"
      putStrLn $ T.unpack msg
    NotConverging -> do
      putStrLn "Fixes not converging - stopping to avoid infinite loop"
  putStrLn ""
  exitFailure


-- | Print the hybrid results.
printHybridResult :: HybridResult -> IO ()
printHybridResult hr = do
  putStrLn ""
  putStrLn "=== Hybrid TDD Complete ==="
  putStrLn ""
  putStrLn $ "Success: " <> show hr.hrSuccess
  putStrLn $ "Functions: " <> show (length hr.hrSpec)
  putStrLn $ "Total cost: $" <> show hr.hrTotalCost
  putStrLn ""
  putStrLn "Mutation Testing:"
  case hr.hrMutationAdversary of
    Nothing -> putStrLn "  Not performed"
    Just mar -> putStrLn $ "  Verdict: " <> show mar.marVerdict
  putStrLn ""
  let concerns = hr.hrWitness.wrConcerns
  unless (null concerns) $ do
    putStrLn "Concerns:"
    mapM_ (\c -> putStrLn $ "  - " <> T.unpack c) concerns
    putStrLn ""


-- ════════════════════════════════════════════════════════════════════════════
-- OLD GRAPH SUPPORT
-- ════════════════════════════════════════════════════════════════════════════

-- | Get the spec for a given experiment (OLD graph).
getSpec :: String -> FilePath -> IO Old.StackSpec
getSpec "stack" projectPath = pure $ Old.StackSpec
  { Old.ssProjectPath = projectPath
  , Old.ssModuleName = "Data.Stack"
  , Old.ssDescription = "A LIFO (last-in-first-out) stack data structure with operations: \
                    \push (add element to top), pop (remove and return top element), \
                    \peek (view top element without removing), isEmpty (check if empty)"
  , Old.ssAcceptanceCriteria =
      [ "LIFO order: push a,b,c then pop returns c,b,a"
      , "push then pop is identity: pop (push x s) == Just (x, s)"
      , "empty stack invariants: isEmpty empty, pop empty == Nothing"
      ]
  , Old.ssProjectType = PureLibrary
  , Old.ssResumeStrategy = Old.SmartResume
  }

getSpec "url-shortener" projectPath = pure $ Old.StackSpec
  { Old.ssProjectPath = projectPath
  , Old.ssModuleName = "UrlShortener"
  , Old.ssDescription = T.unlines
      [ "A URL shortening service with three endpoints:"
      , "- POST /shorten: Accept a JSON body with 'url' field, return a short URL"
      , "- GET /:code: Expand a short code to the original URL"
      , "- GET /:code/stats: Get usage statistics (hit count) for a short code"
      , ""
      , "The service should store mappings in memory using an IORef with a Map."
      , "Short codes should be generated deterministically (e.g., hash-based) so that"
      , "the same URL always produces the same short code."
      ]
  , Old.ssAcceptanceCriteria =
      [ "Roundtrip: POST /shorten with a URL, then GET /:code returns the original URL"
      , "Idempotent: POSTing the same URL twice returns the same short code"
      , "Stats increment: Each GET /:code request increments the hit count in stats"
      , "Not found: GET with an invalid code returns HTTP 404"
      , "Validation: POST with invalid/empty URL returns HTTP 400"
      ]
  , Old.ssProjectType = ServantServer
  , Old.ssResumeStrategy = Old.SmartResume
  }

getSpec experiment _ = do
  putStrLn $ "ERROR: Unknown experiment: " <> experiment
  putStrLn "Available experiments: stack, url-shortener"
  exitFailure


-- | Print a workflow error (OLD graph).
printOldError :: Old.WorkflowError -> IO ()
printOldError err = do
  putStrLn ""
  putStrLn "=== Workflow Error ==="
  putStrLn ""
  case err of
    Old.SkeletonCompileFailed msg -> do
      putStrLn "Skeleton compilation failed:"
      putStrLn $ T.unpack msg
    Old.WorktreeCreationFailed msg -> do
      putStrLn "Worktree creation failed:"
      putStrLn $ T.unpack msg
    Old.AgentFailed name msg -> do
      putStrLn $ "Agent '" <> T.unpack name <> "' failed:"
      putStrLn $ T.unpack msg
    Old.AgentNoOutput name -> do
      putStrLn $ "Agent '" <> T.unpack name <> "' returned no output"
    Old.AgentParseFailed name msg -> do
      putStrLn $ "Agent '" <> T.unpack name <> "' parse failed:"
      putStrLn $ T.unpack msg
    Old.MergeFailed msg -> do
      putStrLn "Merge failed:"
      putStrLn $ T.unpack msg
  putStrLn ""
  exitFailure


-- | Print the parallel results.
--
-- Now prints metadata (commit messages, design notes) instead of code.
-- Code lives in the project files after agents edit them.
printResult :: ParallelResults -> IO ()
printResult pr = do
  putStrLn ""
  putStrLn "=== Parallel Results ==="
  putStrLn ""

  -- Impl result (build status is mechanical - handler already verified)
  putStrLn "Implementation:"
  putStrLn $ "  Function rubrics: " <> show (length pr.prImplResult.irFunctionRubrics)
  putStrLn $ "  Commit msg: " <> T.unpack pr.prImplResult.irCommitMessage
  putStrLn $ "  Design notes: " <> T.unpack pr.prImplResult.irDesignNotes
  case pr.prImplResult.irBlocker of
    Nothing -> pure ()
    Just blocker -> putStrLn $ "  BLOCKED: [" <> T.unpack (blCategory blocker) <> "] " <> T.unpack (blDescription blocker)
  putStrLn ""

  -- Tests result (build status is mechanical - handler already verified)
  putStrLn "Tests:"
  putStrLn $ "  Function rubrics: " <> show (length pr.prTestsResult.trFunctionRubrics)
  putStrLn $ "  Commit msg: " <> T.unpack pr.prTestsResult.trCommitMessage
  putStrLn $ "  Strategy: " <> T.unpack pr.prTestsResult.trTestingStrategy
  case pr.prTestsResult.trBlocker of
    Nothing -> pure ()
    Just blocker -> putStrLn $ "  BLOCKED: [" <> T.unpack (blCategory blocker) <> "] " <> T.unpack (blDescription blocker)
  putStrLn ""

  putStrLn "Work committed and merged. Check git log for agent commits."


-- | Validate that the Zellij session exists before proceeding.
--
-- Fails fast with a clear error message if the session doesn't exist,
-- rather than hanging indefinitely waiting for a non-existent session.
validateZellijSession :: T.Text -> IO ()
validateZellijSession sessionName = do
  sessions <- readProcess "zellij" ["list-sessions"] ""
  -- Strip ANSI codes and extract just the session name (first word of each line)
  let sessionLines = lines sessions
      -- Session name is the first word after stripping ANSI codes
      extractName line = case words (stripAnsi line) of
        (name:_) -> Just name
        [] -> Nothing
      knownSessions = mapMaybe extractName $ filter (not . null) sessionLines
      sessionExists = T.unpack sessionName `elem` knownSessions
  unless sessionExists $ do
    putStrLn ""
    putStrLn "ERROR: Zellij session not found!"
    putStrLn ""
    putStrLn $ "  Expected session: " <> T.unpack sessionName
    putStrLn ""
    putStrLn "Available sessions:"
    mapM_ (\s -> putStrLn $ "  - " <> s) knownSessions
    putStrLn ""
    putStrLn "To create the session, run:"
    putStrLn $ "  zellij --session " <> T.unpack sessionName
    putStrLn ""
    exitFailure

-- | Strip ANSI escape codes from a string.
stripAnsi :: String -> String
stripAnsi [] = []
stripAnsi ('\ESC':'[':rest) = stripAnsi $ drop 1 $ dropWhile (/= 'm') rest
stripAnsi (c:rest) = c : stripAnsi rest
