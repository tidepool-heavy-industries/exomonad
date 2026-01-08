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
import Data.Maybe (mapMaybe)
import Control.Monad.Freer.Reader (runReader)
import Data.Text qualified as T
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import System.Process (readProcess)

import Tidepool.ClaudeCode.Config (mkClaudeCodeConfig, ClaudeCodeConfig(..))
import Tidepool.ClaudeCode.Effect (runClaudeCodeExecIO)
import Tidepool.Graph.Execute (callHandler, dispatchGoto)
import Tidepool.Worktree.Executor (runWorktreeIO, defaultWorktreeConfig)

import TypesFirstDev.Baseline (runBaselineWithSpec)
import TypesFirstDev.DevRuns (runDirectory)
import TypesFirstDev.Graph (TypesFirstGraph(..))
import TypesFirstDev.Handlers (typesFirstHandlers)
import TypesFirstDev.Stats (RunMetadata(..))
import TypesFirstDev.Types (StackSpec(..), ProjectType(..), ParallelResults(..), TestsResult(..), ImplResult(..))


-- | Main entry point.
--
-- Runs the types-first workflow to generate type definitions.
-- Supports multiple experiments via command-line args or env vars.
main :: IO ()
main = do
  args <- getArgs
  case args of
    ("baseline":rest) -> runBaselineCommand rest
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
  -- Note: Interpreter order must match DevEffects = '[Reader ClaudeCodeConfig, Reader StackSpec, ...]
  -- Innermost interpreter handles last effect in the list.
  result <- runM
    . runWorktreeIO worktreeConfig
    . runClaudeCodeExecIO claudeConfig
    . runReader spec          -- Reader StackSpec (second in DevEffects)
    . runReader claudeConfig  -- Reader ClaudeCodeConfig (first in DevEffects)
    $ do
        let handlers = typesFirstHandlers spec
        choice <- callHandler (types handlers) spec
        dispatchGoto handlers choice

  printResult result


-- | Parse experiment name from command-line args.
parseExperiment :: [String] -> String
parseExperiment ("--experiment":name:_) = name
parseExperiment ("-e":name:_) = name
parseExperiment _ = "stack"  -- default


-- | Get the spec for a given experiment.
getSpec :: String -> FilePath -> IO StackSpec
getSpec "stack" projectPath = pure $ StackSpec
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

getSpec "url-shortener" projectPath = pure $ StackSpec
  { ssProjectPath = projectPath
  , ssModuleName = "UrlShortener"
  , ssDescription = T.unlines
      [ "A URL shortening service with three endpoints:"
      , "- POST /shorten: Accept a JSON body with 'url' field, return a short URL"
      , "- GET /:code: Expand a short code to the original URL"
      , "- GET /:code/stats: Get usage statistics (hit count) for a short code"
      , ""
      , "The service should store mappings in memory using an IORef with a Map."
      , "Short codes should be generated deterministically (e.g., hash-based) so that"
      , "the same URL always produces the same short code."
      ]
  , ssAcceptanceCriteria =
      [ "Roundtrip: POST /shorten with a URL, then GET /:code returns the original URL"
      , "Idempotent: POSTing the same URL twice returns the same short code"
      , "Stats increment: Each GET /:code request increments the hit count in stats"
      , "Not found: GET with an invalid code returns HTTP 404"
      , "Validation: POST with invalid/empty URL returns HTTP 400"
      ]
  , ssProjectType = ServantServer
  }

getSpec experiment _ = do
  putStrLn $ "ERROR: Unknown experiment: " <> experiment
  putStrLn "Available experiments: stack, url-shortener"
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

  -- Impl result
  putStrLn "Implementation:"
  putStrLn $ "  Build passed: " <> show pr.prImplResult.irBuildPassed
  putStrLn $ "  All functions: " <> show pr.prImplResult.irAllFunctionsImplemented
  putStrLn $ "  Commit msg: " <> T.unpack pr.prImplResult.irCommitMessage
  putStrLn $ "  Design notes: " <> T.unpack pr.prImplResult.irDesignNotes
  case pr.prImplResult.irBlocker of
    Nothing -> pure ()
    Just blocker -> putStrLn $ "  BLOCKED: " <> T.unpack blocker
  putStrLn ""

  -- Tests result
  putStrLn "Tests:"
  putStrLn $ "  Build passed: " <> show pr.prTestsResult.trBuildPassed
  putStrLn $ "  All properties: " <> show pr.prTestsResult.trAllPropertiesWritten
  putStrLn $ "  Commit msg: " <> T.unpack pr.prTestsResult.trCommitMessage
  putStrLn $ "  Strategy: " <> T.unpack pr.prTestsResult.trTestingStrategy
  case pr.prTestsResult.trBlocker of
    Nothing -> pure ()
    Just blocker -> putStrLn $ "  BLOCKED: " <> T.unpack blocker
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
