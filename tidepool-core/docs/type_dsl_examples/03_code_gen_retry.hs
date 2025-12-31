{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Code Generation with Retry Loop
--
-- Generate code, run tests, retry on failure.
-- Demonstrates: loops via Goto, Goto Exit for success, Tools.
--
-- ```mermaid
-- flowchart TD
--     entry((start)) -->|Spec| generateInitial
--     generateInitial[["generateInitial<br/>LLM + Tools"]] -->|Code| runTests
--     runTests{{"runTests<br/>Logic"}} -->|TestResult| evaluate
--     evaluate{{"evaluate<br/>Logic"}} -->|Code| exit((end))
--     evaluate -->|RetryContext| generateRetry
--     generateRetry[["generateRetry<br/>LLM + Tools"]] -->|Code| runTests
-- ```

module Examples.CodeGenRetry where

import Tidepool.Graph

-- ════════════════════════════════════════════════════════════════
-- TYPES
-- ════════════════════════════════════════════════════════════════

data Spec = Spec
  { specDescription :: Text
  , specTests :: [Text]
  , specLanguage :: Text
  }

newtype Code = Code Text
  deriving (Generic, ToJSON, FromJSON)

data TestResult
  = AllPassed
  | SomeFailed [TestFailure]

data TestFailure = TestFailure
  { tfName :: Text
  , tfExpected :: Text
  , tfActual :: Text
  }
  deriving (Generic, ToJSON)

data RetryContext = RetryContext
  { rcSpec :: Spec
  , rcPreviousCode :: Code
  , rcFailures :: [TestFailure]
  , rcAttempt :: Int
  }

-- Session state for tracking attempts
data CodeGenState = CodeGenState
  { cgsAttempts :: Int
  , cgsMaxAttempts :: Int
  }

-- Tool types
data WriteFile
data ReadFile
data ListFiles

-- ════════════════════════════════════════════════════════════════
-- GRAPH
-- ════════════════════════════════════════════════════════════════

type CodeGenGraph = Graph '[
    Entry :~> Spec

    -- Initial generation with tools
  , "generateInitial" := LLM
      :@ Needs '[Spec]
      :@ Template InitialGenTpl
      :@ Tools '[WriteFile, ReadFile, ListFiles]
      :@ Schema Code

    -- Retry generation (different template, different inputs)
  , "generateRetry" := LLM
      :@ Needs '[RetryContext]
      :@ Template RetryGenTpl
      :@ Tools '[WriteFile, ReadFile]
      :@ Schema Code

    -- Run tests (sends result to evaluate)
  , "runTests" := Logic
      :@ Needs '[Code]
      :@ Eff '[Process, Log, Goto "evaluate" TestResult]

    -- Evaluate: either exit with code or retry
  , "evaluate" := Logic
      :@ Needs '[Spec, Code, TestResult]
      :@ Eff '[
          State CodeGenState
        , Goto Exit Code           -- Success: exit with code
        , Goto "generateRetry" RetryContext  -- Failure: retry
        ]

  , Exit :<~ Code
  ]

-- Edge derivation:
--   Entry → generateInitial (generateInitial Needs Spec)
--   Entry → evaluate (evaluate Needs Spec)
--   generateInitial → runTests (runTests Needs Code, generateInitial Schema Code)
--   generateRetry → runTests (runTests Needs Code, generateRetry Schema Code)
--   runTests → evaluate (runTests Goto "evaluate" TestResult)
--   generateInitial → evaluate (evaluate Needs Code)  -- also via Schema
--   generateRetry → evaluate (evaluate Needs Code)    -- also via Schema
--   evaluate → Exit (evaluate Goto Exit Code)
--   evaluate → generateRetry (evaluate Goto "generateRetry" RetryContext)

-- ════════════════════════════════════════════════════════════════
-- HANDLERS
-- ════════════════════════════════════════════════════════════════

-- Handler types:
--   generateInitial :: Spec -> LLM Code
--   generateRetry :: RetryContext -> LLM Code
--   runTests :: Code -> Eff '[Process, Log, Goto "evaluate" TestResult] ()
--   evaluate :: Spec -> Code -> TestResult -> Eff '[State CodeGenState, Goto Exit Code, Goto "generateRetry" RetryContext] ()

codeGenHandlers :: Handlers CodeGenGraph
codeGenHandlers = Handlers
  { generateInitial = \spec ->
      runLLMWithTools initialGenTemplate [writeFile, readFile, listFiles] spec

  , generateRetry = \ctx ->
      runLLMWithTools retryGenTemplate [writeFile, readFile] ctx

  , runTests = \(Code code) -> do
      log "Running tests..."
      result <- exec $ "echo '" <> code <> "' | run-tests"
      goto @"evaluate" (parseTestResult result)

  , evaluate = \spec code result -> do
      state <- get @CodeGenState
      case result of
        AllPassed ->
          goto @Exit code

        SomeFailed failures -> do
          let attempt = state.cgsAttempts + 1
          put state { cgsAttempts = attempt }
          if attempt >= state.cgsMaxAttempts
            then goto @Exit code  -- Give up, return best effort
            else goto @"generateRetry" RetryContext
              { rcSpec = spec
              , rcPreviousCode = code
              , rcFailures = failures
              , rcAttempt = attempt
              }
  }

-- ════════════════════════════════════════════════════════════════
-- NOTES
-- ════════════════════════════════════════════════════════════════

-- This example demonstrates:
--
-- 1. Loops via Goto
--    - evaluate can Goto "generateRetry"
--    - generateRetry produces Code (Schema)
--    - runTests Needs Code, so it receives from generateRetry
--    - Loop: runTests → evaluate → generateRetry → runTests
--
-- 2. Goto Exit for success
--    - evaluate has Goto Exit Code in its effect stack
--    - On success: goto @Exit code
--    - Exit receives the Code directly
--
-- 3. Multiple Goto targets
--    - evaluate can go to Exit OR generateRetry
--    - Both are in the Eff stack
--    - Handler chooses based on runtime result
--
-- 4. State effect for attempt tracking
--    - CodeGenState tracks attempt count
--    - Persists across loop iterations
--    - Used to implement max retry limit
--
-- 5. Tools for LLM nodes
--    - generateInitial has Tools '[WriteFile, ReadFile, ListFiles]
--    - LLM can invoke tools during generation
