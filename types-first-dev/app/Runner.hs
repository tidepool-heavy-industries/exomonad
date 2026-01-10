{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- | Actor runtime runner for the V2 tree-based TDD graph.
--
-- V2 is radically simpler: just 4 nodes.
--
-- @
-- Entry(Spec) → hScaffold → hImplement → Exit(V2Result)
-- @
--
-- The tree structure emerges from SpawnSelf calls in hImplement,
-- not from graph topology. Same graph handles both decomposition
-- and leaf cases.
module Main (main) where

import Control.Monad.Freer (Eff, runM)
import Control.Monad.Freer.Error (runError)
import Control.Monad.Freer.Reader (runReader)
import Data.Aeson (ToJSON(..), FromJSON(..))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

import Tidepool.Actor.Graph
  ( runGraphAsActors
  , HandlerBuilder
  , wrapEffHandler
  , pureHandler
  , ExtractChoice
  , GotoChoice
  , gotoChoice
  )
import Tidepool.Actor.Subgraph (SubgraphState, withRecursiveGraph, runSubgraph)
import Tidepool.Graph.Execute (callHandler, CallHandler)
import Tidepool.Graph.Goto (To, ClaudeCodeLLMHandler)
import Tidepool.Graph.Types ()
import Tidepool.Graph.Memory (evalMemory)
import Tidepool.Session.Executor (runSessionIO, defaultSessionConfig, SessionConfig(..))
import Tidepool.Worktree.Executor (runWorktreeIO, defaultWorktreeConfig, WorktreeConfig(..))

import TypesFirstDev.Effect.Build (runBuildIO)
import TypesFirstDev.Handlers.V2.Effects
  ( V2Effects
  , V2Context(..)
  , V2Error(..)
  , initialV2Context
  )
import TypesFirstDev.Types.V2
  ( Spec(..)
  , V2Result(..)
  )
import TypesFirstDev.Handlers.V2.Genesis
  ( hScaffoldingHandler
  , hImplementHandler
  )


-- ════════════════════════════════════════════════════════════════════════════
-- CONFIGURATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Build a test Spec for development.
buildTestSpec :: FilePath -> Spec
buildTestSpec targetDir = Spec
  { specDescription = "A purely functional stack data structure with O(1) push/pop"
  , specAcceptanceCriteria =
      [ "Push adds element to top"
      , "Pop removes and returns top element"
      , "Peek returns top without removing"
      , "IsEmpty checks for empty stack"
      , "Size returns number of elements"
      ]
  , specTargetPath = targetDir <> "/src/Data/Stack.hs"
  , specTestPath = targetDir <> "/test/Data/StackSpec.hs"
  , specParentBranch = Nothing  -- Root node
  , specDepth = 0               -- Root depth
  }


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT INTERPRETER
-- ════════════════════════════════════════════════════════════════════════════

-- | Run the V2Effects stack to IO.
--
-- Effect stack (top to bottom):
--   Error V2Error
--   Memory V2Context
--   Subgraph Spec V2Result  (recursive spawning!)
--   Reader Spec
--   Build
--   Session
--   Worktree
--   IO
--
-- Note: SubgraphState is passed in because it needs to be created via
-- 'withRecursiveGraph' at the top level to enable deferred wiring.
runV2Effects
  :: Spec
  -> WorktreeConfig
  -> SessionConfig
  -> V2Context
  -> SubgraphState Spec V2Result  -- ^ Subgraph state (created externally for wiring)
  -> Eff V2Effects a
  -> IO (Either V2Error a)
runV2Effects spec wtConfig sessConfig initialMem subgraphState eff =
  runM
    . runWorktreeIO wtConfig
    . runSessionIO sessConfig
    . runBuildIO
    . runReader spec
    . runSubgraph subgraphState
    . evalMemory initialMem
    . runError @V2Error
    $ eff


-- | Unwrapping version that crashes on V2Error (for handlers).
runV2EffectsUnsafe
  :: Spec
  -> WorktreeConfig
  -> SessionConfig
  -> V2Context
  -> SubgraphState Spec V2Result
  -> Eff V2Effects a
  -> IO a
runV2EffectsUnsafe spec wtConfig sessConfig initialMem subgraphState eff = do
  result <- runV2Effects spec wtConfig sessConfig initialMem subgraphState eff
  case result of
    Left err -> error $ "V2Error: " <> show err
    Right a -> pure a


-- ════════════════════════════════════════════════════════════════════════════
-- HANDLER WRAPPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Wrap a ClaudeCodeLLMHandler as a HandlerBuilder.
wrapClaudeCodeHandler
  :: forall model payload schema targets tpl.
     ( FromJSON payload
     , ExtractChoice targets
     , CallHandler (ClaudeCodeLLMHandler model payload schema targets V2Effects tpl) payload V2Effects targets
     )
  => (forall a. Eff V2Effects a -> IO a)
  -> ClaudeCodeLLMHandler model payload schema targets V2Effects tpl
  -> HandlerBuilder
wrapClaudeCodeHandler interpret handler =
  pure $ wrapEffHandler interpret (callHandler handler)


-- ════════════════════════════════════════════════════════════════════════════
-- HANDLER MAP
-- ════════════════════════════════════════════════════════════════════════════

-- | Build the handler map for V2's 4 nodes.
buildHandlerMap
  :: (forall a. Eff V2Effects a -> IO a)
  -> IO (Map.Map Text HandlerBuilder)
buildHandlerMap interpret = do
  pure $ Map.fromList
    -- Entry: receives Spec, routes to hScaffold
    [ ("entry", entryHandler)

    -- Scaffold: TDD scaffolding (tests + stubs + rubric)
    , ("hScaffold", wrapClaudeCodeHandler interpret hScaffoldingHandler)

    -- Implement: spawn children if needed, then implement
    -- NOTE: hImplementHandler has Subgraph constraint, need special handling
    , ("hImplement", wrapImplementHandler interpret)
    ]
  where
    entryHandler :: HandlerBuilder
    entryHandler = pureHandler entryFn
      where
        entryFn :: Spec -> GotoChoice '[To "hScaffold" Spec]
        entryFn spec = gotoChoice @"hScaffold" spec

    -- hImplement needs special handling for Subgraph effect
    -- For now, wrap with the V2Effects interpreter which includes Subgraph
    wrapImplementHandler :: (forall a. Eff V2Effects a -> IO a) -> HandlerBuilder
    wrapImplementHandler interp = pure $ wrapEffHandler interp hImplementHandler


-- ════════════════════════════════════════════════════════════════════════════
-- MAIN
-- ════════════════════════════════════════════════════════════════════════════

main :: IO ()
main = do
  -- Parse CLI args
  args <- getArgs
  cwd <- getCurrentDirectory

  let targetDir = case args of
        (d:_) -> d
        []    -> cwd <> "/test-repo"

  -- Print banner
  putStrLn "══════════════════════════════════════════════════════════════════"
  putStrLn "  types-first-dev V2: Tree-based TDD with Subgraph Effect"
  putStrLn "══════════════════════════════════════════════════════════════════"
  putStrLn ""
  putStrLn $ "Target directory: " <> targetDir
  putStrLn $ "Working directory: " <> cwd
  putStrLn ""
  hFlush stdout

  -- Build configuration
  let spec = buildTestSpec targetDir
      worktreeConfig = defaultWorktreeConfig cwd
      sessionConfig = (defaultSessionConfig cwd)
        { scMantlePath = cwd <> "/../rust/target/release/mantle"
        }

  putStrLn "Configuration:"
  putStrLn $ "  Description: " <> T.unpack spec.specDescription
  putStrLn $ "  Target path: " <> spec.specTargetPath
  putStrLn $ "  Test path: " <> spec.specTestPath
  putStrLn $ "  Depth: " <> show spec.specDepth
  putStrLn ""

  -- Initial memory state
  let initialMem = initialV2Context "v2-runner" 0 "main"

  -- ════════════════════════════════════════════════════════════════════════════
  -- RECURSIVE GRAPH EXECUTION
  -- ════════════════════════════════════════════════════════════════════════════
  --
  -- Use withRecursiveGraph to handle the chicken-and-egg dependency:
  -- 1. SubgraphState needs the graph runner
  -- 2. Graph runner needs handlers
  -- 3. Handlers need the interpreter
  -- 4. Interpreter needs SubgraphState
  --
  -- withRecursiveGraph breaks this cycle via deferred binding (IORef internally).
  -- The wire function is called AFTER handlers are built but BEFORE graph runs.

  result <- withRecursiveGraph @Spec @V2Result $ \subgraphState wireRecursion -> do

    -- Build effect interpreter using the subgraph state
    let interpret :: forall a. Eff V2Effects a -> IO a
        interpret = runV2EffectsUnsafe spec worktreeConfig sessionConfig initialMem subgraphState

    -- Build handler map
    putStrLn "Building handler map for V2 (4 nodes)..."
    handlers <- buildHandlerMap interpret
    putStrLn $ "  Handlers: " <> show (Map.keys handlers)
    putStrLn ""
    hFlush stdout

    -- ════════════════════════════════════════════════════════════════════════
    -- WIRE THE RECURSION
    -- ════════════════════════════════════════════════════════════════════════
    --
    -- This is the key step! When a handler calls spawnSelf with a child Spec,
    -- the interpreter needs to run the full graph for that child. We wire
    -- that capability here, now that handlers exist.
    --
    -- After this call, spawned children will recursively run the full graph.
    wireRecursion $ \childSpec -> runGraphAsActors @V2Result handlers (toJSON childSpec)

    -- Run the graph!
    putStrLn "Starting V2 graph execution..."
    putStrLn "══════════════════════════════════════════════════════════════════"
    putStrLn ""
    hFlush stdout

    runGraphAsActors @V2Result handlers (toJSON spec)

  -- Print result
  putStrLn ""
  putStrLn "══════════════════════════════════════════════════════════════════"
  putStrLn "  Result"
  putStrLn "══════════════════════════════════════════════════════════════════"
  printResult result


-- | Pretty print the V2 result.
printResult :: V2Result -> IO ()
printResult result = do
  putStrLn $ "Success: " <> show result.vrSuccess
  putStrLn $ "Description: " <> T.unpack result.vrDescription
  putStrLn $ "Branch: " <> T.unpack result.vrBranch
  putStrLn $ "Commit: " <> T.unpack result.vrCommitHash
  case result.vrSubsystemName of
    Nothing -> putStrLn "Root node (not a subsystem)"
    Just name -> putStrLn $ "Subsystem: " <> T.unpack name
