{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- | Actor runtime runner for the hybrid TDD graph.
--
-- This module wires up all 20 nodes of the TypesFirstGraphHybrid to the
-- actor runtime system, enabling full parallel execution.
--
-- = Architecture
--
-- @
-- ┌────────────────────────────────────────────────────────────────┐
-- │                         Main                                    │
-- │  1. Parse CLI args (target repo, config)                       │
-- │  2. Build StackSpec configuration                               │
-- │  3. Build effect interpreter: Eff HybridEffects a -> IO a      │
-- │  4. Build handler map: Map Text HandlerBuilder                 │
-- │  5. runGraphAsActors handlers (toJSON stackSpec)               │
-- └────────────────────────────────────────────────────────────────┘
--                              │
--                              v
-- ┌────────────────────────────────────────────────────────────────┐
-- │                    Actor System (ki threads)                    │
-- │                                                                 │
-- │  [entry] ─► [hTypes] ─► [hSkeleton] ─► [hTypeAdversary]        │
-- │                                              │                  │
-- │              ┌───────────────────────────────┘                  │
-- │              v                                                  │
-- │         [hGate] ──holes──► [hTypesFix] ───┐                    │
-- │              │                            │                     │
-- │              └──clean──► [hFork] ◄────────┘                    │
-- │                            │                                    │
-- │              ┌─────────────┴─────────────┐                     │
-- │              v                           v                      │
-- │         [hTests]                     [hImpl]     (parallel)    │
-- │              │                           │                      │
-- │              └─── Arrive ───► [hJoin] ◄──┘                     │
-- │                                  │                              │
-- │         [hVerifyTDD] ◄───────────┘                             │
-- │              │                                                  │
-- │    trivial   │   pass                                          │
-- │       │      │                                                  │
-- │       v      v                                                  │
-- │  [hTestsReject]  [hMerge] ──conflict──► [hConflictResolve]     │
-- │       │              │                        │                 │
-- │       └──► hFork     └──clean──► [hValidate] ◄┘                │
-- │                            │                                    │
-- │                   fail     │    pass                           │
-- │                     │      │                                    │
-- │                     v      v                                    │
-- │                  [hFix]  [hPostValidate] ─► [hMutationAdversary]│
-- │                     │                              │            │
-- │                     └──► hValidate     [hWitness] ◄┘            │
-- │                                            │                    │
-- │                                            v                    │
-- │                                         [exit]                  │
-- └────────────────────────────────────────────────────────────────┘
-- @
module Main (main) where

import Control.Monad.Freer (Eff, runM)
import Control.Monad.Freer.Error (runError)
import Control.Monad.Freer.Reader (runReader)
import Data.Aeson (ToJSON(..), FromJSON(..))
import qualified Data.Aeson as Aeson
-- IORef not needed - using pure memory interpretation
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

import Tidepool.Actor.Graph
  ( runGraphAsActors
  , HandlerBuilder
  , NodeHandler(..)
  , wrapEffHandler
  , pureHandler
  , ExtractChoice
  , GotoChoice
  , gotoChoice
  )
import Tidepool.Actor.Fork (forkHandler)
import Tidepool.Actor.Barrier (barrierHandler)
import Tidepool.Actor.Merge (ExpectedSources, FromPayloads, ExtractMergeResults)
import Tidepool.Graph.Execute (callHandler, CallHandler)
import Tidepool.Graph.Goto (To, ClaudeCodeLLMHandler)
import Tidepool.Graph.Types (HList(..), Exit, Arrive, ModelChoice(..), From)
import Tidepool.Graph.Memory (evalMemory)
import Tidepool.Session.Executor (runSessionIO, defaultSessionConfig, SessionConfig(..))
import Tidepool.Worktree.Executor (runWorktreeIO, defaultWorktreeConfig, WorktreeConfig(..))

import TypesFirstDev.Effect.Build (runBuildIO)
import TypesFirstDev.Handlers.Hybrid.Effects
  ( HybridEffects
  , SessionContext(..)
  , WorkflowError(..)
  , initialSessionContext
  )
import TypesFirstDev.Types.Hybrid
import TypesFirstDev.Handlers.Hybrid.Genesis
  ( hTypesHandler
  , hSkeletonHandler
  , hTypeAdversaryHandler
  , hGateHandler
  , hTypesFixHandler
  )
import TypesFirstDev.Handlers.Hybrid.Parallel
  ( hForkHandler
  , hTestsHandler
  , hImplHandler
  )
import TypesFirstDev.Handlers.Hybrid.Merge
  ( hJoinHandler
  , hVerifyTDDHandler
  , hTestsRejectHandler
  , hMergeHandler
  , hConflictResolveHandler
  )
import TypesFirstDev.Handlers.Hybrid.Validation
  ( hValidateHandler
  , hFixHandler
  , hPostValidateHandler
  )
import TypesFirstDev.Handlers.Hybrid.Adversary
  ( hMutationAdversaryHandler
  )
import TypesFirstDev.Handlers.Hybrid.Witness
  ( hWitnessHandler
  )


-- ════════════════════════════════════════════════════════════════════════════
-- CONFIGURATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Build a StackSpec for a test target.
buildStackSpec :: FilePath -> StackSpec
buildStackSpec targetDir = StackSpec
  { specModuleName = "Data.Stack"
  , specDescription = "A purely functional stack data structure with O(1) push/pop"
  , specAcceptanceCriteria =
      [ "Push adds element to top"
      , "Pop removes and returns top element"
      , "Peek returns top without removing"
      , "IsEmpty checks for empty stack"
      , "Size returns number of elements"
      ]
  , specImplPath = targetDir <> "/src/Data/Stack.hs"
  , specTestPath = targetDir <> "/test/Data/StackSpec.hs"
  , specStrictness = defaultStrictness
  }


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT INTERPRETER
-- ════════════════════════════════════════════════════════════════════════════

-- | Run the HybridEffects stack to IO.
--
-- Effect stack (top to bottom):
--   Error WorkflowError
--   Memory SessionContext
--   Reader StackSpec
--   Build
--   Session
--   Worktree
--   IO
--
-- NOTE: Memory is interpreted purely with evalMemory. This means state is
-- NOT persisted between handler invocations. For full state persistence,
-- we'd need to serialize/deserialize SessionContext between calls or use
-- the actor runtime's IORef-based Memory.
runHybridEffects
  :: StackSpec
  -> WorktreeConfig
  -> SessionConfig
  -> SessionContext  -- ^ Initial memory state
  -> Eff HybridEffects a
  -> IO (Either WorkflowError a)
runHybridEffects spec wtConfig sessConfig initialMem =
    runM
  . runWorktreeIO wtConfig
  . runSessionIO sessConfig
  . runBuildIO
  . runReader spec
  . evalMemory initialMem
  . runError @WorkflowError


-- | Unwrapping version that crashes on WorkflowError (for handlers).
runHybridEffectsUnsafe
  :: StackSpec
  -> WorktreeConfig
  -> SessionConfig
  -> SessionContext  -- ^ Initial memory state
  -> Eff HybridEffects a
  -> IO a
runHybridEffectsUnsafe spec wtConfig sessConfig initialMem eff = do
  result <- runHybridEffects spec wtConfig sessConfig initialMem eff
  case result of
    Left err -> error $ "WorkflowError: " <> show err
    Right a -> pure a


-- ════════════════════════════════════════════════════════════════════════════
-- HANDLER WRAPPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Wrap a ClaudeCodeLLMHandler as a HandlerBuilder.
--
-- Uses CallHandler typeclass to invoke executeClaudeCodeHandler.
wrapClaudeCodeHandler
  :: forall model payload schema targets tpl.
     ( FromJSON payload
     , ExtractChoice targets
     , CallHandler (ClaudeCodeLLMHandler model payload schema targets HybridEffects tpl) payload HybridEffects targets
     )
  => (forall a. Eff HybridEffects a -> IO a)
  -> ClaudeCodeLLMHandler model payload schema targets HybridEffects tpl
  -> HandlerBuilder
wrapClaudeCodeHandler interpret handler =
  pure $ wrapEffHandler interpret (callHandler handler)


-- | Wrap a logic handler as a HandlerBuilder.
wrapLogicHandler
  :: forall payload targets.
     ( FromJSON payload
     , ExtractChoice targets
     )
  => (forall a. Eff HybridEffects a -> IO a)
  -> (payload -> Eff HybridEffects (GotoChoice targets))
  -> HandlerBuilder
wrapLogicHandler interpret handler =
  pure $ wrapEffHandler interpret handler


-- ════════════════════════════════════════════════════════════════════════════
-- HANDLER MAP
-- ════════════════════════════════════════════════════════════════════════════

-- | Build the complete handler map for all 20 nodes.
--
-- Each node is wrapped appropriately:
-- - ClaudeCodeLLMHandler nodes use wrapClaudeCodeHandler
-- - Logic nodes use wrapLogicHandler
-- - ForkNode uses forkHandler
-- - BarrierNode uses barrierHandler
buildHandlerMap
  :: (forall a. Eff HybridEffects a -> IO a)
  -> IO (Map.Map Text HandlerBuilder)
buildHandlerMap interpret = do
  -- BarrierNode needs IO to allocate state
  -- Uses From markers to specify expected source names and payload types
  joinNodeHandler <- barrierHandler
    @'[From "hTests" TestsResult, From "hImpl" ImplResult]
    interpret
    hJoinHandler

  pure $ Map.fromList
    -- Entry point: passes through to hTypes
    [ ("entry", entryHandler)

    -- Phase 1: Type Design (WS1)
    , ("hTypes", wrapClaudeCodeHandler interpret hTypesHandler)
    , ("hSkeleton", wrapLogicHandler interpret hSkeletonHandler)
    , ("hTypeAdversary", wrapClaudeCodeHandler interpret hTypeAdversaryHandler)
    , ("hGate", wrapLogicHandler interpret hGateHandler)
    , ("hTypesFix", wrapClaudeCodeHandler interpret hTypesFixHandler)

    -- Phase 2: Parallel Blind Execution (WS2)
    , ("hFork", pure $ forkHandler
        @'[To "hTests" TestsTemplateCtx, To "hImpl" ImplTemplateCtx]
        interpret
        hForkHandler)
    , ("hTests", wrapClaudeCodeHandler interpret hTestsHandler)
    , ("hImpl", wrapClaudeCodeHandler interpret hImplHandler)

    -- Phase 3: TDD Verification (WS3)
    , ("hJoin", pure joinNodeHandler)
    , ("hVerifyTDD", wrapLogicHandler interpret hVerifyTDDHandler)
    , ("hTestsReject", wrapLogicHandler interpret hTestsRejectHandler)
    , ("hMerge", wrapLogicHandler interpret hMergeHandler)
    , ("hConflictResolve", wrapClaudeCodeHandler interpret hConflictResolveHandler)

    -- Phase 4: Validation Loop (WS4)
    , ("hValidate", wrapLogicHandler interpret hValidateHandler)
    , ("hFix", wrapClaudeCodeHandler interpret hFixHandler)
    , ("hPostValidate", wrapLogicHandler interpret hPostValidateHandler)
    , ("hMutationAdversary", wrapClaudeCodeHandler interpret hMutationAdversaryHandler)
    , ("hWitness", wrapLogicHandler interpret hWitnessHandler)
    ]
  where
    -- Entry handler: receives StackSpec, routes to hTypes
    entryHandler :: HandlerBuilder
    entryHandler = pureHandler entryFn
      where
        entryFn :: StackSpec -> GotoChoice '[To "hTypes" StackSpec]
        entryFn spec = gotoChoice @"hTypes" spec


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
  putStrLn "  types-first-dev: Actor Runtime Execution"
  putStrLn "══════════════════════════════════════════════════════════════════"
  putStrLn ""
  putStrLn $ "Target directory: " <> targetDir
  putStrLn $ "Working directory: " <> cwd
  putStrLn ""
  hFlush stdout

  -- Build configuration
  let stackSpec = buildStackSpec targetDir
      worktreeConfig = defaultWorktreeConfig cwd
      sessionConfig = (defaultSessionConfig cwd)
        { scMantlePath = cwd <> "/../rust/target/release/mantle"
        }

  putStrLn "Configuration:"
  putStrLn $ "  Module: " <> T.unpack stackSpec.specModuleName
  putStrLn $ "  Impl path: " <> stackSpec.specImplPath
  putStrLn $ "  Test path: " <> stackSpec.specTestPath
  putStrLn ""

  -- Initial memory state
  let initialMem = initialSessionContext "types-first-dev-runner"

  -- Build effect interpreter
  let interpret :: forall a. Eff HybridEffects a -> IO a
      interpret = runHybridEffectsUnsafe stackSpec worktreeConfig sessionConfig initialMem

  -- Build handler map
  putStrLn "Building handler map for 20 nodes..."
  handlers <- buildHandlerMap interpret
  putStrLn $ "  Handlers: " <> show (Map.keys handlers)
  putStrLn ""
  hFlush stdout

  -- Run the graph!
  putStrLn "Starting graph execution..."
  putStrLn "══════════════════════════════════════════════════════════════════"
  putStrLn ""
  hFlush stdout

  result <- runGraphAsActors @HybridResult handlers (toJSON stackSpec)

  -- Print result
  putStrLn ""
  putStrLn "══════════════════════════════════════════════════════════════════"
  putStrLn "  Result"
  putStrLn "══════════════════════════════════════════════════════════════════"
  printResult result


-- | Pretty print the final result.
printResult :: HybridResult -> IO ()
printResult result = do
  putStrLn $ "Success: " <> show result.hrSuccess
  putStrLn ""
  putStrLn "Spec functions:"
  mapM_ (\f -> putStrLn $ "  - " <> T.unpack f.name) result.hrSpec
  putStrLn ""
  putStrLn $ "Total cost: $" <> show result.hrTotalCost
  putStrLn $ "Witness observations: " <> show (length result.hrWitness.wrObservations)
