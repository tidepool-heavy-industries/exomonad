{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Graph execution engine.
--
-- This module provides the runtime for executing Graph definitions.
-- It manages:
--
-- * Available values (from Entry, Schema outputs, Goto payloads)
-- * Node dispatch based on Needs satisfaction
-- * Goto effect handling for transitions
-- * Exit handling for graph termination
--
-- = Execution Model
--
-- 1. Entry value is added to available values
-- 2. Find nodes whose Needs are all satisfied
-- 3. Execute the node's handler
-- 4. For LLM nodes: add Schema output to available values
-- 5. For Logic nodes: follow Goto to next node or Exit
-- 6. Repeat until Exit is reached
--
-- = Usage
--
-- @
-- result <- runGraph \@MyGraph handlers entryValue
-- @
module Tidepool.Graph.Runner
  ( -- * Graph Execution
    RunnableGraph(..)
  , runGraph
  , runGraphWith

    -- * Execution State
  , GraphState(..)
  , emptyState
  , addAvailable
  , getAvailable
  , isNodeRunnable

    -- * Execution Context
  , GraphContext(..)
  , defaultContext
  ) where

import Data.Kind (Type, Constraint)
import Data.Dynamic (Dynamic, toDyn, fromDynamic, Typeable)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Typeable (TypeRep, typeRep)
import Data.Proxy (Proxy(..))
import Effectful
import GHC.TypeLits (Symbol)

import Tidepool.Graph.Types hiding (Eff)
import Tidepool.Graph.Edges
import Tidepool.Graph.Validate
import Tidepool.Graph.Goto
import Tidepool.Graph.TH (HandlersFor)

-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH EXECUTION TYPECLASS
-- ════════════════════════════════════════════════════════════════════════════

-- | Typeclass for graphs that can be executed.
--
-- This typeclass ties together:
-- * The graph definition
-- * The handlers record type
-- * The entry and exit types
--
-- Instances can be derived via Template Haskell or written manually.
type RunnableGraph :: Type -> Constraint
class ValidGraph g => RunnableGraph g where
  -- | Type of the handlers record for this graph.
  type HandlersType g :: Type
  type HandlersType g = HandlersFor g

  -- | Entry point type.
  type EntryType g :: Type

  -- | Exit point type.
  type ExitType g :: Type

  -- | Execute the graph.
  --
  -- This is the main execution function. It takes handlers and
  -- an entry value, and returns the exit value.
  executeGraph
    :: HandlersType g
    -> EntryType g
    -> Eff es (ExitType g)

-- ════════════════════════════════════════════════════════════════════════════
-- EXECUTION STATE
-- ════════════════════════════════════════════════════════════════════════════

-- | Runtime state during graph execution.
data GraphState = GraphState
  { gsAvailable :: Map TypeRep Dynamic
    -- ^ Values available for Needs satisfaction.
    -- Keyed by TypeRep for O(1) lookup.

  , gsCompleted :: Set Text
    -- ^ Names of nodes that have completed.

  , gsPending :: Set Text
    -- ^ Names of nodes waiting to run.

  , gsCurrentNode :: Maybe Text
    -- ^ Currently executing node (for debugging).
  }
  deriving (Show)

-- | Empty initial state.
emptyState :: GraphState
emptyState = GraphState
  { gsAvailable = Map.empty
  , gsCompleted = Set.empty
  , gsPending = Set.empty
  , gsCurrentNode = Nothing
  }

-- | Add a value to available values.
addAvailable :: forall a. Typeable a => a -> GraphState -> GraphState
addAvailable x state = state
  { gsAvailable = Map.insert (typeRep (Proxy @a)) (toDyn x) state.gsAvailable
  }

-- | Get a value from available values.
getAvailable :: forall a. Typeable a => GraphState -> Maybe a
getAvailable state = do
  dyn <- Map.lookup (typeRep (Proxy @a)) state.gsAvailable
  fromDynamic dyn

-- | Check if all needs for a node are satisfied.
isNodeRunnable :: [TypeRep] -> GraphState -> Bool
isNodeRunnable needs state =
  all (`Map.member` state.gsAvailable) needs

-- ════════════════════════════════════════════════════════════════════════════
-- EXECUTION CONTEXT
-- ════════════════════════════════════════════════════════════════════════════

-- | Configuration for graph execution.
data GraphContext = GraphContext
  { gcDebug :: Bool
    -- ^ Enable debug logging.

  , gcMaxSteps :: Int
    -- ^ Maximum execution steps (prevents infinite loops).

  , gcOnNodeStart :: Text -> IO ()
    -- ^ Callback when a node starts execution.

  , gcOnNodeEnd :: Text -> IO ()
    -- ^ Callback when a node completes execution.

  , gcOnGoto :: Text -> Text -> IO ()
    -- ^ Callback on Goto transition (from, to).
  }

-- | Default execution context.
defaultContext :: GraphContext
defaultContext = GraphContext
  { gcDebug = False
  , gcMaxSteps = 1000
  , gcOnNodeStart = \_ -> pure ()
  , gcOnNodeEnd = \_ -> pure ()
  , gcOnGoto = \_ _ -> pure ()
  }

-- ════════════════════════════════════════════════════════════════════════════
-- MAIN EXECUTION FUNCTIONS
-- ════════════════════════════════════════════════════════════════════════════

-- | Run a graph with default context.
--
-- @
-- result <- runGraph \@MyGraph handlers entryValue
-- @
runGraph
  :: forall g es.
     RunnableGraph g
  => HandlersType g
  -> EntryType g
  -> Eff es (ExitType g)
runGraph = runGraphWith @g defaultContext

-- | Run a graph with custom context.
runGraphWith
  :: forall g es.
     RunnableGraph g
  => GraphContext
  -> HandlersType g
  -> EntryType g
  -> Eff es (ExitType g)
runGraphWith _ctx handlers entry = executeGraph @g handlers entry

-- ════════════════════════════════════════════════════════════════════════════
-- EXECUTION LOOP (INTERNAL)
-- ════════════════════════════════════════════════════════════════════════════

-- The actual execution loop needs to:
--
-- 1. Start with Entry value in available
-- 2. Find runnable nodes (all Needs satisfied)
-- 3. For LLM nodes:
--    - Call handler with Needs values
--    - Add Schema output to available
--    - Continue to dependent nodes
-- 4. For Logic nodes:
--    - Call handler with Needs values
--    - Interpret Goto effect to determine next node
--    - If Goto Exit: return the exit value
--    - If Goto "name": add payload to available, mark "name" as pending
-- 5. Repeat until Exit
--
-- This requires runtime dispatch which is complex to implement generically.
-- The TH module generates specialized execution code for each graph.

-- | Internal: execute a single node.
--
-- This is a placeholder showing the pattern. Actual implementation
-- would be generated by TH for each specific graph.
executeNode
  :: Text              -- ^ Node name
  -> GraphState        -- ^ Current state
  -> IO (Either GotoResult GraphState)
executeNode _nodeName _state = do
  -- 1. Get handler from handlers record (needs TH)
  -- 2. Extract Needs values from state
  -- 3. Call handler
  -- 4. Handle result based on node kind:
  --    - LLM: add Schema output to state
  --    - Logic: capture Goto, return Left for exit or update state
  error "executeNode: requires TH-generated code for specific graph"

-- ════════════════════════════════════════════════════════════════════════════
-- HELPER FUNCTIONS FOR MANUAL IMPLEMENTATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Run a graph manually when TH isn't available.
--
-- This helper allows writing graph execution by hand for
-- simple cases or when debugging.
--
-- @
-- runManual :: IO Result
-- runManual = do
--   let state0 = addAvailable entryValue emptyState
--
--   -- Run "classify" node
--   intent <- handlers.h_classify (getAvailable' @Message state0)
--   let state1 = addAvailable intent state0
--
--   -- Run "route" node (Logic, captures Goto)
--   (mGoto, ()) <- runGotoCapture $ handlers.h_route
--     (getAvailable' @Message state1)
--     (getAvailable' @Intent state1)
--
--   case mGoto of
--     Just (GotoExit result) -> return result
--     Just (GotoNode target payload) -> ...
-- @

-- | Unsafe version of getAvailable that errors on missing.
getAvailable' :: forall a. Typeable a => GraphState -> a
getAvailable' state = case getAvailable @a state of
  Just x  -> x
  Nothing -> error $ "Missing required value: " ++ show (typeRep (Proxy @a))
