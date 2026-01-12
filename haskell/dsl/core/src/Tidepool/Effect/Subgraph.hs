{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Subgraph effect for spawning recursive graph instances.
--
-- This effect enables a graph to spawn child instances of itself,
-- creating a tree of concurrent executions. Key features:
--
-- * Generic over entry and result types
-- * Minimal operations: SpawnSelf, AwaitAny, GetPending
-- * Error handling: AwaitAny returns Either ChildError result
-- * Rebase coordination happens via git, not messaging
--
-- Example usage:
--
-- @
-- hDecideHandler :: ScaffoldingResult
--                -> Eff (Subgraph Spec V2Result ': effs) (GotoChoice '[...])
-- hDecideHandler result = do
--   handles <- traverse spawnSelf childSpecs
--   results <- collectLoop handles
--   pure (gotoChoice @Exit results)
--
-- collectLoop :: [ChildHandle] -> Eff (Subgraph Spec V2Result ': effs) [V2Result]
-- collectLoop handles = go handles []
--   where
--     go [] acc = pure (reverse acc)
--     go pending acc = do
--       (cid, outcome) <- awaitAny
--       case outcome of
--         Right result -> go (removeByCid cid pending) (result : acc)
--         Left err -> handleChildError cid err  -- Or propagate
-- @
module Tidepool.Effect.Subgraph
  ( -- * Effect Type
    Subgraph(..)

    -- * Smart Constructors
  , spawnSelf
  , awaitAny
  , getPending

    -- * Handle Types
  , ChildHandle(..)
  , ChildId(..)

    -- * Error Types
  , ChildError(..)
  ) where

import Control.Monad.Freer (Eff, Member, send)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT TYPE
-- ════════════════════════════════════════════════════════════════════════════

-- | Effect for spawning instances of the current graph.
--
-- Type parameters:
--   * @entry@ - what the graph takes as input (e.g., Spec)
--   * @result@ - what the graph produces on exit (e.g., V2Result)
--
-- This is a generic DSL primitive. Specific graphs instantiate with their types.
-- The interpreter handles actual spawning and coordination.
data Subgraph entry result r where
  -- | Spawn another instance of THIS graph with the given input.
  -- Returns a handle to track the spawned child.
  SpawnSelf :: entry -> Subgraph entry result ChildHandle

  -- | Block until any child completes (success or failure).
  -- Returns the child's ID and either an error or the result.
  -- Errors are captured from exceptions thrown during child execution.
  AwaitAny :: Subgraph entry result (ChildId, Either ChildError result)

  -- | Get handles to children that haven't completed yet.
  GetPending :: Subgraph entry result [ChildHandle]


-- ════════════════════════════════════════════════════════════════════════════
-- SMART CONSTRUCTORS
-- ════════════════════════════════════════════════════════════════════════════

-- | Spawn a child graph instance with the given entry value.
--
-- The child runs the same graph as the parent, enabling recursive
-- decomposition. Returns immediately with a handle; the child
-- runs asynchronously.
--
-- Usage requires type application to specify the result type:
--
-- @
-- handle <- spawnSelf \@Spec \@V2Result childSpec
-- @
spawnSelf
  :: forall entry result effs.
     Member (Subgraph entry result) effs
  => entry
  -> Eff effs ChildHandle
spawnSelf entry = send (SpawnSelf entry :: Subgraph entry result ChildHandle)

-- | Block until any spawned child completes (success or failure).
--
-- Returns the child's ID (for correlation) and either an error or result.
-- If multiple children complete simultaneously, returns one
-- arbitrarily (implementation-dependent).
--
-- Usage requires type application to specify the entry type:
--
-- @
-- (cid, outcome) <- awaitAny \@Spec \@V2Result
-- case outcome of
--   Right result -> processResult result
--   Left err -> handleError err
-- @
awaitAny
  :: forall entry result effs.
     Member (Subgraph entry result) effs
  => Eff effs (ChildId, Either ChildError result)
awaitAny = send (AwaitAny :: Subgraph entry result (ChildId, Either ChildError result))

-- | Get handles to all children that haven't completed yet.
--
-- Useful for tracking progress or implementing custom collection
-- strategies.
--
-- Usage requires type application to specify both types:
--
-- @
-- pending <- getPending \@Spec \@V2Result
-- @
getPending
  :: forall entry result effs.
     Member (Subgraph entry result) effs
  => Eff effs [ChildHandle]
getPending = send (GetPending :: Subgraph entry result [ChildHandle])


-- ════════════════════════════════════════════════════════════════════════════
-- HANDLE TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Opaque handle to a spawned child graph instance.
--
-- Used to track and correlate child results. The internal ID
-- is a UUID for uniqueness across the tree.
newtype ChildHandle = ChildHandle { chId :: ChildId }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Unique identifier for a child graph instance.
--
-- Generated by the interpreter when SpawnSelf is called.
-- Used to correlate results in AwaitAny with specific children.
newtype ChildId = ChildId { unChildId :: UUID }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)


-- ════════════════════════════════════════════════════════════════════════════
-- ERROR TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Error from a child graph execution.
--
-- Captures exception information when a child fails so the parent
-- can decide how to handle it (retry, abort, continue with partial results).
data ChildError = ChildError
  { ceMessage   :: Text      -- ^ Human-readable error message
  , ceException :: Text      -- ^ Exception type name
  , ceDetails   :: Maybe Text -- ^ Additional details (stderr, exit code, etc.)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
