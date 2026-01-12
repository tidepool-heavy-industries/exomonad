{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

-- | Handlers for the single Work node.
--
-- The Work node is recursive and handles everything via self-loops:
-- - Continue: immediate self-loop (more local work)
-- - Spawn: spawn children, then await
-- - AwaitNext: block until child completes, inject result
-- - Complete: exit graph
--
-- State is tracked via Memory effect to persist across self-loops.
module TypesFirstDev.WorkHandlers
  ( -- * Handlers
    workBefore
  , workAfter
    -- * State
  , WorkMem(..)
  , emptyWorkMem
  ) where

import Control.Monad (forM_)
import Control.Monad.Freer (Eff, Member)
import Data.Text (Text)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Tidepool.Effect.Session (Session, SessionOperation(..), SessionId, SessionInfo(..))
import Tidepool.Effect.Subgraph (Subgraph, ChildId, ChildHandle(..), ChildError(..), spawnSelf, awaitAny)
import Tidepool.Effect.GraphContext (GraphContext, getEntry)
import Tidepool.Graph.Memory (Memory, getMem, updateMem)
import Tidepool.Graph.Goto (To, GotoChoice, gotoChoice, gotoExit, ClaudeCodeResult(..))
import Tidepool.Graph.Types (Exit)

import TypesFirstDev.WorkContext (WorkTemplateCtx(..), CompletedChildCtx(..), ChildOutcome(..), mkCompletedChild)
import TypesFirstDev.Types.Work
import TypesFirstDev.Types.Core (Spec(..))

-- ════════════════════════════════════════════════════════════════════════════
-- MEMORY STATE
-- ════════════════════════════════════════════════════════════════════════════

-- | Memory state for the Work node.
--
-- Persists across self-loops to track:
-- - Spawned children
-- - Completed children
-- - Last session info for continuation (includes worktree/branch for stateless mantle)
-- - Child directives for correlation
data WorkMem = WorkMem
  { wmSessionInfo     :: Maybe SessionInfo             -- ^ Last session info for continuation
  , wmPendingCount    :: Int                           -- ^ Number of children still running
  , wmCompleted       :: [CompletedChildCtx]           -- ^ All completed children
  , wmLastCompleted   :: Maybe CompletedChildCtx       -- ^ Most recently completed child
  , wmChildDirectives :: M.Map ChildId Text            -- ^ ChildId -> directive for lookup
  }
  deriving stock (Show, Eq)

-- | Initial empty memory state.
emptyWorkMem :: WorkMem
emptyWorkMem = WorkMem
  { wmSessionInfo = Nothing
  , wmPendingCount = 0
  , wmCompleted = []
  , wmLastCompleted = Nothing
  , wmChildDirectives = M.empty
  }

-- ════════════════════════════════════════════════════════════════════════════
-- BEFORE HANDLER
-- ════════════════════════════════════════════════════════════════════════════

-- | Work before handler: build template context from input + memory.
--
-- The context varies based on state:
-- - Fresh: no completed children, no pending
-- - After Spawn: pending > 0, no completed yet
-- - After AwaitNext: lastCompleted populated, pending decremented
workBefore
  :: ( Member Session es
     , Member (Memory WorkMem) es
     )
  => WorkInput
  -> Eff es (WorkTemplateCtx, SessionOperation)
workBefore input = do
  mem <- getMem @WorkMem

  let ctx = WorkTemplateCtx
        { spec = input.wiSpec
        , depth = input.wiDepth
        , maxDepth = input.wiMaxDepth
        , completedChild = mem.wmLastCompleted
        , pendingCount = mem.wmPendingCount
        , allCompleted = mem.wmCompleted
        -- Derived fields
        , isRoot = input.wiDepth == 0
        , atMaxDepth = input.wiDepth >= input.wiMaxDepth
        , hasCompletedChild = mem.wmLastCompleted /= Nothing
        , hasPendingChildren = mem.wmPendingCount > 0
        }

  -- Clear lastCompleted after building context (it's been shown to LLM)
  updateMem @WorkMem $ \m -> m { wmLastCompleted = Nothing }

  -- Session strategy:
  -- 1. Child with parent info: fork from parent (inherits context)
  -- 2. Continuation of current session: continue from stored session info
  -- 3. Fresh start (root): start fresh with slug "work"
  let sessionOp = case (input.wiParentInfo, mem.wmSessionInfo) of
        -- Child with parent info: fork from parent session
        (Just pinfo, _) ->
          let childSlug = "child-" <> T.take 8 input.wiSpec.sId
          in ForkFrom pinfo.siSessionId pinfo.siWorktree pinfo.siBranch childSlug
        -- Continuation: reuse current session info
        (_, Just sinfo) ->
          ContinueFrom sinfo.siSessionId sinfo.siWorktree sinfo.siBranch
        -- Root fresh start
        (Nothing, Nothing) -> StartFresh "work"

  pure (ctx, sessionOp)

-- ════════════════════════════════════════════════════════════════════════════
-- AFTER HANDLER
-- ════════════════════════════════════════════════════════════════════════════

-- | Work after handler: route based on exit type.
--
-- All exits except Complete are self-loops:
-- - Continue: immediate self-loop
-- - Spawn: spawn children, update pending count, then AwaitNext
-- - AwaitNext: block on child, inject result, self-loop
-- - Complete: exit graph
--
-- Receives ClaudeCodeResult which includes worktree/branch for stateless continuation.
workAfter
  :: ( Member Session es
     , Member (Subgraph WorkInput WorkResult) es
     , Member (GraphContext WorkInput) es
     , Member (Memory WorkMem) es
     )
  => (ClaudeCodeResult WorkExit, SessionId)
  -> Eff es (GotoChoice '[To "wgWork" WorkInput, To Exit WorkResult])
workAfter (result, _sid) = do
  entry <- getEntry @WorkInput

  -- Store session info (id, worktree, branch) for continuation
  let sessionInfo = SessionInfo result.ccrSessionId result.ccrWorktree result.ccrBranch
  updateMem @WorkMem $ \m -> m { wmSessionInfo = Just sessionInfo }

  case result.ccrParsedOutput of
    Continue -> do
      -- Immediate self-loop with same input
      pure $ gotoChoice @"wgWork" entry

    Spawn { weChildren = children } -> do
      -- Spawn each child as recursive graph, passing current session info for fork
      forM_ children $ \childSpec -> do
        let childInput = WorkInput
              { wiSpec = childSpecToSpec childSpec entry.wiSpec
              , wiDepth = entry.wiDepth + 1
              , wiMaxDepth = entry.wiMaxDepth
              , wiParentInfo = Just sessionInfo  -- Pass parent session info for fork-based inheritance
              }
        handle <- spawnSelf @WorkInput @WorkResult childInput
        -- Store directive for lookup when child completes
        updateMem @WorkMem $ \m -> m
          { wmChildDirectives = M.insert handle.chId childSpec.csDirective m.wmChildDirectives
          }

      -- Update pending count
      updateMem @WorkMem $ \m -> m { wmPendingCount = length children }

      -- Now wait for first child (implicit AwaitNext behavior after Spawn)
      awaitAndLoop entry

    AwaitNext -> do
      -- Block until a child completes, but only if children are pending
      mem <- getMem @WorkMem
      if mem.wmPendingCount > 0
        then awaitAndLoop entry
        else do
          -- No pending children - self-loop to re-prompt LLM
          -- (it will likely respond with Complete since nothing to wait for)
          pure $ gotoChoice @"wgWork" entry

    Complete { weCommitHash = commitHash } -> do
      -- Exit graph with result
      pure $ gotoExit (WorkResult commitHash)

-- | Wait for a child to complete and self-loop with result injected.
--
-- Handles both success and failure cases. On failure, the error is
-- captured in CompletedChildCtx so the LLM can decide what to do.
awaitAndLoop
  :: ( Member (Subgraph WorkInput WorkResult) es
     , Member (Memory WorkMem) es
     )
  => WorkInput
  -> Eff es (GotoChoice '[To "wgWork" WorkInput, To Exit WorkResult])
awaitAndLoop entry = do
  -- Block until a child completes (success or failure)
  (childId, outcome) <- awaitAny @WorkInput @WorkResult

  -- Look up the directive from stored memory
  mem <- getMem @WorkMem
  let directive = M.findWithDefault "unknown" childId (mem.wmChildDirectives)

  -- Build completed child context based on success/failure
  let childOutcome = case outcome of
        Right childResult -> ChildSuccess { coCommitHash = childResult.wrCommitHash }
        Left err -> ChildFailure { coErrorMessage = err.ceMessage, coErrorDetails = err.ceDetails }

  let completed = mkCompletedChild directive childOutcome

  -- Update memory: add to completed, set as last, decrement pending, remove from directives
  updateMem @WorkMem $ \m -> m
    { wmCompleted = completed : m.wmCompleted
    , wmLastCompleted = Just completed
    , wmPendingCount = max 0 (m.wmPendingCount - 1)
    , wmChildDirectives = M.delete childId (m.wmChildDirectives)
    }

  -- Self-loop to show completed child to LLM
  -- The LLM can see the error and decide whether to retry, abort, or continue
  pure $ gotoChoice @"wgWork" entry

-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Convert ChildSpec to Spec for recursive spawning.
--
-- The directive becomes the description.
-- Boundary becomes target path if specified.
childSpecToSpec :: ChildSpec -> Spec -> Spec
childSpecToSpec cs parentSpec = Spec
  { sId = "child-" <> T.take 8 (T.filter (/= ' ') cs.csDirective)
  , sDescription = cs.csDirective
  , sAcceptanceCriteria = []  -- Children inherit via session fork
  , sTargetPath = maybe parentSpec.sTargetPath T.unpack cs.csBoundary
  , sTestPath = parentSpec.sTestPath
  , sComplexityConstraints = Nothing
  }
