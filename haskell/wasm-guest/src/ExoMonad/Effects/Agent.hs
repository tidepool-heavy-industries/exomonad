{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Agent effects for spawning, cleaning up, and listing agents.
--
-- All effects are dispatched via the @agent@ namespace.
-- Request and response types are proto-generated from @proto/effects/agent.proto@.
module ExoMonad.Effects.Agent
  ( -- * Effect Types
    AgentSpawn,
    AgentSpawnBatch,
    AgentCleanup,
    AgentCleanupBatch,
    AgentCleanupMerged,
    AgentList,

    -- * Smart Constructors
    spawnAgent,
    spawnBatch,
    cleanupAgent,
    cleanupBatch,
    cleanupMerged,
    listAgents,

    -- * Re-exported proto types
    module Effects.Agent,
  )
where

import Effects.Agent
import Effects.EffectError (EffectError)
import ExoMonad.Effect.Class (Effect (..), runEffect)

-- ============================================================================
-- Effect phantom types + instances
-- ============================================================================

data AgentSpawn

instance Effect AgentSpawn where
  type Input AgentSpawn = SpawnRequest
  type Output AgentSpawn = SpawnResponse
  effectId = "agent.spawn"

data AgentSpawnBatch

instance Effect AgentSpawnBatch where
  type Input AgentSpawnBatch = SpawnBatchRequest
  type Output AgentSpawnBatch = SpawnBatchResponse
  effectId = "agent.spawn_batch"

data AgentCleanup

instance Effect AgentCleanup where
  type Input AgentCleanup = CleanupRequest
  type Output AgentCleanup = CleanupResponse
  effectId = "agent.cleanup"

data AgentCleanupBatch

instance Effect AgentCleanupBatch where
  type Input AgentCleanupBatch = CleanupBatchRequest
  type Output AgentCleanupBatch = CleanupBatchResponse
  effectId = "agent.cleanup_batch"

data AgentCleanupMerged

instance Effect AgentCleanupMerged where
  type Input AgentCleanupMerged = CleanupMergedRequest
  type Output AgentCleanupMerged = CleanupMergedResponse
  effectId = "agent.cleanup_merged"

data AgentList

instance Effect AgentList where
  type Input AgentList = ListRequest
  type Output AgentList = ListResponse
  effectId = "agent.list"

-- ============================================================================
-- Smart constructors
-- ============================================================================

spawnAgent :: SpawnRequest -> IO (Either EffectError SpawnResponse)
spawnAgent = runEffect @AgentSpawn

spawnBatch :: SpawnBatchRequest -> IO (Either EffectError SpawnBatchResponse)
spawnBatch = runEffect @AgentSpawnBatch

cleanupAgent :: CleanupRequest -> IO (Either EffectError CleanupResponse)
cleanupAgent = runEffect @AgentCleanup

cleanupBatch :: CleanupBatchRequest -> IO (Either EffectError CleanupBatchResponse)
cleanupBatch = runEffect @AgentCleanupBatch

cleanupMerged :: CleanupMergedRequest -> IO (Either EffectError CleanupMergedResponse)
cleanupMerged = runEffect @AgentCleanupMerged

listAgents :: ListRequest -> IO (Either EffectError ListResponse)
listAgents = runEffect @AgentList
