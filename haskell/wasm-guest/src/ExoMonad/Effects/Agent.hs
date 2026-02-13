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
    AgentSpawnGeminiTeammate,
    AgentSpawnWorker,
    AgentSpawnSubtree,
    AgentCleanup,
    AgentCleanupBatch,
    AgentCleanupMerged,
    AgentList,

    -- * Smart Constructors
    spawnAgent,
    spawnBatch,
    spawnGeminiTeammate,
    spawnWorker,
    spawnSubtree,
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

data AgentSpawnGeminiTeammate

instance Effect AgentSpawnGeminiTeammate where
  type Input AgentSpawnGeminiTeammate = SpawnGeminiTeammateRequest
  type Output AgentSpawnGeminiTeammate = SpawnGeminiTeammateResponse
  effectId = "agent.spawn_gemini_teammate"

data AgentSpawnWorker

instance Effect AgentSpawnWorker where
  type Input AgentSpawnWorker = SpawnWorkerRequest
  type Output AgentSpawnWorker = SpawnWorkerResponse
  effectId = "agent.spawn_worker"

data AgentSpawnSubtree

instance Effect AgentSpawnSubtree where
  type Input AgentSpawnSubtree = SpawnSubtreeRequest
  type Output AgentSpawnSubtree = SpawnSubtreeResponse
  effectId = "agent.spawn_subtree"

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

spawnGeminiTeammate :: SpawnGeminiTeammateRequest -> IO (Either EffectError SpawnGeminiTeammateResponse)
spawnGeminiTeammate = runEffect @AgentSpawnGeminiTeammate

spawnWorker :: SpawnWorkerRequest -> IO (Either EffectError SpawnWorkerResponse)
spawnWorker = runEffect @AgentSpawnWorker

spawnSubtree :: SpawnSubtreeRequest -> IO (Either EffectError SpawnSubtreeResponse)
spawnSubtree = runEffect @AgentSpawnSubtree

cleanupAgent :: CleanupRequest -> IO (Either EffectError CleanupResponse)
cleanupAgent = runEffect @AgentCleanup

cleanupBatch :: CleanupBatchRequest -> IO (Either EffectError CleanupBatchResponse)
cleanupBatch = runEffect @AgentCleanupBatch

cleanupMerged :: CleanupMergedRequest -> IO (Either EffectError CleanupMergedResponse)
cleanupMerged = runEffect @AgentCleanupMerged

listAgents :: ListRequest -> IO (Either EffectError ListResponse)
listAgents = runEffect @AgentList
