-- | Spawn core re-exports for role code.
module ExoMonad.Guest.Records.Spawn
  ( -- * ForkWave
    ForkWaveArgs (..),
    ForkWaveChild (..),
    ForkWaveResult (..),
    forkWaveCore,
    forkWaveRender,
    forkWaveDescription,
    forkWaveSchema,

    -- * SpawnLeafSubtree
    SpawnLeafSubtreeArgs (..),
    spawnLeafSubtreeCore,
    spawnLeafRender,
    spawnLeafSubtreeDescription,
    spawnLeafSubtreeSchema,

    -- * SpawnWorkers
    SpawnWorkersArgs (..),
    WorkerSpec (..),
    WorkerType (..),
    spawnWorkersCore,
    spawnWorkersDescription,
    spawnWorkersSchema,

    -- * SpawnAcp
    SpawnAcpArgs (..),
    spawnAcpCore,
  )
where

import ExoMonad.Guest.Tools.Spawn
