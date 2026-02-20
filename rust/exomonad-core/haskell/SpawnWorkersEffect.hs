{-# LANGUAGE GADTs, DataKinds, TypeOperators, FlexibleContexts #-}
module SpawnWorkersEffect where

import Control.Monad.Freer

-- Effect GADT. Constructor names must match Rust #[core(name = "...")] exactly.
data SpawnWorkersOp a where
  GetToolInput :: SpawnWorkersOp [WorkerSpec]
  SpawnWorker  :: String -> String -> SpawnWorkersOp SpawnWorkerResult

-- Worker spec. Prompt is pre-rendered by Rust from the full spec fields.
data WorkerSpec = WorkerSpec
  { wsName   :: String
  , wsPrompt :: String
  }

-- Result returned to caller per spawned worker.
data SpawnWorkerResult = SpawnWorkerResult
  { swrTabName :: String
  }

-- Entry point: iterate over specs, spawn each worker.
spawnWorkersTool :: Eff '[SpawnWorkersOp] [SpawnWorkerResult]
spawnWorkersTool = do
  specs <- send GetToolInput
  mapM (\spec -> send (SpawnWorker (wsName spec) (wsPrompt spec))) specs
