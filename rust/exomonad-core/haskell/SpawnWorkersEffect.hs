{-# LANGUAGE GADTs, DataKinds, TypeOperators, FlexibleContexts #-}
module SpawnWorkersEffect where

import Control.Monad.Freer

-- Per-tool input effect (tag 0 in HList)
data SpawnWorkersInput' a where
  GetToolInput :: SpawnWorkersInput' [WorkerSpec]

data WorkerSpec = WorkerSpec
  { wsName   :: String
  , wsPrompt :: String
  }

-- Shared Identity effect (tag 1 in HList)
-- All constructors declared for DataConTable completeness.
data Identity a where
  GetAgentId   :: Identity String
  GetParentTab :: Identity String
  GetOwnTab    :: Identity String
  GetWorkingDir :: Identity String

-- Per-tool domain op (tag 2 in HList)
data SpawnWorkerOp a where
  SpawnWorker :: String -> String -> SpawnWorkerOp SpawnWorkerResult

data SpawnWorkerResult = SpawnWorkerResult
  { swrTabName :: String
  }

-- Entry point: multi-effect composition.
spawnWorkersTool :: Eff '[SpawnWorkersInput', Identity, SpawnWorkerOp] [SpawnWorkerResult]
spawnWorkersTool = do
  specs <- send GetToolInput
  _ <- send GetAgentId
  mapM (\spec -> send (SpawnWorker (wsName spec) (wsPrompt spec))) specs
