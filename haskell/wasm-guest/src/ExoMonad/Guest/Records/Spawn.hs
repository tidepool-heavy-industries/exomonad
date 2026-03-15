-- | Spawn tool record for hylo primitives.
module ExoMonad.Guest.Records.Spawn
  ( SpawnTools (..),
    spawnToolsHandler,
    spawnToolsSchema,
    spawnTools,
  )
where

import ExoMonad.Guest.Tool.Mode (AsHandler, AsSchema, ToolMode ((:-)), mkHandler, mkSchema)
import ExoMonad.Guest.Tools.Spawn (ForkWave, SpawnLeafSubtree, SpawnWorkers)
import GHC.Generics (Generic)

data SpawnTools mode = SpawnTools
  { forkWave :: mode :- ForkWave,
    spawnLeafSubtree :: mode :- SpawnLeafSubtree,
    spawnWorkers :: mode :- SpawnWorkers
  }
  deriving (Generic)

spawnToolsHandler :: SpawnTools AsHandler
spawnToolsHandler =
  SpawnTools
    { forkWave = mkHandler @ForkWave,
      spawnLeafSubtree = mkHandler @SpawnLeafSubtree,
      spawnWorkers = mkHandler @SpawnWorkers
    }

spawnToolsSchema :: SpawnTools AsSchema
spawnToolsSchema =
  SpawnTools
    { forkWave = mkSchema @ForkWave,
      spawnLeafSubtree = mkSchema @SpawnLeafSubtree,
      spawnWorkers = mkSchema @SpawnWorkers
    }

spawnTools :: SpawnTools AsHandler
spawnTools = spawnToolsHandler
