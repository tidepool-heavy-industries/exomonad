-- | Spawn tool record for hylo primitives.
module ExoMonad.Guest.Records.Spawn
  ( SpawnTools (..),
    spawnToolsHandler,
    spawnToolsSchema,
    spawnTools,
  )
where

import ExoMonad.Guest.Tool.Mode (AsHandler, AsSchema, ToolMode ((:-)), mkHandler, mkSchema)
import ExoMonad.Guest.Tools.Spawn (SpawnSubtree, SpawnLeafSubtree, SpawnWorkers)
import GHC.Generics (Generic)

data SpawnTools mode = SpawnTools
  { spawnSubtree :: mode :- SpawnSubtree,
    spawnLeafSubtree :: mode :- SpawnLeafSubtree,
    spawnWorkers :: mode :- SpawnWorkers
  }
  deriving (Generic)

spawnToolsHandler :: SpawnTools AsHandler
spawnToolsHandler =
  SpawnTools
    { spawnSubtree = mkHandler @SpawnSubtree,
      spawnLeafSubtree = mkHandler @SpawnLeafSubtree,
      spawnWorkers = mkHandler @SpawnWorkers
    }

spawnToolsSchema :: SpawnTools AsSchema
spawnToolsSchema =
  SpawnTools
    { spawnSubtree = mkSchema @SpawnSubtree,
      spawnLeafSubtree = mkSchema @SpawnLeafSubtree,
      spawnWorkers = mkSchema @SpawnWorkers
    }

spawnTools :: SpawnTools AsHandler
spawnTools = spawnToolsHandler
