-- | Spawn tool record for hylo primitives.
module ExoMonad.Guest.Records.Spawn
  ( SpawnTools (..),
    spawnToolsHandler,
    spawnToolsSchema,
    spawnTools,
  )
where

import ExoMonad.Guest.Tool.Mode (AsHandler, AsSchema, ToolMode ((:-)), mkHandler, mkSchema)
import ExoMonad.Guest.Tools.Spawn (SpawnSubtree, SpawnWorker)
import GHC.Generics (Generic)

data SpawnTools mode = SpawnTools
  { spawnSubtree :: mode :- SpawnSubtree,
    spawnWorker :: mode :- SpawnWorker
  }
  deriving (Generic)

spawnToolsHandler :: SpawnTools AsHandler
spawnToolsHandler =
  SpawnTools
    { spawnSubtree = mkHandler @SpawnSubtree,
      spawnWorker = mkHandler @SpawnWorker
    }

spawnToolsSchema :: SpawnTools AsSchema
spawnToolsSchema =
  SpawnTools
    { spawnSubtree = mkSchema @SpawnSubtree,
      spawnWorker = mkSchema @SpawnWorker
    }

spawnTools :: SpawnTools AsHandler
spawnTools = spawnToolsHandler
