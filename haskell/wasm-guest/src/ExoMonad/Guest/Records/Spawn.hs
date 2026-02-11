-- | Spawn tool record for hylo primitives.
module ExoMonad.Guest.Records.Spawn
  ( SpawnTools (..),
    spawnToolsHandler,
    spawnToolsSchema,
    spawnTools,
  )
where

import ExoMonad.Guest.Tool.Mode (AsHandler, AsSchema, ToolMode ((:-)), mkHandler, mkSchema)
import ExoMonad.Guest.Tools.Spawn (SpawnSubtree, SpawnLeaf)
import GHC.Generics (Generic)

data SpawnTools mode = SpawnTools
  { spawnSubtree :: mode :- SpawnSubtree,
    spawnLeaf :: mode :- SpawnLeaf
  }
  deriving (Generic)

spawnToolsHandler :: SpawnTools AsHandler
spawnToolsHandler =
  SpawnTools
    { spawnSubtree = mkHandler @SpawnSubtree,
      spawnLeaf = mkHandler @SpawnLeaf
    }

spawnToolsSchema :: SpawnTools AsSchema
spawnToolsSchema =
  SpawnTools
    { spawnSubtree = mkSchema @SpawnSubtree,
      spawnLeaf = mkSchema @SpawnLeaf
    }

spawnTools :: SpawnTools AsHandler
spawnTools = spawnToolsHandler
