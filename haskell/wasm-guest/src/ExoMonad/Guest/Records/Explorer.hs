-- | Explorer tool record and handlers.
module ExoMonad.Guest.Records.Explorer
  ( ExplorerTools (..),
    explorerToolsHandler,
    explorerToolsSchema,
  )
where

import ExoMonad.Guest.Tool.Mode (AsHandler, AsSchema, ToolMode ((:-)), mkHandler, mkSchema)
import ExoMonad.Guest.Tools.Explorer (ContinueExploration, ExploreCodebase)
import GHC.Generics (Generic)

-- | Explorer tools record.
data ExplorerTools mode = ExplorerTools
  { exploreCodebase :: mode :- ExploreCodebase,
    continueExploration :: mode :- ContinueExploration
  }
  deriving (Generic)

-- | Explorer tools handler record.
explorerToolsHandler :: ExplorerTools AsHandler
explorerToolsHandler =
  ExplorerTools
    { exploreCodebase = mkHandler @ExploreCodebase,
      continueExploration = mkHandler @ContinueExploration
    }

-- | Explorer tools schema record.
explorerToolsSchema :: ExplorerTools AsSchema
explorerToolsSchema =
  ExplorerTools
    { exploreCodebase = mkSchema @ExploreCodebase,
      continueExploration = mkSchema @ContinueExploration
    }
