-- | FilePR tool record.
module ExoMonad.Guest.Records.FilePR
  ( FilePRTools (..),
    filePRToolsHandler,
    filePRToolsSchema,
    filePRTools, -- Convenience alias for handler
  )
where

import ExoMonad.Guest.Tool.Mode (AsHandler, AsSchema, ToolMode ((:-)), mkHandler, mkSchema)
import ExoMonad.Guest.Tools.FilePR (FilePR)
import GHC.Generics (Generic)

-- | FilePR tools record.
data FilePRTools mode = FilePRTools
  { filePR :: mode :- FilePR
  }
  deriving (Generic)

-- | FilePR tools handler.
filePRToolsHandler :: FilePRTools AsHandler
filePRToolsHandler = FilePRTools {filePR = mkHandler @FilePR}

-- | FilePR tools schema.
filePRToolsSchema :: FilePRTools AsSchema
filePRToolsSchema = FilePRTools {filePR = mkSchema @FilePR}

-- | Default handler instance for use in Role.hs
filePRTools :: FilePRTools AsHandler
filePRTools = filePRToolsHandler
