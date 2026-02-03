{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

-- | File tool records.
module ExoMonad.Guest.Records.File
  ( FileTools (..),
    fileToolsHandler,
    fileToolsSchema,
    fileTools, -- Convenience alias for handler
  )
where

import ExoMonad.Guest.Tool.Mode (AsHandler, AsSchema, ToolMode ((:-)), mkHandler, mkSchema)
import ExoMonad.Guest.Tools.File (ReadFile)
import GHC.Generics (Generic)
import Prelude hiding (readFile)

data FileTools mode = FileTools
  { readFile :: mode :- ReadFile
  }
  deriving (Generic)

fileToolsHandler :: FileTools AsHandler
fileToolsHandler =
  FileTools
    { readFile = mkHandler @ReadFile
    }

fileToolsSchema :: FileTools AsSchema
fileToolsSchema =
  FileTools
    { readFile = mkSchema @ReadFile
    }

-- | Default handler instance for use in Role.hs
fileTools :: FileTools AsHandler
fileTools = fileToolsHandler
