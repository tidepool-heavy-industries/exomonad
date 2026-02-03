{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

-- | File tool records.
module ExoMonad.Guest.Records.File
  ( FileTools (..),
    fileToolsHandler,
    fileToolsSchema,
  )
where

import ExoMonad.Guest.Tool.Mode (AsHandler, AsSchema, ToolMode ((:-)), mkHandler, mkSchema)
import ExoMonad.Guest.Tools.File (ReadFile)
import GHC.Generics (Generic)

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
