-- | Ping tool record.
module ExoMonad.Guest.Records.Ping
  ( PingTools (..),
    pingToolsHandler,
    pingToolsSchema,
  )
where

import GHC.Generics (Generic)

import ExoMonad.Guest.Tool.Mode (AsHandler, AsSchema, ToolMode ((:-)), mkHandler, mkSchema)
import ExoMonad.Guest.Tools.Ping (Ping)

-- | Ping tools record.
data PingTools mode = PingTools
  { ping :: mode :- Ping
  }
  deriving (Generic)

-- | Ping tools handler.
pingToolsHandler :: PingTools AsHandler
pingToolsHandler = PingTools {ping = mkHandler @Ping}

-- | Ping tools schema.
pingToolsSchema :: PingTools AsSchema
pingToolsSchema = PingTools {ping = mkSchema @Ping}
