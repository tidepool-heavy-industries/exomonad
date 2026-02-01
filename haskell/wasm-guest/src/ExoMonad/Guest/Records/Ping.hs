-- | Ping tool record.
module ExoMonad.Guest.Records.Ping
  ( PingTools (..),
    pingToolsHandler,
    pingToolsSchema,
  )
where

import ExoMonad.Guest.Tool.Mode (AsHandler, AsSchema, ToolMode ((:-)), mkHandler, mkSchema)
import ExoMonad.Guest.Tools.Ping (Ping)
import GHC.Generics (Generic)

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
