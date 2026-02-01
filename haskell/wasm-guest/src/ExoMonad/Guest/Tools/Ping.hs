-- | Simple ping tool to verify WASM guest is responsive.
module ExoMonad.Guest.Tools.Ping
  ( Ping,
    PingArgs (..),
  )
where

import Data.Aeson (FromJSON, Value, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

import ExoMonad.Guest.Tool.Class

-- | Simple ping tool - returns "pong" to verify guest is alive.
data Ping

data PingArgs = PingArgs
  deriving (Show, Eq, Generic)

instance FromJSON PingArgs where
  parseJSON = Aeson.withObject "PingArgs" $ \_ -> pure PingArgs

instance MCPTool Ping where
  type ToolArgs Ping = PingArgs
  toolName = "ping"
  toolDescription = "Verify WASM guest is responsive (returns pong)"
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "properties" .= object []
      ]
  toolHandler _args = pure $ successResult $ Aeson.String "pong"
