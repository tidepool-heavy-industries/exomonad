-- | Native tidepool server - Servant + WebSocket handling.
--
-- Ties together all executors and exposes WebSocket endpoint.
module Tidepool.Server
  ( -- * Server
    runServer
  , ServerConfig(..)
  , defaultConfig
    -- * Re-exports
  , module Tidepool.Server.API
  , module Tidepool.Server.WebSocket
  ) where

import Data.Text (Text)
import Network.HTTP.Types (status400)
import Network.Wai (responseLBS)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets as WS

import Tidepool.Server.API
import Tidepool.Server.WebSocket

-- | Server configuration.
data ServerConfig = ServerConfig
  { scPort :: Int
  , scHost :: Text
  , scStaticDir :: Maybe FilePath
  }

-- | Default server configuration.
defaultConfig :: ServerConfig
defaultConfig = ServerConfig
  { scPort = 8080
  , scHost = "localhost"
  , scStaticDir = Nothing
  }

-- | Run the server with WebSocket support.
runServer :: ServerConfig -> SessionHandler -> IO ()
runServer config handler = do
  putStrLn $ "Starting server on port " ++ show (scPort config)

  let wsApp = websocketApp handler
      fallback _ respond = respond $ responseLBS status400 [] "WebSocket required"
      -- Combine WebSocket with static file serving
      app = case scStaticDir config of
        Nothing -> WaiWS.websocketsOr WS.defaultConnectionOptions wsApp fallback
        Just dir -> WaiWS.websocketsOr WS.defaultConnectionOptions wsApp (tidepoolApp dir)

  Warp.run (scPort config) app
