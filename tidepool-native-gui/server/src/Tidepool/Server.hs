-- | Native tidepool server - Servant + WebSocket handling.
--
-- Ties together all executors and exposes WebSocket endpoint.
module Tidepool.Server
  ( -- * Server
    runServer
  , ServerConfig(..)
  ) where

import Data.Text (Text)

-- | Server configuration.
data ServerConfig = ServerConfig
  { scPort :: Int
  , scHost :: Text
  }

-- | Run the server (stub - actual implementation by Agent 1).
runServer :: ServerConfig -> IO ()
runServer _ = putStrLn "Server stub - implementation by Agent 1"
