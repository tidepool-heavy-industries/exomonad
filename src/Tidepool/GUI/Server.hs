-- | Threepenny-GUI server configuration and startup
module Tidepool.GUI.Server
  ( -- * Server configuration
    ServerConfig(..)
  , defaultServerConfig
    -- * Starting the server
  , startServer
  ) where

import Control.Monad (void)
import qualified Data.ByteString.Char8 as BS
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

-- | Server configuration
data ServerConfig = ServerConfig
  { scPort :: Int
    -- ^ Port to run on (default: 8023)
  , scAddr :: Maybe String
    -- ^ Address to bind to (default: "0.0.0.0" for all interfaces)
  , scStaticDir :: Maybe FilePath
    -- ^ Optional static files directory
  , scTitle :: String
    -- ^ Window title
  }
  deriving (Show, Eq)

-- | Default server configuration
defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig
  { scPort = 8023
  , scAddr = Just "0.0.0.0"  -- Bind to all interfaces
  , scStaticDir = Nothing
  , scTitle = "Tidepool"
  }

-- | Start the threepenny-gui server
--
-- This function blocks and runs the GUI server. The setup function
-- is called for each new browser connection.
startServer
  :: ServerConfig
  -> (Window -> UI ())  -- ^ Setup function for each window
  -> IO ()
startServer config setup = do
  let addr = maybe "0.0.0.0" id config.scAddr
  putStrLn $ "Tidepool GUI running at http://" ++ addr ++ ":" ++ show config.scPort
  let tpConfig = UI.defaultConfig
        { UI.jsPort = Just config.scPort
        , UI.jsAddr = fmap BS.pack config.scAddr
        , UI.jsStatic = config.scStaticDir
        , UI.jsLog = const (pure ())  -- Suppress default logging
        }
  UI.startGUI tpConfig $ \window -> do
    void $ return window # set UI.title config.scTitle
    setup window
