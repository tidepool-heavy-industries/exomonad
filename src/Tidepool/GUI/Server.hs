-- | Threepenny-GUI server configuration and startup
module Tidepool.GUI.Server
  ( -- * Server configuration
    ServerConfig(..)
  , defaultServerConfig
    -- * Starting the server
  , startServer
  ) where

import Control.Monad (void)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

-- | Server configuration
data ServerConfig = ServerConfig
  { scPort :: Int
    -- ^ Port to run on (default: 8023)
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
  putStrLn $ "Tidepool GUI running at http://localhost:" ++ show config.scPort
  let tpConfig = UI.defaultConfig
        { UI.jsPort = Just config.scPort
        , UI.jsStatic = config.scStaticDir
        , UI.jsLog = const (pure ())  -- Suppress default logging
        }
  UI.startGUI tpConfig $ \window -> do
    void $ return window # set UI.title config.scTitle
    setup window
