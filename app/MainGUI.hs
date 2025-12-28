-- | Main entry point for the Tidepool DM GUI
--
-- This runs the GUI connected to the real DM game loop.
-- The game loop runs in a background thread while the GUI
-- server blocks the main thread.
module Main where

import Control.Concurrent (forkIO)
import Data.Text (pack)

import DM.State (initialWorld)
import DM.Loop (gameLoopWithGUI)
import DM.GUI.App (dmGUISetup, defaultDMGUIConfig)
import Tidepool.GUI.Core (newGUIBridge, logInfo)
import Tidepool.GUI.Server (startServer, defaultServerConfig, ServerConfig(..))

main :: IO ()
main = do
  -- Create the GUI bridge with initial world state
  bridge <- newGUIBridge initialWorld

  -- Event handler: log DM events to debug panel
  let handleEvent event = logInfo bridge (pack $ show event)

  -- Start game loop in background thread
  _ <- forkIO $ gameLoopWithGUI bridge handleEvent

  -- Configure and start the GUI server (blocks main thread)
  let config = defaultServerConfig
        { scTitle = "Tidepool DM - Blades in the Dark"
        }

  startServer config (dmGUISetup defaultDMGUIConfig bridge)
