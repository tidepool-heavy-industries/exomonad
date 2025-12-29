{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Main entry point for the Tidying GUI
--
-- Run with: cabal run tidepool-tidy-gui
-- Opens a browser at http://localhost:8024
--
-- The GUI runs on the main thread (threepenny event loop).
-- The tidying agent runs in a background thread, communicating
-- via the GUIBridge (TVars for state, MVars for input).
module Main where

import Control.Concurrent (forkIO)

import Tidying.State (newSession, SessionState(..), Phase)
import Tidying.GUI.App (tidyingGUISetup, defaultTidyingGUIConfig)
import Tidying.GUI.Runner (tidyingGameLoopWithGUI)
import Tidepool.GUI.Core (newGUIBridge)
import Tidepool.GUI.Server (startServer, ServerConfig(..), defaultServerConfig)

main :: IO ()
main = do
  putStrLn "Starting Tidying GUI..."
  putStrLn "Open http://localhost:8024 in your browser"
  putStrLn "(Make sure ANTHROPIC_API_KEY is set)"

  -- Create the bridge with initial session state
  bridge <- newGUIBridge newSession

  -- Spawn agent loop ONCE (not per-connection!)
  -- The agent thread persists across browser reconnects
  _ <- forkIO $ tidyingGameLoopWithGUI bridge
  putStrLn "Tidepool GUI running at http://localhost:8024"

  -- Start the server
  let config = defaultServerConfig
        { scPort = 8024
        , scTitle = "Tidying Session"
        }

  startServer config $ \window -> do
    -- Set up GUI for this connection (can happen multiple times)
    tidyingGUISetup defaultTidyingGUIConfig bridge getPhase window

-- | Get phase from session state (workaround for HasField)
getPhase :: SessionState -> Phase
getPhase SessionState{phase} = phase
