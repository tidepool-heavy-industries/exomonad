{-# LANGUAGE OverloadedStrings #-}
module Main where

import Tidepool.Server (runServer, defaultConfig)

main :: IO ()
main = runServer defaultConfig { scPort = 8080 } stubHandler
  where
    -- | Stub handler - waits forever. Replace with real handler.
    stubHandler _actionVar _stateVar = do
      putStrLn "Server running with stub handler..."
      -- Stub: wait forever (real handler would run the graph)
      _ <- getLine
      pure ()
