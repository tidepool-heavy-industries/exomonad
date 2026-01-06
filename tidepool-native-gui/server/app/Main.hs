{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (lookupEnv)
import Tidepool.Server (runServer, ServerConfig(..), ServerMode(..), simpleAgent)
import Tidepool.Server.EffectRunner (loadExecutorConfig)

main :: IO ()
main = do
  -- Load executor configuration from environment variables
  executorConfig <- loadExecutorConfig

  -- Check for environment variable to set mode
  modeEnv <- lookupEnv "TIDEPOOL_MODE"
  distEnv <- lookupEnv "TIDEPOOL_DIST"

  let mode = case modeEnv of
        Just "dev" -> DevProxy 3000
        _ -> StaticFiles (maybe "solid-frontend/dist" id distEnv)

  runServer ServerConfig
    { scPort = 8080
    , scHost = "0.0.0.0"
    , scExecutorConfig = executorConfig
    , scMode = mode
    , scAgent = simpleAgent
    , scGraphInfo = Nothing  -- SimpleAgent is a plain agent, no graph to visualize
    }
