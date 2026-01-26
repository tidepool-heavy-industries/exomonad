{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (lookupEnv)
import ExoMonad.Server (runServer, ServerConfig(..), ServerMode(..), simpleAgent)
import ExoMonad.Server.EffectRunner (loadInterpreterConfig)

main :: IO ()
main = do
  -- Load interpreter configuration from environment variables
  interpreterConfig <- loadInterpreterConfig

  -- Check for environment variable to set mode
  modeEnv <- lookupEnv "EXOMONAD_MODE"
  distEnv <- lookupEnv "EXOMONAD_DIST"

  let mode = case modeEnv of
        Just "dev" -> DevProxy 3000
        _ -> StaticFiles (maybe "solid-frontend/dist" id distEnv)

  runServer ServerConfig
    { scPort = 8080
    , scHost = "0.0.0.0"
    , scInterpreterConfig = interpreterConfig
    , scMode = mode
    , scAgent = simpleAgent
    , scGraphInfo = Nothing  -- SimpleAgent is a plain agent, no graph to visualize
    }
