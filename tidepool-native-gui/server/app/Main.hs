{-# LANGUAGE OverloadedStrings #-}
module Main where

import Tidepool.Server (runServer, ServerConfig(..))
import Tidepool.Server.EffectRunner (loadExecutorConfig)

main :: IO ()
main = do
  -- Load executor configuration from environment variables
  executorConfig <- loadExecutorConfig

  runServer ServerConfig
    { scPort = 8080
    , scHost = "0.0.0.0"
    , scExecutorConfig = executorConfig
    }
