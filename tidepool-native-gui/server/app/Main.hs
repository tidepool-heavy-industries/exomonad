{-# LANGUAGE OverloadedStrings #-}
module Main where

import Tidepool.Server (runServer, ServerConfig(..))

main :: IO ()
main = runServer ServerConfig
  { scPort = 8080
  , scHost = "0.0.0.0"
  }
