module Main where

import Data.Text (pack)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import Tidepool.Control.Server

main :: IO ()
main = do
  -- Read config from environment
  hostEnv <- lookupEnv "MANTLE_CONTROL_HOST"
  portEnv <- lookupEnv "MANTLE_CONTROL_PORT"

  let config = defaultConfig
        { host = maybe (host defaultConfig) pack hostEnv
        , port = maybe (port defaultConfig) id (portEnv >>= readMaybe)
        }

  runServer config
