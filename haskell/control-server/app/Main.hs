module Main where

import System.Directory (getCurrentDirectory)
import System.Environment (lookupEnv)

import Tidepool.Control.Server

main :: IO ()
main = do
  -- Read project directory from environment or use current directory
  projectDirEnv <- lookupEnv "TIDEPOOL_PROJECT_DIR"
  projectDir <- case projectDirEnv of
    Just dir -> pure dir
    Nothing -> getCurrentDirectory

  let config = ServerConfig { projectDir = projectDir }

  runServer config
