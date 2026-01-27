{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import ExoMonad.Effects.SocketClient (ServiceRequest(..))
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  putStrLn "Testing ServiceRequest JSON encoding..."
  
  let req = AnthropicChat "claude-3" [] 1000 Nothing Nothing Nothing
      json = LBS.unpack $ encode req
      
  putStrLn $ "Encoded: " <> json
  
  if "\"type\":\"AnthropicChat\"" `T.isInfixOf` T.pack json
    then do
      putStrLn "PASS: Type tag found"
      exitSuccess
    else do
      putStrLn "FAIL: Type tag missing"
      exitFailure
