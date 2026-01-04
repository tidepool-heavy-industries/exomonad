module Main (main) where

import Test.Hspec

import qualified WebSocketSpec
import qualified StaticFileSpec

main :: IO ()
main = hspec $ do
  describe "WebSocket" $ do
    WebSocketSpec.spec

  describe "Static File Serving" $ do
    StaticFileSpec.spec
