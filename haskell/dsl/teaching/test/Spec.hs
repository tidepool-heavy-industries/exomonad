module Main (main) where

import Test.Hspec
import qualified ExoMonad.Teaching.RecordSpec

main :: IO ()
main = hspec $ do
  describe "ExoMonad.Teaching.Record" ExoMonad.Teaching.RecordSpec.spec
