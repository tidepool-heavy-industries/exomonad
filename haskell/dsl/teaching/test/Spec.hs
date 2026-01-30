module Main (main) where

import ExoMonad.Teaching.RecordSpec qualified
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "ExoMonad.Teaching.Record" ExoMonad.Teaching.RecordSpec.spec
