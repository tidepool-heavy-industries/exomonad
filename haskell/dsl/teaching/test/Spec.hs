module Main (main) where

import Test.Hspec

import qualified Tidepool.Teaching.ConvertSpec

main :: IO ()
main = hspec $ do
  describe "Tidepool.Teaching.Convert" Tidepool.Teaching.ConvertSpec.spec
