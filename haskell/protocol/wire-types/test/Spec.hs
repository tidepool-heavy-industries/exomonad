module Main (main) where

import Test.Hspec
import qualified Tidepool.Wire.TypesSpec

main :: IO ()
main = hspec $ do
  describe "Tidepool.Wire.Types" Tidepool.Wire.TypesSpec.spec
