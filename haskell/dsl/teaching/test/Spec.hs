module Main (main) where

import Test.Hspec
import qualified Tidepool.Teaching.ConvertSpec
import qualified Tidepool.Teaching.RecordSpec

main :: IO ()
main = hspec $ do
  describe "Tidepool.Teaching.Convert" Tidepool.Teaching.ConvertSpec.spec
  describe "Tidepool.Teaching.Record" Tidepool.Teaching.RecordSpec.spec
