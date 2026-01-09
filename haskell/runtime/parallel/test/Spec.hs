module Main (main) where

import Test.Hspec
import qualified ParallelSpec

main :: IO ()
main = hspec $ do
  describe "Parallel" ParallelSpec.spec
