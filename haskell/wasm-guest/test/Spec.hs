module Main (main) where

import Test.Hspec

import qualified ToolsSpec
import qualified ParsersSpec

main :: IO ()
main = hspec $ do
  describe "Tools" ToolsSpec.spec
  describe "Parsers" ParsersSpec.spec
