module Main (main) where

import ParsersSpec qualified
import Test.Hspec
import ToolsSpec qualified

main :: IO ()
main = hspec $ do
  describe "Tools" ToolsSpec.spec
  describe "Parsers" ParsersSpec.spec
