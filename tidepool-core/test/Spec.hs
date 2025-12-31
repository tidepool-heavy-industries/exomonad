module Main (main) where

import Test.Hspec

import qualified GraphValidationSpec

main :: IO ()
main = hspec $ do
  describe "Graph DSL" $ do
    GraphValidationSpec.spec
