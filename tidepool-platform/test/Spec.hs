module Main where

import Test.Hspec

import qualified ChatHistoryCompressionSpec

main :: IO ()
main = hspec ChatHistoryCompressionSpec.spec
