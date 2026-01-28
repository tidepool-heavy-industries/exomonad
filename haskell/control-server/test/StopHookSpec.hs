module Main where

import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "StopHook" [] -- Tests removed as ErrorParser module was removed