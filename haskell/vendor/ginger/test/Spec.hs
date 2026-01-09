module Main where

import Test.Tasty

import Text.Ginger.SimulationTests (simulationTests)
import Text.Ginger.PropertyTests (propertyTests)
import Text.Ginger.TH.Tests (thTests)

main = defaultMain allTests

allTests :: TestTree
allTests =
    testGroup "All Tests"
        [ simulationTests
        , propertyTests
        , thTests
        ]

