{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit

import ExoMonad.Control.LSPTools (parseADTConstructors, Constructor(..), stripDerivingClause)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Constructor Parser"
    [ testGroup "stripDerivingClause"
        [ testCase "strips single line deriving" $
            stripDerivingClause "Indexing | Ready deriving (Show, Eq)" @?= "Indexing | Ready"
        , testCase "strips multi-line deriving" $
            stripDerivingClause "Indexing | Ready\n  deriving (Show, Eq)\n  deriving Generic" @?= "Indexing | Ready\n"
        ]
  ,
    testGroup "parseADTConstructors"
      [
        testCase "IndexingState returns only Indexing and Ready" $ do
          let rawDef = "data IndexingState = Indexing | Ready deriving (Show, Eq, Generic)"
              cons = parseADTConstructors rawDef
          map conName cons @?= ["Indexing", "Ready"]
          concatMap conFields cons @?= []
      , testCase "Record with deriving stripped correctly" $ do
          let rawDef = "data Foo = Bar { x :: Int, y :: String } deriving (Show, Eq)"
              cons = parseADTConstructors rawDef
          map conName cons @?= ["Bar"]
          case cons of
            [c] -> conFields c @?= ["Int", "String"]
            _ -> assertFailure "Expected exactly one constructor"
      ]
  ]
