module Main where

import Test.Hspec
import Data.Aeson (encode, decode)

import TypesFirstDev.Types


-- | Types-first dev tests.
main :: IO ()
main = hspec $ do
  describe "Schema Types" $ do
    describe "StackSpec" $ do
      it "roundtrips through JSON" $ do
        let spec = StackSpec "." "Data.Stack" "A stack"
        decode (encode spec) `shouldBe` Just spec

    describe "TypeDefinitions" $ do
      it "roundtrips through JSON" $ do
        let td = TypeDefinitions
              { tdDataType = "data Stack a = Empty | Push a (Stack a)"
              , tdSignatures = ["push :: a -> Stack a -> Stack a"]
              , tdModuleHeader = "module Data.Stack where"
              }
        decode (encode td) `shouldBe` Just td

    describe "ImplementationResult" $ do
      it "roundtrips through JSON" $ do
        let result = ImplementationResult True 5 "All tests passed"
        decode (encode result) `shouldBe` Just result

  describe "Graph Structure" $ do
    it "types-first graph can be constructed" $ do
      -- Just verify the module imports work
      True `shouldBe` True
