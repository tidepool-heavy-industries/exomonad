{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (eitherDecode, encode)
import ExoMonad.Effects.Justfile
import JustExecSpec qualified
import Test.Hspec

main :: IO ()
main = hspec $ do
  JustExecSpec.spec
  describe "JustResult JSON parsing" $ do
    it "round-trips JustResult" $ do
      let res = JustResult "stdout content" "stderr content" 0
          encoded = encode res
      case eitherDecode encoded of
        Left err -> expectationFailure $ "Failed to parse: " <> err
        Right res' -> (res' :: JustResult) `shouldBe` res

    it "parses JustResult from JSON" $ do
      let json = "{\"stdout\":\"out\",\"stderr\":\"err\",\"exitCode\":1}"
      case eitherDecode json of
        Left err -> expectationFailure $ "Failed to parse: " <> err
        Right res -> do
          (res :: JustResult).stdout `shouldBe` "out"
          res.stderr `shouldBe` "err"
          res.exitCode `shouldBe` 1
