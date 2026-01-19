{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Data.Aeson (eitherDecode, encode)
import Test.Hspec

import Tidepool.Effects.Justfile

main :: IO ()
main = hspec $ do
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