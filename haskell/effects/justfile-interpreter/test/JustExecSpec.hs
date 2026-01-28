{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module JustExecSpec (spec) where

import Data.Aeson (eitherDecode, encode)
import Test.Hspec

import ExoMonad.Effects.JustExec

spec :: Spec
spec = do
  describe "JustExec ExecResult JSON" $ do
    it "round-trips ExecResult" $ do
      let res = ExecResult "stdout content" "stderr content" 0
          encoded = encode res
      case eitherDecode encoded of
        Left err -> expectationFailure $ "Failed to parse: " <> err
        Right res' -> (res' :: ExecResult) `shouldBe` res

    it "parses ExecResult from docker-ctl JSON format" $ do
      let json = "{\"exit_code\":123, \"stdout\":\"out\", \"stderr\":\"err\"}"
      case eitherDecode json of
        Left err -> expectationFailure $ "Failed to parse: " <> err
        Right res -> do
          (res :: ExecResult).exitCode `shouldBe` 123
          res.stdout `shouldBe` "out"
          res.stderr `shouldBe` "err"

    it "parses ExecResult with null exit_code as -1" $ do
      let json = "{\"exit_code\":null, \"stdout\":\"out\", \"stderr\":\"err\"}"
      case eitherDecode json of
        Left err -> expectationFailure $ "Failed to parse: " <> err
        Right res -> do
          (res :: ExecResult).exitCode `shouldBe` -1
