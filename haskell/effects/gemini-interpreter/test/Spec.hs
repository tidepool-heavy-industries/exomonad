{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Test.Hspec
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Text as T
import Tidepool.Gemini.Interpreter
import Tidepool.Effect.Gemini (GeminiResult(..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseGeminiOutput" $ do
    it "parses valid JSON output" $ do
      let stdout = "{\"result\": \"success\", \"code\": 200}"
      let res = parseGeminiOutput stdout
      res.grOutput `shouldBe` object ["result" .= ("success" :: T.Text), "code" .= (200 :: Int)]
      res.grRawResponse `shouldBe` T.pack stdout

    it "handles invalid JSON by returning Null" $ do
      let stdout = "not a json"
      let res = parseGeminiOutput stdout
      res.grOutput `shouldBe` Null
      res.grRawResponse `shouldBe` T.pack stdout

    it "handles empty output" $ do
      let stdout = ""
      let res = parseGeminiOutput stdout
      res.grOutput `shouldBe` Null
      res.grRawResponse `shouldBe` ""
