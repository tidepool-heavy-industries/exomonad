{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck
import Tidepool.Training.Arbitrary (heuristicScoreEdge)
import Tidepool.Training.Types (ScoreEdgeOutput(..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "heuristicScoreEdge" $ do
    it "produces relevance scores within valid range (1-5)" $ property $
      \input ->
        let output = heuristicScoreEdge input
        in seoRelevance output `shouldSatisfy` (\x -> x >= 1 && x <= 5)
    
    it "produces risk scores within valid range (1-5)" $ property $
      \input ->
        let output = heuristicScoreEdge input
        in seoRisk output `shouldSatisfy` (\x -> x >= 1 && x <= 5)

    it "always includes reasoning" $ property $
      \input ->
        let output = heuristicScoreEdge input
        in not (T.null (seoReasoning output))