{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck
import ExoMonad.Training.Arbitrary (heuristicScoreEdge)
import ExoMonad.Training.Types (ScoreEdgeOutput(..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "heuristicScoreEdge" $ do
    it "produces relevance scores within valid range (1-5)" $ property $
      \input ->
        let output = heuristicScoreEdge input
        in output.relevance `shouldSatisfy` (\x -> x >= 1 && x <= 5)
    
    it "produces risk scores within valid range (1-5)" $ property $
      \input ->
        let output = heuristicScoreEdge input
        in output.risk `shouldSatisfy` (\x -> x >= 1 && x <= 5)

    it "always includes reasoning" $ property $
      \input ->
        let output = heuristicScoreEdge input
        in not (T.null (output.reasoning))