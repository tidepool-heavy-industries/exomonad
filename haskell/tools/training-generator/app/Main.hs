{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (replicateM_)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Options.Applicative
import Test.QuickCheck (generate)

import Tidepool.Training.Arbitrary (generateExample)
import Tidepool.Training.Format (formatEdgeTrainingExample)

data Options = Options
  { count :: Int
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> argument auto
      ( metavar "COUNT"
     <> help "Number of training examples to generate" )

main :: IO ()
main = do
  opts <- execParser optsInfo
  replicateM_ (count opts) $ do
    example <- generate generateExample
    BSL.putStrLn $ formatEdgeTrainingExample example
  where
    optsInfo = info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc "Generate JSONL training data for FunctionGemma 270M edge scoring"
     <> header "training-generator - semantic edge training data generator" )
