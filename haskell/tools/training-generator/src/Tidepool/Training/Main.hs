-- | CLI entry point for training data generation.
module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as BSL
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Test.QuickCheck (generate, vectorOf, arbitrary)

import Tidepool.Training.Arbitrary ()
import Tidepool.Training.Format (formatTrainingLine)
import Tidepool.Training.Heuristics (scoreExample)
import Tidepool.Training.Types (QueryContext, NodeContext)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [nStr] | Just n <- readMaybe nStr -> generateExamples n
    ["--help"] -> printUsage
    ["-h"] -> printUsage
    _ -> printUsage


printUsage :: IO ()
printUsage = do
  putStrLn "Usage: training-generator <count>"
  putStrLn ""
  putStrLn "Generate <count> training examples in FunctionGemma JSONL format."
  putStrLn "Output is written to stdout."
  putStrLn ""
  putStrLn "Example:"
  putStrLn "  training-generator 1000 > training-data.jsonl"


generateExamples :: Int -> IO ()
generateExamples n = do
  -- Generate random query/node pairs
  pairs <- generate $ vectorOf n (arbitrary @(QueryContext, NodeContext))

  -- Score each pair with heuristics and format as JSONL
  mapM_ outputExample pairs


outputExample :: (QueryContext, NodeContext) -> IO ()
outputExample (query, node) = do
  let example = scoreExample query node
  BSL.putStrLn (formatTrainingLine example)
