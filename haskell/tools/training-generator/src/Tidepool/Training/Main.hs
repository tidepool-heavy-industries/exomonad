-- | CLI entry point for training data generation.
--
-- Generates JSONL training data for FunctionGemma 270M fine-tuning.
-- Uses the 2-turn minimal format (user context → model call).
module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as BSL
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Test.QuickCheck (generate, vectorOf, arbitrary)

import Tidepool.Training.Arbitrary ()
import Tidepool.Training.Format (formatEdgeTrainingLine)
import Tidepool.Training.Types (EdgeTrainingExample)


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
  putStrLn "training-generator - Generate FunctionGemma training data"
  putStrLn ""
  putStrLn "Usage:"
  putStrLn "  training-generator <count>    Generate training data (2-turn format)"
  putStrLn ""
  putStrLn "Output is written to stdout in JSONL format."
  putStrLn ""
  putStrLn "Example:"
  putStrLn "  training-generator 1000 > training.jsonl"
  putStrLn ""
  putStrLn "JSONL Format (2-turn minimal):"
  putStrLn "  {\"text\": \"<start_of_turn>user\\n...\\n<end_of_turn>\\n<start_of_turn>model\\n...\\n<end_of_turn>\"}"


-- | Generate edge training examples (2-turn minimal format).
--
-- This is the format for FunctionGemma fine-tuning.
-- Schema is "baked" into weights, no schema turn needed.
generateExamples :: Int -> IO ()
generateExamples n = do
  examples <- generate $ vectorOf n (arbitrary @EdgeTrainingExample)
  mapM_ (BSL.putStrLn . formatEdgeTrainingLine) examples
