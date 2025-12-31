{-# LANGUAGE OverloadedStrings #-}

-- | Generate JSON fixtures for cross-language protocol conformance tests.
--
-- This executable generates JSON files from Haskell WireTypes that are
-- then parsed by TypeScript tests using Zod schemas. If Zod fails to
-- parse, the types don't match.
--
-- Usage: cabal run generate-fixtures
-- Output: fixtures/protocol/*.json
module Main where

import Data.Aeson (ToJSON, encode, toJSON, Value(..))
import Data.ByteString.Lazy qualified as LBS
import System.Directory (createDirectoryIfMissing)

import Tidepool.Wasm.WireTypes


main :: IO ()
main = do
  let dir = "fixtures/protocol"
  createDirectoryIfMissing True dir

  -- SerializableEffect variants
  writeFixture dir "effect-llm-complete" $ EffLlmComplete
    { effNode = "classify"
    , effSystemPrompt = "You are a classifier"
    , effUserContent = "Classify this"
    , effSchema = Nothing
    }
  writeFixture dir "effect-http-fetch" $ EffHttpFetch
    { effUrl = "https://example.com"
    , effMethod = "GET"
    }
  writeFixture dir "effect-log-info" $ EffLogInfo "test message"
  writeFixture dir "effect-log-error" $ EffLogError "error message"

  -- EffectResult
  writeFixture dir "result-success" $ ResSuccess (Just (toJSON (42 :: Int)))
  writeFixture dir "result-error" $ ResError "something failed"

  -- ExecutionPhase
  writeFixture dir "phase-idle" PhaseIdle
  writeFixture dir "phase-in-node" $ PhaseInNode "classify"
  writeFixture dir "phase-transitioning" $ PhaseTransitioning "a" "b"
  writeFixture dir "phase-completed" $ PhaseCompleted (toJSON ("done" :: String))
  writeFixture dir "phase-failed" $ PhaseFailed "timeout"

  -- GraphState
  writeFixture dir "graph-state" $ GraphState (PhaseInNode "test") ["a", "b"]

  -- StepOutput
  writeFixture dir "step-output-effect" $ StepOutput
    { soEffect = Just (EffLogInfo "computing")
    , soDone = False
    , soStepResult = Nothing
    , soGraphState = GraphState PhaseIdle []
    }
  writeFixture dir "step-output-done" $ StepOutput
    { soEffect = Nothing
    , soDone = True
    , soStepResult = Just (toJSON (42 :: Int))
    , soGraphState = GraphState (PhaseCompleted (toJSON (42 :: Int))) ["compute"]
    }

  putStrLn $ "Generated fixtures in " <> dir


writeFixture :: ToJSON a => FilePath -> String -> a -> IO ()
writeFixture dir name val = do
  let path = dir <> "/" <> name <> ".json"
  LBS.writeFile path (encode val)
  putStrLn $ "  " <> path
