{-# LANGUAGE OverloadedStrings #-}

-- | Generate golden JSON samples for cross-language protocol verification.
--
-- This executable produces a JSON file containing canonical examples of all
-- wire types, which TypeScript can parse and validate against protocol.ts.
--
-- Run with: cabal run generate-golden-samples
-- Output: deploy/test/golden-samples.json
module Main (main) where

import Data.Aeson (Value(..), object, (.=), toJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)

import Tidepool.Wasm.WireTypes


-- | Build the golden samples as a JSON Value directly
buildGoldenSamples :: [SerializableEffect] -> [EffectResult] -> [ExecutionPhase] -> [GraphState] -> [StepOutput] -> Value
buildGoldenSamples effects results phases states outputs = object
  [ "serializableEffects" .= map toJSON effects
  , "effectResults" .= map toJSON results
  , "executionPhases" .= map toJSON phases
  , "graphStates" .= map toJSON states
  , "stepOutputs" .= map toJSON outputs
  , "_metadata" .= object
      [ "generatedBy" .= ("tidepool-wasm/golden/GenerateGoldenSamples.hs" :: Text)
      , "purpose" .= ("Cross-language protocol verification" :: Text)
      , "usage" .= ("Run verify-protocol.ts to validate TypeScript can parse these" :: Text)
      ]
  ]


-- | Sample SerializableEffect values covering all variants
sampleEffects :: [SerializableEffect]
sampleEffects =
  [ -- LlmComplete with schema
    EffLlmComplete
      { effNode = "classify"
      , effSystemPrompt = "You are a helpful classifier."
      , effUserContent = "Classify this text into categories."
      , effSchema = Just $ object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "category" .= object ["type" .= ("string" :: Text)]
              , "confidence" .= object ["type" .= ("number" :: Text)]
              ]
          ]
      , effModel = Nothing
      }
  -- LlmComplete without schema
  , EffLlmComplete
      { effNode = "respond"
      , effSystemPrompt = "You are a friendly assistant."
      , effUserContent = "Hello!"
      , effSchema = Nothing
      , effModel = Just "@cf/meta/llama-3.2-1b-instruct"  -- With explicit model
      }
  -- Log effects (without fields)
  , EffLogInfo "Processing request..." Nothing
  , EffLogInfo "" Nothing -- Empty message edge case
  , EffLogInfo "Unicode test: \x1F600 \x2764" Nothing -- Emoji
  , EffLogError "Connection failed: timeout" Nothing
  , EffLogError "Error with special chars: \"quoted\" and \\backslash\\" Nothing
  -- Habitica effects
  , EffHabitica "GetUser" (object [])
  , EffHabitica "ScoreTask" (object
      [ "taskId" .= ("abc123" :: Text)
      , "direction" .= ("up" :: Text)
      ])
  , EffHabitica "CreateTodo" (object
      [ "title" .= ("New task" :: Text)
      , "notes" .= ("Task notes here" :: Text)
      ])
  ]


-- | Sample EffectResult values
sampleResults :: [EffectResult]
sampleResults =
  [ ResSuccess Nothing  -- Null value (e.g., for Log effects)
  , ResSuccess (Just $ Number 42)
  , ResSuccess (Just $ String "result text")
  , ResSuccess (Just $ object
      [ "parsed" .= ("data" :: Text)
      , "tokens" .= (150 :: Int)
      ])
  , ResSuccess (Just $ Array mempty)  -- Empty array
  , ResError "Network timeout" Nothing
  , ResError "" Nothing  -- Empty error message edge case
  , ResError "Error with newline:\nSecond line" Nothing
  , ResError "Rate limited" (Just "rate_limited")  -- With structured error code
  ]


-- | Sample ExecutionPhase values covering all variants
samplePhases :: [ExecutionPhase]
samplePhases =
  [ PhaseIdle
  , PhaseInNode "classify"
  , PhaseInNode ""  -- Empty node name edge case
  , PhaseTransitioning "classify" "respond"
  , PhaseTransitioning "entry" "exit"
  , PhaseCompleted (String "success")
  , PhaseCompleted (Number 42)
  , PhaseCompleted (object ["answer" .= (42 :: Int)])
  , PhaseCompleted Null
  , PhaseFailed "Node execution error"
  , PhaseFailed ""  -- Empty error edge case
  ]


-- | Sample GraphState values
sampleGraphStates :: [GraphState]
sampleGraphStates =
  [ GraphState PhaseIdle []
  , GraphState (PhaseInNode "compute") []
  , GraphState (PhaseInNode "validate") ["entry", "fetch"]
  , GraphState (PhaseTransitioning "validate" "respond") ["entry", "fetch", "validate"]
  , GraphState (PhaseCompleted (String "done")) ["entry", "compute", "validate", "respond"]
  , GraphState (PhaseFailed "oops") ["entry"]
  ]


-- | Sample StepOutput values covering all variants
sampleStepOutputs :: [StepOutput]
sampleStepOutputs =
  [ -- StepYield with Log effect
    StepYield
      (EffLogInfo "Computing..." Nothing)
      (GraphState (PhaseInNode "compute") [])
  -- StepYield with LLM effect
  , StepYield
      (EffLlmComplete "classify" "sys" "user" Nothing Nothing)
      (GraphState (PhaseInNode "classify") ["entry"])
  -- StepDone with various results
  , StepDone
      (Number 42)
      (GraphState (PhaseCompleted (Number 42)) ["compute"])
  , StepDone
      (String "completed")
      (GraphState (PhaseCompleted (String "completed")) ["entry", "process", "finish"])
  , StepDone
      (object ["result" .= ("success" :: Text)])
      (GraphState (PhaseCompleted (object ["result" .= ("success" :: Text)])) [])
  , StepDone
      Null
      (GraphState (PhaseCompleted Null) [])
  -- StepFailed
  , StepFailed
      "Execution timeout"
      (GraphState (PhaseFailed "Execution timeout") ["entry", "stuck"])
  , StepFailed
      ""
      (GraphState (PhaseFailed "") [])
  ]


main :: IO ()
main = do
  let outputPath = "deploy/test/golden-samples.json"
      samples = buildGoldenSamples
        sampleEffects
        sampleResults
        samplePhases
        sampleGraphStates
        sampleStepOutputs
  BL.writeFile outputPath (encodePretty samples)
  putStrLn $ "Generated golden samples: " ++ outputPath
  putStrLn $ "  - SerializableEffect samples: " ++ show (length sampleEffects)
  putStrLn $ "  - EffectResult samples: " ++ show (length sampleResults)
  putStrLn $ "  - ExecutionPhase samples: " ++ show (length samplePhases)
  putStrLn $ "  - GraphState samples: " ++ show (length sampleGraphStates)
  putStrLn $ "  - StepOutput samples: " ++ show (length sampleStepOutputs)
