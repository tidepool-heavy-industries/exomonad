{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}

-- | POC for aeson-schemas integration.
--
-- Demonstrates type-safe JSON schema definition and parsing.
module ExoMonad.Schema.POC where

import Data.Aeson (Value, encode)
import Data.Aeson.Schema
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Prelude hiding (get)

-- | Define the schema for a Task Extraction result.
--
-- The schema is defined at the type level.
-- The compiler generates the parsing logic.
type ExtractTaskSchema =
  [schema|
    {
      "title": Text,
      "difficulty": Text,
      "tags": List Text,
      "estimated_hours": Maybe Double
    }
  |]

-- | A type alias for an object matching the schema.
type TaskResult = Object ExtractTaskSchema

-- | Example handler that consumes the strongly-typed object.
--
-- No need to define a 'data TaskResult = ...' record!
-- Access fields using type-safe getters: [get| .field |]
processTask :: TaskResult -> IO ()
processTask task = do
  TIO.putStrLn $ "Title: " <> [get| task.title |]
  TIO.putStrLn $ "Difficulty: " <> [get| task.difficulty |]
  print $ "Tags: " <> [get| task.tags |]
  case [get| task.estimated_hours |] of
    Just hours -> print $ "Est: " <> show hours <> "h"
    Nothing -> putStrLn "No estimate"

-- | Parse generic JSON Value into the typed Object.
--
-- Returns a result that proves the JSON matches the schema.
parseTask :: Value -> Either String TaskResult
parseTask = parseValue @ExtractTaskSchema

-- | TODO: Generate JSON Schema for LLM from ExtractTaskSchema.
--
-- aeson-schemas doesn't output "JSON Schema" (the spec) by default,
-- but we can introspect the type to generate it.
-- This would replace our 'StructuredOutput' class.
