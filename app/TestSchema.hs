-- | Schema derivation test: verify Haddock docs appear in JSON schema
module Main where

import Data.Aeson (Value(..), encode)
import Data.Aeson.Key (fromText)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Text (Text)
import qualified Data.Text as T

import Tidepool.Schema (schemaToValue)
import DM.Output (turnOutputJSONSchema)

main :: IO ()
main = do
  putStrLn "Schema Derivation Test: Verify Haddock → JSON Schema"
  putStrLn "====================================================="

  let schema = schemaToValue turnOutputJSONSchema

  -- Test 1: narration field has description from Haddock
  putStrLn "\n[1] Checking 'narration' field description..."
  case getFieldDescription "narration" schema of
    Nothing -> fail "  ✗ No description found for 'narration'"
    Just desc
      | "Narrative prose" `T.isInfixOf` desc ->
          putStrLn $ "  ✓ Found expected description: " <> T.unpack (T.take 60 desc) <> "..."
      | otherwise ->
          fail $ "  ✗ Unexpected description: " <> T.unpack desc

  -- Test 2: stressDelta field has description with range
  putStrLn "\n[2] Checking 'stressDelta' field description..."
  case getFieldDescription "stressDelta" schema of
    Nothing -> fail "  ✗ No description found for 'stressDelta'"
    Just desc
      | "-9 to +9" `T.isInfixOf` desc ->
          putStrLn $ "  ✓ Found expected description: " <> T.unpack (T.take 60 desc) <> "..."
      | otherwise ->
          fail $ "  ✗ Unexpected description: " <> T.unpack desc

  -- Test 3: continueScene field has description
  putStrLn "\n[3] Checking 'continueScene' field description..."
  case getFieldDescription "continueScene" schema of
    Nothing -> fail "  ✗ No description found for 'continueScene'"
    Just desc
      | "True" `T.isInfixOf` desc && "False" `T.isInfixOf` desc ->
          putStrLn $ "  ✓ Found expected description: " <> T.unpack (T.take 60 desc) <> "..."
      | otherwise ->
          fail $ "  ✗ Unexpected description: " <> T.unpack desc

  -- Print full schema for inspection
  putStrLn "\n[Full Schema JSON]"
  LBS.putStrLn $ encode schema

  putStrLn "\n✓ All schema tests passed!"

-- | Extract description from a field in the schema
getFieldDescription :: Text -> Value -> Maybe Text
getFieldDescription fieldName (Object obj) = do
  Object props <- KM.lookup (fromText "properties") obj
  Object field <- KM.lookup (fromText fieldName) props
  String desc <- KM.lookup (fromText "description") field
  pure desc
getFieldDescription _ _ = Nothing
