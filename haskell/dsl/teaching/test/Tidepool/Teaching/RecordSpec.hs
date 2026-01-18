{-# LANGUAGE OverloadedStrings #-}

module Tidepool.Teaching.RecordSpec (spec) where

import Test.Hspec
import Data.Aeson (object, (.=))
import Data.Maybe (fromJust)
import Data.Time (getCurrentTime)
import qualified Data.UUID as UUID
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Tidepool.Teaching.Record
import Tidepool.Teaching.Types

-- Helper to create a test UUID
testUUID :: UUID.UUID
testUUID = fromJust $ UUID.fromString "12345678-1234-5678-1234-567812345678"

spec :: Spec
spec = do
  describe "initRecording" $ do
    it "creates session directory and files" $ do
      withSystemTempDirectory "test-recording" $ \tmpDir -> do
        let sessionId = testUUID
        handles <- initRecording tmpDir sessionId

        let sessionDir = tmpDir </> ("session-" <> UUID.toString sessionId)
        doesDirectoryExist sessionDir `shouldReturn` True
        doesFileExist (sessionDir </> "anthropic.jsonl") `shouldReturn` True
        doesFileExist (sessionDir </> "gemma.jsonl") `shouldReturn` True

        closeRecording handles

  describe "recordTurn" $ do
    it "writes turn to anthropic.jsonl" $ do
      withSystemTempDirectory "test-recording" $ \tmpDir -> do
        let sessionId = testUUID
        handles <- initRecording tmpDir sessionId

        now <- getCurrentTime
        let turn = TeachingTurn
              { ttNodeName = "gClassify"
              , ttGraphName = "SupportGraph"
              , ttSystemPrompt = "You are a helpful assistant."
              , ttUserContent = object ["message" .= ("hello" :: String)]
              , ttOutputSchema = object ["type" .= ("string" :: String)]
              , ttToolDefs = []
              , ttResponse = object ["content" .= ("world" :: String)]
              , ttTimestamp = now
              }

        recordTurn handles turn

        closeRecording handles

        -- Verify anthropic.jsonl contains the turn
        let sessionDir = tmpDir </> ("session-" <> UUID.toString sessionId)
        anthropicContent <- readFile (sessionDir </> "anthropic.jsonl")
        anthropicContent `shouldContain` "gClassify"
        anthropicContent `shouldContain` "SupportGraph"
        anthropicContent `shouldContain` "helpful assistant"

  describe "writeMetadata" $ do
    it "writes metadata.json with session info" $ do
      withSystemTempDirectory "test-recording" $ \tmpDir -> do
        let sessionId = testUUID
        let sessionDir = tmpDir </> ("session-" <> UUID.toString sessionId)
        let config = TeachingConfig
              { tcEnabled = True
              , tcOutputDir = tmpDir
              , tcSessionId = sessionId
              , tcAnthropicKey = "test-key"
              }

        -- Create the session directory first (writeMetadata expects it to exist)
        createDirectoryIfMissing True sessionDir
        writeMetadata sessionDir config

        doesFileExist (sessionDir </> "metadata.json") `shouldReturn` True

        metaContent <- readFile (sessionDir </> "metadata.json")
        metaContent `shouldContain` UUID.toString sessionId
        metaContent `shouldContain` "\"version\":\"0.1.0\""
