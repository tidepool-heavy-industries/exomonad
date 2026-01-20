module Main where

import Test.Hspec
import Control.Monad (when)
import Control.Monad.Freer (runM)
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as LBS
import Data.Time (UTCTime(..), fromGregorian)
import System.Directory (doesFileExist, removeFile)
import Tidepool.Effect.Runners (runDecisionLog)
import Tidepool.Effect.Types (Decision(..), DecisionContext(..), DecisionTrace(..), recordDecision)

main :: IO ()
main = hspec $ do
  describe "tidepool-platform" $ do
    describe "Decision Logging" $ do
      it "writes decision trace to .tidepool/decision_log.jsonl" $ do
        let logFile = ".tidepool/decision_log.jsonl"
        -- Cleanup before test
        exists <- doesFileExist logFile
        when exists $ removeFile logFile

        let trace = DecisionTrace
              { dtContext = DecisionContext "Test prompt" ["bead-1"]
              , dtOptionsPresented = ["Select Bead: bead-1", "Continue", "Abort", "Provide Guidance"]
              , dtDecision = Continue
              , dtLatencyMs = 500
              , dtTimestamp = UTCTime (fromGregorian 2026 1 19) 0
              }

        runM . runDecisionLog $ recordDecision trace

        fileExisted <- doesFileExist logFile
        fileExisted `shouldBe` True

        content <- LBS.readFile logFile
        -- The log file has one JSON per line, so we need to decode the first line
        let firstLine = head (LBS.split 10 content) -- 10 is '\n'
        let decoded = decode firstLine :: Maybe DecisionTrace
        decoded `shouldBe` Just trace
