module Main where

import Test.Hspec
import Control.Monad (when)
import Control.Monad.Freer (runM)
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as LBS
import Data.Time (UTCTime(..), fromGregorian)
import System.Directory (doesFileExist, removeFile)
import ExoMonad.Effect.Runners (runDecisionLog)
import ExoMonad.Effect.Types (Decision(..), DecisionContext(..), DecisionTrace(..), recordDecision)

main :: IO ()
main = hspec $ do
  describe "exomonad-platform" $ do
    describe "Decision Logging" $ do
      it "writes decision trace to .exomonad/decision_log.jsonl" $ do
        let logFile = ".exomonad/decision_log.jsonl"
        -- Cleanup before test
        exists <- doesFileExist logFile
        when exists $ removeFile logFile

        let trace = DecisionTrace
              { dtContext = DecisionContext "Test prompt" ["issue-1"]
              , dtOptionsPresented = ["Select Issue: issue-1", "Continue", "Abort", "Provide Guidance"]
              , dtDecision = Continue
              , dtLatencyMs = 500
              , dtTimestamp = UTCTime (fromGregorian 2026 1 19) 0
              }

        runM . runDecisionLog $ recordDecision trace

        fileExisted <- doesFileExist logFile
        fileExisted `shouldBe` True

        content <- LBS.readFile logFile
        -- The log file has one JSON per line
        case LBS.split 10 content of -- 10 is '\n'
          (firstLine:_) -> do
            let decoded = decode firstLine :: Maybe DecisionTrace
            decoded `shouldBe` Just trace
          [] -> fail "Log file was empty"

        -- Cleanup after test
        existsAfter <- doesFileExist logFile
        when existsAfter $ removeFile logFile
