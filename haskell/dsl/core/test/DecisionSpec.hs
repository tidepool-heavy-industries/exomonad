module DecisionSpec (spec) where

import Test.Hspec
import Data.Aeson (encode, decode)
import Data.Time (UTCTime(..), fromGregorian)
import Tidepool.Effect.Decision

spec :: Spec
spec = do
  describe "Decision JSON roundtrip" $ do
    it "roundtrips SelectBead" $ do
      let d = SelectBead "tidepool-123"
      decode (encode d) `shouldBe` Just d

    it "roundtrips ProvideGuidance" $ do
      let d = ProvideGuidance "Do better"
      decode (encode d) `shouldBe` Just d

    it "roundtrips Abort" $ do
      let d = Abort
      decode (encode d) `shouldBe` Just d

    it "roundtrips Continue" $ do
      let d = Continue
      decode (encode d) `shouldBe` Just d

  describe "DecisionContext JSON roundtrip" $ do
    it "roundtrips simple context" $ do
      let c = DecisionContext "What next?" []
      decode (encode c) `shouldBe` Just c

  describe "DecisionTrace JSON roundtrip" $ do
    it "roundtrips full trace" $ do
      let t = DecisionTrace
            { dtContext = DecisionContext "What next?" [BeadSummary "bead-1" "Test Bead" 1]
            , dtDecision = Continue
            , dtTimestamp = UTCTime (fromGregorian 2026 1 19) 0
            }
      decode (encode t) `shouldBe` Just t
