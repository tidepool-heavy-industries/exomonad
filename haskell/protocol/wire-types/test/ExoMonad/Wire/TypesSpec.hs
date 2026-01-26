{-# LANGUAGE OverloadedStrings #-}
module ExoMonad.Wire.TypesSpec (spec) where

import Test.Hspec
import Data.Aeson (encode, decode, Result(..), fromJSON)
import ExoMonad.Wire.Types
import Data.Text (Text)

spec :: Spec
spec = do
  describe "JSON Roundtrip" $ do
    it "MessageRole" $ do
      let roles = [User, Assistant, System]
      mapM_ (\r -> decode (encode r) `shouldBe` Just r) roles

    it "UserAction - TextAction" $ do
      let action = TextAction "hello world"
      decode (encode action) `shouldBe` Just action

    it "UserAction - ChoiceAction" $ do
      let action = ChoiceAction 42
      decode (encode action) `shouldBe` Just action

    it "UserAction - MultiChoiceAction" $ do
      let action = MultiChoiceAction [1, 2, 3]
      decode (encode action) `shouldBe` Just action

    it "UserAction - PhotoAction" $ do
      let action = PhotoAction "base64data" "image/png"
      decode (encode action) `shouldBe` Just action

    it "UIState - Minimal" $ do
      let state = UIState
            {
              usMessages = []
            ,
              usTextInput = Nothing
            ,
              usPhotoUpload = Nothing
            ,
              usChoices = Nothing
            ,
              usGraphNode = "start"
            ,
              usThinking = False
            ,
              usDMStats = Nothing
            ,
              usDMClocks = []
            ,
              usDMDicePool = Nothing
            ,
              usDMMood = Nothing
            ,
              usDMCharCreation = Nothing
            ,
              usDMHistory = []
            }
      decode (encode state) `shouldBe` Just state

    it "UIState - Full" $ do
      let state = UIState
            {
              usMessages = [ChatMessage User "hello" "2023-01-01T00:00:00Z"]
            ,
              usTextInput = Just (TextInputConfig "type here...")
            ,
              usPhotoUpload = Just (PhotoUploadConfig "upload a photo")
            ,
              usChoices = Just (ChoiceConfig "choose one" [ChoiceOption 0 "Option A" (Just "Desc") [] Nothing] False)
            ,
              usGraphNode = "node-1"
            ,
              usThinking = True
            ,
              usDMStats = Just (DMStats 2 1 5 0 ["Stressed"] OperatingFromStrength)
            ,
              usDMClocks = [Clock "clk-1" "Threat" 8 4 True ClockThreat]
            ,
              usDMDicePool = Just (DicePool [] Risky Standard "Context" True (Just "Bargain"))
            ,
              usDMMood = Just (MoodScene (SvEncounter UrgencyMedium))
            ,
              usDMCharCreation = Nothing
            ,
              usDMHistory = []
            }
      decode (encode state) `shouldBe` Just state

  describe "DM Mood Serialization" $ do
    it "MoodScene" $ do
      let mood = MoodScene (SvEncounter UrgencyHigh)
      decode (encode mood) `shouldBe` Just mood

    it "MoodAction" $ do
      let mood = MoodAction (AvRisky "Threat" "Opp") (Just DomSocial)
      decode (encode mood) `shouldBe` Just mood

    it "MoodAftermath" $ do
      let mood = MoodAftermath AmClean
      decode (encode mood) `shouldBe` Just mood

    it "MoodDowntime" $ do
      let mood = MoodDowntime (DtRecovery ["Act1"])
      decode (encode mood) `shouldBe` Just mood

    it "MoodBargain" $ do
      let mood = MoodBargain [BargainOption "Label" (CostStress 2) "Desc"]
      decode (encode mood) `shouldBe` Just mood
