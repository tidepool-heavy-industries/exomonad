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
              messages = []
            ,
              textInput = Nothing
            ,
              photoUpload = Nothing
            ,
              choices = Nothing
            ,
              graphNode = "start"
            ,
              thinking = False
            ,
              stats = Nothing
            ,
              clocks = []
            ,
              dicePool = Nothing
            ,
              mood = Nothing
            ,
              charCreation = Nothing
            ,
              history = []
            }
      decode (encode state) `shouldBe` Just state

    it "UIState - Full" $ do
      let state = UIState
            {
              messages = [ChatMessage User "hello" "2023-01-01T00:00:00Z"]
            ,
              textInput = Just (TextInputConfig "type here...")
            ,
              photoUpload = Just (PhotoUploadConfig "upload a photo")
            ,
              choices = Just (ChoiceConfig "choose one" [ChoiceOption 0 "Option A" (Just "Desc") [] Nothing] False)
            ,
              graphNode = "node-1"
            ,
              thinking = True
            ,
              stats = Just (DMStats 2 1 5 0 ["Stressed"] OperatingFromStrength)
            ,
              clocks = [Clock "clk-1" "Threat" 8 4 True ClockThreat]
            ,
              dicePool = Just (DicePool [] Risky Standard "Context" True (Just "Bargain"))
            ,
              mood = Just (MoodScene (SvEncounter UrgencyMedium))
            ,
              charCreation = Nothing
            ,
              history = []
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
