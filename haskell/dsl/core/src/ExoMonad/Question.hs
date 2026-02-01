{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Question DSL for structured user interaction.
--
-- This module provides types for asking structured questions to users
-- via GUI backends. Originally from Tidying agent, now shared in core.
module ExoMonad.Question
  ( -- * Question Tree
    Question (..),
    Choice (..),
    ChoiceOption (..),

    -- * Item Disposition
    ItemDisposition (..),

    -- * Answers
    Answer (..),
  )
where

import Data.Aeson (FromJSON, ToJSON, Value)

-- | Where an item goes
data ItemDisposition
  = -- | Specific location
    PlaceAt Text
  | -- | Garbage
    Trash
  | -- | Give away
    Donate
  | -- | Recycling bin
    Recycle
  | -- | Put it back, come back later
    SkipForNow
  | -- | Agent needs more context
    NeedMoreInfo
  deriving (Show, Eq, Generic)

instance ToJSON ItemDisposition where
  toJSON (PlaceAt loc) = object ["placeAt" .= loc]
  toJSON Trash = String "trash"
  toJSON Donate = String "donate"
  toJSON Recycle = String "recycle"
  toJSON SkipForNow = String "skip"
  toJSON NeedMoreInfo = String "needMoreInfo"

instance FromJSON ItemDisposition where
  parseJSON (String "trash") = pure Trash
  parseJSON (String "donate") = pure Donate
  parseJSON (String "recycle") = pure Recycle
  parseJSON (String "skip") = pure SkipForNow
  parseJSON (String "needMoreInfo") = pure NeedMoreInfo
  parseJSON (Object o) = PlaceAt <$> o .: "placeAt"
  parseJSON _ = fail "Invalid ItemDisposition"

-- | A question the agent asks the user
data Question
  = ProposeDisposition
      { pdItem :: Text,
        pdChoices :: [Choice],
        pdFallback :: Maybe Text
      }
  | Confirm
      { cfPrompt :: Text,
        cfDefault :: Bool
      }
  | Choose
      { chPrompt :: Text,
        chId :: Text,
        chChoices :: [ChoiceOption]
      }
  | FreeText
      { ftPrompt :: Text,
        ftPlaceholder :: Maybe Text
      }
  deriving (Show, Eq, Generic)

-- | A choice for ProposeDisposition
data Choice = Choice
  { choiceLabel :: Text,
    choiceValue :: ItemDisposition
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | A choice option for Choose
data ChoiceOption = ChoiceOption
  { optionLabel :: Text,
    optionValue :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance ToJSON Question where
  toJSON (ProposeDisposition item choices fallback) =
    object
      [ "proposeDisposition"
          .= object
            [ "item" .= item,
              "choices" .= choices,
              "fallback" .= fallback
            ]
      ]
  toJSON (Confirm prompt def) =
    object
      [ "confirm"
          .= object
            [ "prompt" .= prompt,
              "default" .= def
            ]
      ]
  toJSON (Choose prompt qid choices) =
    object
      [ "choose"
          .= object
            [ "prompt" .= prompt,
              "id" .= qid,
              "choices" .= choices
            ]
      ]
  toJSON (FreeText prompt placeholder) =
    object
      [ "freeText"
          .= object
            [ "prompt" .= prompt,
              "placeholder" .= placeholder
            ]
      ]

instance FromJSON Question where
  parseJSON = withObject "Question" $ \o ->
    parseProposeDisposition o
      <|> parseConfirm o
      <|> parseChoose o
      <|> parseFreeText o
    where
      parseProposeDisposition o = do
        pd <- o .: "proposeDisposition"
        ProposeDisposition
          <$> pd .: "item"
          <*> pd .: "choices"
          <*> pd .:? "fallback"

      parseConfirm o = do
        cf <- o .: "confirm"
        Confirm
          <$> cf .: "prompt"
          <*> cf .:? "default" .!= True

      parseChoose o = do
        ch <- o .: "choose"
        Choose
          <$> ch .: "prompt"
          <*> ch .: "id"
          <*> ch .: "choices"

      parseFreeText o = do
        ft <- o .: "freeText"
        FreeText
          <$> ft .: "prompt"
          <*> ft .:? "placeholder"

-- | User's answer to a question
data Answer
  = DispositionAnswer ItemDisposition
  | ConfirmAnswer Bool
  | ChoiceAnswer Text
  | TextAnswer Text
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
