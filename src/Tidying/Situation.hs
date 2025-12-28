{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Tidying.Situation
  ( -- * Situation (ORIENT output)
    Situation(..)
  , ItemClass(..)

    -- * Parsing
  , parseSituation

    -- * Queries
  , isStuck
  , isAnxious
  , isFlagging
  , extractAnxietyTrigger
  , extractStuckItem
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

-- | How an item should be classified
data ItemClass
  = Trash                  -- ^ Obviously garbage
  | Belongs Text           -- ^ Belongs somewhere: "desk", "shelf", "drawer"
  | Unsure                 -- ^ Not sure yet, goes in unsure pile
  | NeedsDecisionSupport   -- ^ User hesitating, needs reframe
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Situation classification (output of ORIENT)
data Situation
  -- Surveying phase
  = NeedFunction             -- ^ No function established
  | NeedAnchors              -- ^ Have function, need anchors
  | OverwhelmedNeedMomentum  -- ^ User overwhelmed, skip questions, start moving

  -- Sorting phase
  | ItemDescribed Text ItemClass  -- ^ User described an item
  | ActionDone                    -- ^ User completed an instruction
  | UnsureGrowing                 -- ^ Unsure pile too big, needs split
  | MainPileDone                  -- ^ Current pile finished

  -- Energy/stuck signals (any phase)
  | Stuck (Maybe Text)       -- ^ User stuck, maybe on specific item
  | Anxious Text             -- ^ User anxious about something ("boxes")
  | Flagging                 -- ^ Energy dropping
  | WantsToContinue          -- ^ User wants to keep going
  | WantsToStop              -- ^ User wants to pause/stop

  -- Completion
  | AllDone                  -- ^ Session complete
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Parse situation from LLM response
-- Format examples:
--   "NeedFunction"
--   "ItemDescribed \"usb cable\" Belongs \"drawer\""
--   "Stuck \"sketchbook\""
--   "Anxious \"boxes\""
parseSituation :: Text -> Maybe Situation
parseSituation t = case T.words (T.strip t) of
  ["NeedFunction"] -> Just NeedFunction
  ["NeedAnchors"] -> Just NeedAnchors
  ["OverwhelmedNeedMomentum"] -> Just OverwhelmedNeedMomentum
  ["ActionDone"] -> Just ActionDone
  ["UnsureGrowing"] -> Just UnsureGrowing
  ["MainPileDone"] -> Just MainPileDone
  ["Flagging"] -> Just Flagging
  ["WantsToContinue"] -> Just WantsToContinue
  ["WantsToStop"] -> Just WantsToStop
  ["AllDone"] -> Just AllDone

  ("Stuck" : rest) -> Just $ Stuck (parseQuoted $ T.unwords rest)

  ("Anxious" : rest) -> case parseQuoted (T.unwords rest) of
    Just trigger -> Just $ Anxious trigger
    Nothing -> Nothing

  ("ItemDescribed" : rest) -> parseItemDescribed (T.unwords rest)

  _ -> Nothing

parseItemDescribed :: Text -> Maybe Situation
parseItemDescribed t = do
  -- Format: "item description" ClassType "optional location"
  (item, rest) <- parseQuotedAndRest t
  cls <- parseItemClass rest
  Just $ ItemDescribed item cls

parseItemClass :: Text -> Maybe ItemClass
parseItemClass t = case T.words (T.strip t) of
  ["Trash"] -> Just Trash
  ["Unsure"] -> Just Unsure
  ["NeedsDecisionSupport"] -> Just NeedsDecisionSupport
  ("Belongs" : rest) -> Belongs <$> parseQuoted (T.unwords rest)
  _ -> Nothing

parseQuoted :: Text -> Maybe Text
parseQuoted t =
  let stripped = T.strip t
  in if T.head stripped == '"' && T.last stripped == '"'
     then Just $ T.drop 1 $ T.dropEnd 1 stripped
     else Nothing

parseQuotedAndRest :: Text -> Maybe (Text, Text)
parseQuotedAndRest t =
  let stripped = T.strip t
  in if T.head stripped == '"'
     then case T.breakOn "\"" (T.drop 1 stripped) of
       (content, rest) | not (T.null rest) ->
         Just (content, T.drop 1 rest)  -- drop closing quote
       _ -> Nothing
     else Nothing

-- Queries

isStuck :: Situation -> Bool
isStuck (Stuck _) = True
isStuck _ = False

isAnxious :: Situation -> Bool
isAnxious (Anxious _) = True
isAnxious _ = False

isFlagging :: Situation -> Bool
isFlagging Flagging = True
isFlagging _ = False

extractAnxietyTrigger :: Situation -> Maybe Text
extractAnxietyTrigger (Anxious t) = Just t
extractAnxietyTrigger _ = Nothing

extractStuckItem :: Situation -> Maybe Text
extractStuckItem (Stuck item) = item
extractStuckItem _ = Nothing
