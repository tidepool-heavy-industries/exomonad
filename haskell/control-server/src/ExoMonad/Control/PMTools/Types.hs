{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module ExoMonad.Control.PMTools.Types
  ( -- * Epic Planning
    PMEpicCreateArgs(..)
  , PMEpicCreateResult(..)
  , PMEpicListArgs(..)
  , PMEpicListResult(..)
  , EpicSummary(..)
  , PMEpicUpdateArgs(..)
  , PMEpicUpdateResult(..)

    -- * Strategy (TUI)
  , PMPitchArgs(..)
  , PMPitchResult(..)
  , PMInterviewArgs(..)
  , PMInterviewResult(..)
  ) where

import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Aeson (ToJSON, FromJSON)
import ExoMonad.Schema (HasJSONSchema(..), deriveHasJSONSchema)

-- ════════════════════════════════════════════════════════════════════════════
-- EPIC PLANNING
-- ════════════════════════════════════════════════════════════════════════════

data PMEpicCreateArgs = PMEpicCreateArgs
  { title :: Text
  , description :: Text
  , milestone :: Maybe Text
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

$(deriveHasJSONSchema ''PMEpicCreateArgs)

data PMEpicCreateResult = PMEpicCreateResult
  { epicId :: Text
  , url :: Text
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data PMEpicListArgs = PMEpicListArgs
  { state :: Maybe Text -- ^ "open" or "closed"
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

$(deriveHasJSONSchema ''PMEpicListArgs)

data PMEpicListResult = PMEpicListResult
  { epics :: [EpicSummary]
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data EpicSummary = EpicSummary
  { id :: Text
  , title :: Text
  , state :: Text
  , progress :: Int -- ^ Percentage
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data PMEpicUpdateArgs = PMEpicUpdateArgs
  { epicId :: Text
  , status :: Maybe Text
  , addIssues :: Maybe [Text]
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

$(deriveHasJSONSchema ''PMEpicUpdateArgs)

data PMEpicUpdateResult = PMEpicUpdateResult
  { success :: Bool
  , updatedEpic :: EpicSummary
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- ════════════════════════════════════════════════════════════════════════════
-- STRATEGY (TUI)
-- ════════════════════════════════════════════════════════════════════════════

data PMPitchArgs = PMPitchArgs
  { title :: Text
  , proposal :: Text
  , context :: Maybe Text
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

$(deriveHasJSONSchema ''PMPitchArgs)

data PMPitchResult = PMPitchResult
  { approved :: Bool
  , feedback :: Maybe Text
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data PMInterviewArgs = PMInterviewArgs
  { topic :: Text
  , questions :: [Text]
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

$(deriveHasJSONSchema ''PMInterviewArgs)

data PMInterviewResult = PMInterviewResult
  { answers :: [Text] -- ^ Answers corresponding to questions
  , summary :: Text
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)