{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module ExoMonad.Control.ExoTools.FilePR.Types
  ( FilePRArgs(..)
  , FilePRResult(..)
  , PRInfo(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.:?), (.=), object, withObject)
import Data.Text (Text)
import GHC.Generics (Generic)
import ExoMonad.Schema (deriveMCPTypeWith, defaultMCPOptions, (??), MCPOptions(..), HasJSONSchema(..))

-- | Arguments for file_pr tool.
-- Issue number and title are inferred from the branch - agent provides context.
data FilePRArgs = FilePRArgs
  { fpaTesting     :: Text        -- ^ Required: How was this tested?
  , fpaCompromises :: Maybe Text  -- ^ Optional: Tradeoffs or shortcuts taken
  }
  deriving stock (Show, Eq, Generic)

$(deriveMCPTypeWith defaultMCPOptions { fieldPrefix = "fpa" } ''FilePRArgs
  [ 'fpaTesting     ?? "How was this tested? What scenarios were verified?"
  , 'fpaCompromises ?? "Any tradeoffs, shortcuts, or known limitations?"
  ])

-- | PR Info for file_pr result.
data PRInfo = PRInfo
  { priNumber :: Int
  , priUrl    :: Text
  , priStatus :: Text
  , priTitle  :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Result of file_pr tool.
-- Returns PR info for either an existing or newly created PR.
data FilePRResult = FilePRResult
  { fprPr :: Maybe PRInfo      -- ^ PR info (existing or newly created)
  , fprCreated :: Bool         -- ^ True if we created a new PR, False if existing found
  , fprError :: Maybe Text     -- ^ Error message if operation failed
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON FilePRResult where
  toJSON res = object
    [ "pr" .= fprPr res
    , "created" .= fprCreated res
    , "error" .= fprError res
    ]

instance FromJSON FilePRResult where
  parseJSON = withObject "FilePRResult" $ \v ->
    FilePRResult
      <$> v .:? "pr"
      <*> v .: "created"
      <*> v .:? "error"
