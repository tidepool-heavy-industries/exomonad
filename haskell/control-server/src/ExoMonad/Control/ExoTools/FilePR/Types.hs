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
import Language.Haskell.TH (mkName)

-- | Arguments for file_pr tool.
-- Issue number and title are inferred from the branch - agent provides context.
data FilePRArgs = FilePRArgs
  { testing     :: Text        -- ^ Required: How was this tested?
  , compromises :: Maybe Text  -- ^ Optional: Tradeoffs or shortcuts taken
  }
  deriving stock (Show, Eq, Generic)

$(deriveMCPTypeWith defaultMCPOptions ''FilePRArgs
  [ mkName "testing"     ?? "How was this tested? What scenarios were verified?"
  , mkName "compromises" ?? "Any tradeoffs, shortcuts, or known limitations?"
  ])

-- | PR Info for file_pr result.
data PRInfo = PRInfo
  { number :: Int
  , url    :: Text
  , status :: Text
  , title  :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

-- | Result of file_pr tool.
-- Returns PR info for either an existing or newly created PR.
data FilePRResult = FilePRResult
  { pr :: Maybe PRInfo      -- ^ PR info (existing or newly created)
  , created :: Bool         -- ^ True if we created a new PR, False if existing found
  , error :: Maybe Text     -- ^ Error message if operation failed
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON FilePRResult where
  toJSON res = object
    [ "pr" .= res.pr
    , "created" .= res.created
    , "error" .= res.error
    ]
