{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module ExoMonad.Control.ExoTools.FilePR.Types
  ( FilePRArgs (..),
    FilePRResult (..),
    PRInfo (..),
  )
where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text (Text)
import ExoMonad.Schema (defaultMCPOptions, deriveMCPTypeWith, (??))
import GHC.Generics (Generic)
import Language.Haskell.TH (mkName)

-- | Arguments for file_pr tool.
-- Issue number and title are inferred from the branch - agent provides context.
data FilePRArgs = FilePRArgs
  { -- | Required: How was this tested?
    testing :: Text,
    -- | Optional: Tradeoffs or shortcuts taken
    compromises :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

$( deriveMCPTypeWith
     defaultMCPOptions
     ''FilePRArgs
     [ mkName "testing" ?? "How was this tested? What scenarios were verified?",
       mkName "compromises" ?? "Any tradeoffs, shortcuts, or known limitations?"
     ]
 )

-- | PR Info for file_pr result.
data PRInfo = PRInfo
  { number :: Int,
    url :: Text,
    status :: Text,
    title :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

-- | Result of file_pr tool.
-- Returns PR info for either an existing or newly created PR.
data FilePRResult = FilePRResult
  { -- | PR info (existing or newly created)
    pr :: Maybe PRInfo,
    -- | True if we created a new PR, False if existing found
    created :: Bool,
    -- | Error message if operation failed
    error :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON FilePRResult where
  toJSON res =
    object
      [ "pr" .= res.pr,
        "created" .= res.created,
        "error" .= res.error
      ]
