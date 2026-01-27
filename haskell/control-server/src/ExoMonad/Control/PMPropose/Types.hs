{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module ExoMonad.Control.PMPropose.Types
  ( PMProposeArgs(..)
  , PMProposeResult(..)
  ) where

import Data.Aeson (ToJSON(..), object, (.=))
import Data.Text (Text)
import GHC.Generics (Generic)
import ExoMonad.Schema (deriveMCPTypeWith, defaultMCPOptions, (??), MCPOptions(..), HasJSONSchema(..), arraySchema, emptySchema, SchemaType(..), describeField)

-- | Arguments for pm_propose tool.
data PMProposeArgs = PMProposeArgs
  { ppaTitle :: Text
  , ppaIntent :: Text
  , ppaSuggestedPriority :: Maybe Int
  , ppaSuggestedLabels :: Maybe [Text]
  , ppaContext :: Maybe Text
  , ppaScopeHint :: Maybe Text
  , ppaRepo :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

$(deriveMCPTypeWith defaultMCPOptions { fieldPrefix = "ppa" } ''PMProposeArgs
  [ 'ppaTitle             ?? "Title of the proposed issue"
  , 'ppaIntent            ?? "The core intent or goal of this work"
  , 'ppaSuggestedPriority ?? "Suggested priority (0-4, default 2)"
  , 'ppaSuggestedLabels   ?? "List of suggested labels"
  , 'ppaContext           ?? "Additional context or background information"
  , 'ppaScopeHint         ?? "Hint about the scope/size of the work"
  , 'ppaRepo              ?? "GitHub repo"
  ])

-- | Result of pm_propose tool.
data PMProposeResult = PMProposeResult
  { pprIssueNum :: Int
  , pprStatus :: Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON PMProposeResult where
  toJSON res = object
    ["issue_num" .= pprIssueNum res
    , "status" .= pprStatus res
    ]
