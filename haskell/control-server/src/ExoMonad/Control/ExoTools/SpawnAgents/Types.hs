{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module ExoMonad.Control.ExoTools.SpawnAgents.Types
  ( SpawnAgentsArgs(..)
  , SpawnAgentsResult(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.=), object, withObject)
import Data.Text (Text)
import GHC.Generics (Generic)
import ExoMonad.Schema (deriveMCPTypeWith, defaultMCPOptions, (??), MCPOptions(..), HasJSONSchema(..), arraySchema, emptySchema, SchemaType(..), describeField)

-- | Arguments for spawn_agents tool.
data SpawnAgentsArgs = SpawnAgentsArgs
  { saaIssueNumbers :: [Text]  -- ^ List of issue numbers (e.g. "123", "456").
  , saaBackend :: Maybe Text  -- ^ Backend to use: "claude" or "gemini" (defaults to "claude").
  }
  deriving stock (Show, Eq, Generic)

$(deriveMCPTypeWith defaultMCPOptions { fieldPrefix = "saa" } ''SpawnAgentsArgs
  [ 'saaIssueNumbers ?? "List of issue numbers to spawn worktrees for."
  , 'saaBackend      ?? "Backend to use: 'claude' or 'gemini' (defaults to 'claude')."
  ])

-- | Result of spawn_agents tool.
data SpawnAgentsResult = SpawnAgentsResult
  {
    sarWorktrees :: [(Text, FilePath)]  -- ^ Successfully created worktrees: (issueNum, path)
  , sarTabs      :: [(Text, Text)]      -- ^ Successfully launched tabs: (issueNum, tabId)
  , sarFailed    :: [(Text, Text)]      -- ^ Failed operations: (issueNum, reason)
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON SpawnAgentsResult where
  parseJSON = withObject "SpawnAgentsResult" $ \v ->
    SpawnAgentsResult
      <$> v .: "worktrees"
      <*> v .: "tabs"
      <*> v .: "failed"

instance ToJSON SpawnAgentsResult where
  toJSON res = object
    [
      "worktrees" .= sarWorktrees res
    , "tabs"      .= sarTabs res
    , "failed"    .= sarFailed res
    ]
