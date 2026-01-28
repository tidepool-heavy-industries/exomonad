{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module ExoMonad.Control.ExoTools.SpawnAgents.Types
  ( SpawnAgentsArgs(..)
  , SpawnAgentsResult(..)
  , CleanupAgentsArgs(..)
  , CleanupAgentsResult(..)
  , InitialPromptContext(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.=), object, withObject)
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Ginger.GVal (ToGVal(..))
import ExoMonad.Schema (deriveMCPTypeWith, defaultMCPOptions, (??), MCPOptions(..), HasJSONSchema(..), arraySchema, emptySchema, SchemaType(..), describeField)

-- | Context for rendering the initial prompt.
data InitialPromptContext = InitialPromptContext
  { issue_number :: Text
  , issue_title :: Text
  , issue_body :: Text
  , branch_name :: Text
  , worktree_path :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

instance ToGVal m InitialPromptContext

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

-- | Arguments for cleanup_agents tool.
data CleanupAgentsArgs = CleanupAgentsArgs
  { caaIssueNumbers :: [Text]  -- ^ List of issue numbers to clean up.
  , caaForce :: Maybe Bool     -- ^ If true, skip confirmation (not used yet in server logic).
  }
  deriving stock (Show, Eq, Generic)

$(deriveMCPTypeWith defaultMCPOptions { fieldPrefix = "caa" } ''CleanupAgentsArgs
  [ 'caaIssueNumbers ?? "List of issue numbers to clean up worktrees and containers for."
  , 'caaForce        ?? "If true, skip confirmation (defaults to false)."
  ])

-- | Result of cleanup_agents tool.
data CleanupAgentsResult = CleanupAgentsResult
  {
    carCleaned    :: [Text]      -- ^ Successfully cleaned issue IDs.
  , carFailed     :: [(Text, Text)] -- ^ Failed cleanups: (issueNum, reason)
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON CleanupAgentsResult where
  parseJSON = withObject "CleanupAgentsResult" $ \v ->
    CleanupAgentsResult
      <$> v .: "cleaned"
      <*> v .: "failed"

instance ToJSON CleanupAgentsResult where
  toJSON res = object
    [
      "cleaned" .= carCleaned res
    , "failed"  .= carFailed res
    ]