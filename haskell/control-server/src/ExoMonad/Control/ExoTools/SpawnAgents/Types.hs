{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module ExoMonad.Control.ExoTools.SpawnAgents.Types
  ( SpawnAgentsArgs (..),
    SpawnAgentsResult (..),
    CleanupAgentsArgs (..),
    CleanupAgentsResult (..),
    InitialPromptContext (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Text (Text)
import ExoMonad.Schema (defaultMCPOptions, deriveMCPTypeWith, (??))
import GHC.Generics (Generic)
import Language.Haskell.TH (mkName)
import Text.Ginger.GVal (ToGVal (..))

-- | Context for rendering the initial prompt.
data InitialPromptContext = InitialPromptContext
  { issue_number :: Text,
    issue_title :: Text,
    issue_body :: Text,
    branch_name :: Text,
    worktree_path :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

instance ToGVal m InitialPromptContext

-- | Arguments for spawn_agents tool.
data SpawnAgentsArgs = SpawnAgentsArgs
  { -- | List of issue numbers (e.g. "123", "456").
    issueNumbers :: [Text],
    -- | Backend to use: "claude" or "gemini" (defaults to "claude").
    backend :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

$( deriveMCPTypeWith
     defaultMCPOptions
     ''SpawnAgentsArgs
     [ mkName "issueNumbers" ?? "List of issue numbers to spawn worktrees for.",
       mkName "backend" ?? "Backend to use: 'claude' or 'gemini' (defaults to 'claude')."
     ]
 )

-- | Result of spawn_agents tool.
data SpawnAgentsResult = SpawnAgentsResult
  { -- | Successfully created worktrees: (issueNum, path)
    worktrees :: [(Text, FilePath)],
    -- | Successfully launched tabs: (issueNum, tabId)
    tabs :: [(Text, Text)],
    -- | Failed operations: (issueNum, reason)
    failed :: [(Text, Text)]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON SpawnAgentsResult where
  parseJSON = withObject "SpawnAgentsResult" $ \v ->
    SpawnAgentsResult
      <$> v .: "worktrees"
      <*> v .: "tabs"
      <*> v .: "failed"

instance ToJSON SpawnAgentsResult where
  toJSON res =
    object
      [ "worktrees" .= res.worktrees,
        "tabs" .= res.tabs,
        "failed" .= res.failed
      ]

-- | Arguments for cleanup_agents tool.
data CleanupAgentsArgs = CleanupAgentsArgs
  { -- | List of issue numbers to clean up.
    issueNumbers :: [Text],
    -- | If true, skip confirmation (not used yet in server logic).
    force :: Maybe Bool
  }
  deriving stock (Show, Eq, Generic)

$( deriveMCPTypeWith
     defaultMCPOptions
     ''CleanupAgentsArgs
     [ mkName "issueNumbers" ?? "List of issue numbers to clean up worktrees and containers for.",
       mkName "force" ?? "If true, skip confirmation (defaults to false)."
     ]
 )

-- | Result of cleanup_agents tool.
data CleanupAgentsResult = CleanupAgentsResult
  { -- | Successfully cleaned issue IDs.
    cleaned :: [Text],
    -- | Failed cleanups: (issueNum, reason)
    failed :: [(Text, Text)]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON CleanupAgentsResult where
  parseJSON = withObject "CleanupAgentsResult" $ \v ->
    CleanupAgentsResult
      <$> v .: "cleaned"
      <*> v .: "failed"

instance ToJSON CleanupAgentsResult where
  toJSON res =
    object
      [ "cleaned" .= res.cleaned,
        "failed" .= res.failed
      ]
