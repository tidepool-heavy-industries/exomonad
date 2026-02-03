{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module ExoMonad.Guest.Tools.Git
  ( GitBranch,
    GitStatus,
    GitLog,
    GitBranchArgs (..),
    GitStatusArgs (..),
    GitLogArgs (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import ExoMonad.Guest.Tool.Class (MCPTool (..), successResult)
import GHC.Generics (Generic)

-- GitBranch
data GitBranch

data GitBranchArgs = GitBranchArgs
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON GitBranchArgs where
  parseJSON = Aeson.withObject "GitBranchArgs" $ \_ -> pure GitBranchArgs

instance MCPTool GitBranch where
  type ToolArgs GitBranch = GitBranchArgs
  toolName = "git_branch"
  toolDescription = "Get current git branch"
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "properties" .= object []
      ]
  toolHandler _ = pure $ successResult $ Aeson.String "main" -- Stub

-- GitStatus
data GitStatus

data GitStatusArgs = GitStatusArgs
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON GitStatusArgs where
  parseJSON = Aeson.withObject "GitStatusArgs" $ \_ -> pure GitStatusArgs

instance MCPTool GitStatus where
  type ToolArgs GitStatus = GitStatusArgs
  toolName = "git_status"
  toolDescription = "Get git status"
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "properties" .= object []
      ]
  toolHandler _ = pure $ successResult $ Aeson.String "clean" -- Stub

-- GitLog
data GitLog

data GitLogArgs = GitLogArgs
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON GitLogArgs where
  parseJSON = Aeson.withObject "GitLogArgs" $ \_ -> pure GitLogArgs

instance MCPTool GitLog where
  type ToolArgs GitLog = GitLogArgs
  toolName = "git_log"
  toolDescription = "Get git log"
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "properties" .= object []
      ]
  toolHandler _ = pure $ successResult $ Aeson.String "commit 123" -- Stub