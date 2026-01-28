{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoStarIsType #-}

-- | GitHub Server for the Role DSL.
--
-- Tools for GitHub issue operations.
module ExoMonad.Control.Role.Server.GitHub
  ( GitHubServer(..)
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import GHC.Generics (Generic)

import ExoMonad.Control.Role.Types (ToolField)
import ExoMonad.Control.Role.Tool.GitHub (GHIssueList, GHIssueShow)

-- | MCP Server for GitHub tools.
data GitHubServer mode (es :: [Type -> Type]) = GitHubServer
  { ghDescription :: Text
  , ghIssueList :: ToolField mode es GHIssueList
  , ghIssueShow :: ToolField mode es GHIssueShow
  }
  deriving Generic
