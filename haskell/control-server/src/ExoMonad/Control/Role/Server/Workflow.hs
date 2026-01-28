{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoStarIsType #-}

-- | Workflow Server for the Role DSL.
--
-- Tools for developer workflow operations.
module ExoMonad.Control.Role.Server.Workflow
  ( WorkflowServer(..)
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import GHC.Generics (Generic)

import ExoMonad.Control.Role.Types (ToolField)
import ExoMonad.Control.Role.Tool.FilePR (FilePR)

-- | MCP Server for workflow tools.
data WorkflowServer mode (es :: [Type -> Type]) = WorkflowServer
  { wfDescription :: Text
  , wfFilePR :: ToolField mode es FilePR
  }
  deriving Generic
