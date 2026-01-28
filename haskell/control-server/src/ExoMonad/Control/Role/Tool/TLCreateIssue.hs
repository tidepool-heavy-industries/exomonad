{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | TLCreateIssue tool definition.
module ExoMonad.Control.Role.Tool.TLCreateIssue
  ( TLCreateIssue(..)
  , TLCreateIssueArgs(..)
  , TLCreateIssueResult(..)
  ) where

import ExoMonad.Control.Role.Types (ToolSpec(..))
import ExoMonad.Control.TLTools.Types
  ( TLCreateIssueArgs(..)
  , TLCreateIssueResult(..)
  )

-- | Marker type for the TLCreateIssue tool.
data TLCreateIssue = TLCreateIssue

instance ToolSpec TLCreateIssue where
  type Args TLCreateIssue = TLCreateIssueArgs
  type Result TLCreateIssue = TLCreateIssueResult
  toolName = "tl_create_issue"
  toolDescription = "Create a new issue with domain-specific structured fields (Category, Priority, etc.). Enforces best practices for issue creation."
