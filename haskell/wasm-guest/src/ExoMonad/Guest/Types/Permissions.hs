{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module ExoMonad.Guest.Types.Permissions
  ( ToolPattern(..)
  , ClaudePermissions(..)
  , renderToolPattern
  , defaultSandboxPermissions
  ) where

import Data.Text (Text)
import Data.Aeson (FromJSON(..), withObject, (.:?))
import ExoMonad.Guest.Tool.Schema (JsonSchema(..))
import GHC.Generics (Generic)

-- | Typed tool patterns for Claude Code permission rules.
-- Renders to strings like "Read(../**)", "Edit(./src/**)"
data ToolPattern
  = ReadPat Text      -- ^ Read(pattern)
  | EditPat Text      -- ^ Edit(pattern)
  | BashPat Text      -- ^ Bash(pattern)
  | CustomPat Text    -- ^ Arbitrary tool pattern string
  deriving (Show, Eq, Generic)

instance JsonSchema ToolPattern where
  toSchema = toSchema @Text

-- | Claude Code permission rules. Only available for Claude agents (spawn_subtree),
-- not Gemini agents (spawn_leaf_subtree, spawn_workers).
data ClaudePermissions = ClaudePermissions
  { cpAllow :: [ToolPattern]
  , cpDeny  :: [ToolPattern]
  } deriving (Show, Eq, Generic)

instance JsonSchema ClaudePermissions

instance FromJSON ClaudePermissions where
  parseJSON = withObject "ClaudePermissions" $ \v -> do
    allow <- v .:? "allow"
    deny  <- v .:? "deny"
    pure $ ClaudePermissions
      { cpAllow = map CustomPat (maybe [] id allow)
      , cpDeny  = map CustomPat (maybe [] id deny)
      }

-- | Render a ToolPattern to the string format Claude Code expects.
renderToolPattern :: ToolPattern -> Text
renderToolPattern (ReadPat p)   = "Read(" <> p <> ")"
renderToolPattern (EditPat p)   = "Edit(" <> p <> ")"
renderToolPattern (BashPat p)   = "Bash(" <> p <> ")"
renderToolPattern (CustomPat p) = p

-- | Default permissions for sandboxed agents: deny all access outside working dir.
defaultSandboxPermissions :: ClaudePermissions
defaultSandboxPermissions = ClaudePermissions
  { cpAllow = []
  , cpDeny  = [ReadPat "../**", EditPat "../**"]
  }
