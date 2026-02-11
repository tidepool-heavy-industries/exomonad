{-# LANGUAGE OverloadedStrings #-}

-- | Typed permission sets for the three-tier permission cascade.
--
-- The cascade works as follows:
--
-- 1. Check the agent's own permission set (auto-approve if within scope)
-- 2. Escalate to TL (auto-approve within TL scope)
-- 3. Escalate to human (popup in Zellij)
--
-- Permission types are ADTs — illegal states are unrepresentable.
-- The actual checking logic starts as a passthrough and is built
-- incrementally as the HTTP-native role system matures.
module ExoMonad.Permissions
  ( -- * Permission check result
    PermissionCheck (..),

    -- * Role-specific permission types
    DevPermission (..),
    TLPermission (..),

    -- * Domain types
    BranchName (..),

    -- * Checking
    checkAgentPermissions,
    checkDevPermission,
    checkTLPermission,
  )
where

import Data.Aeson (Value)
import Data.Text (Text)

-- | Newtype for git branch names.
newtype BranchName = BranchName {unBranchName :: Text}
  deriving (Show, Eq, Ord)

-- | Result of a permission check.
--
-- @Allowed@: The action is within the agent's own scope — proceed.
-- @Escalate@: The action is outside the agent's scope — ask the TL (or human).
-- @Denied@: The action is explicitly forbidden with a reason.
data PermissionCheck
  = Allowed
  | Escalate
  | Denied Text
  deriving (Show, Eq)

-- | Permissions available to dev agents.
--
-- Dev agents can: commit, push to feature branches, read files, run tests.
-- Dev agents cannot: push to main, delete branches, modify CI, spawn agents.
data DevPermission
  = -- | Commit changes in the worktree
    DevCommitAllowed
  | -- | Push to a feature branch (never main)
    DevPushFeatureBranch BranchName
  | -- | Read a file from the filesystem
    DevReadFile FilePath
  | -- | Run test commands
    DevRunTests
  | -- | File a PR from the current branch
    DevFilePR
  | -- | Send a message to the TL
    DevSendMessage
  deriving (Show, Eq)

-- | Permissions available to TL agents.
--
-- TL agents can: everything dev can, plus branch management, agent spawning,
-- issue/PR management. TL cannot: push to main (requires human approval).
data TLPermission
  = -- | Push to any branch except main/master
    TLPushAnyBranch BranchName
  | -- | Close a pull request
    TLClosePR
  | -- | Close an issue
    TLCloseIssue
  | -- | Delete a branch that has been merged
    TLDeleteMergedBranch BranchName
  | -- | Spawn sub-agents (spawn_subtree, spawn_worker)
    TLSpawnAgents
  | -- | Show popup UI
    TLShowPopup
  deriving (Show, Eq)

-- | Check whether a tool call is within a role's permission set.
--
-- This is a passthrough for the initial migration — always returns 'Allowed'.
-- As the permission cascade matures, this will dispatch to role-specific
-- permission checkers based on the role name.
checkAgentPermissions :: Text -> Text -> Value -> PermissionCheck
checkAgentPermissions _role _toolName _toolArgs = Allowed

-- | Check a specific dev permission.
--
-- Stub: always returns 'Allowed'. Will be wired into the cascade
-- once tool-to-permission mapping is defined.
checkDevPermission :: DevPermission -> PermissionCheck
checkDevPermission _ = Allowed

-- | Check a specific TL permission.
--
-- Stub: always returns 'Allowed'. Will be wired into the cascade
-- once tool-to-permission mapping is defined.
checkTLPermission :: TLPermission -> PermissionCheck
checkTLPermission _ = Allowed
