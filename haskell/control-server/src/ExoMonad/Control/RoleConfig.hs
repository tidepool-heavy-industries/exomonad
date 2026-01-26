module ExoMonad.Control.RoleConfig
  ( Role(..)
  , roleFromText
  , roleTools
  , isToolAllowed
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as Set
import ExoMonad.Role (Role(..))

roleFromText :: Text -> Maybe Role
roleFromText "pm" = Just PM
roleFromText "tl" = Just TL
roleFromText "dev" = Just Dev
roleFromText _ = Nothing

roleTools :: Role -> Maybe (Set.Set Text)
roleTools TL = Just $ Set.fromList
  -- Orchestration: spawn and monitor agents
  [ "spawn_agents"
  , "exo_status"      -- Check agent status
  -- Messaging with agents
  , "send_message", "check_inbox", "read_message", "mark_read"
  -- Interactive (TUI-backed)
  , "popup"
  -- GitHub tools
  , "gh_issue_list", "gh_issue_show"
  , "gh_issue_create", "gh_issue_update", "gh_issue_close", "gh_issue_reopen"
  ]

roleTools Dev = Just $ Set.fromList
  -- Workflow execution
  [ "file_pr"         -- File pull requests
  -- Code intelligence (LSP-backed)
  , "find_callers", "show_fields", "show_constructors", "teach-graph"
  -- Interactive (TUI-backed)
  , "popup"
  -- GitHub tools
  , "gh_issue_list", "gh_issue_show"
  , "gh_issue_update"
  ]

roleTools PM = Just $ Set.fromList
  -- Planning tools
  [ "pm_status", "pm_propose"
  , "pm_approve_expansion", "pm_prioritize"
  -- Messaging
  , "send_message", "check_inbox", "read_message", "mark_read"
  , "exo_status"  -- Can view but not spawn
  -- Interactive (TUI-backed)
  , "popup"
  -- GitHub tools
  , "gh_issue_list", "gh_issue_show"
  , "gh_issue_create", "gh_issue_update", "gh_issue_close", "gh_issue_reopen"
  ]

isToolAllowed :: Role -> Text -> Bool
isToolAllowed role toolName = case roleTools role of
  Nothing -> True
  Just allowed -> Set.member toolName allowed
