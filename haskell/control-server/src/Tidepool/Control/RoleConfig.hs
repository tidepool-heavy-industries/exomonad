module Tidepool.Control.RoleConfig
  ( Role(..)
  , roleFromText
  , roleTools
  , isToolAllowed
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as Set
import Tidepool.Role (Role(..))

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
  , "exo_complete"    -- Mark agent work complete
  -- Messaging with agents
  , "send_message", "check_inbox", "read_message", "mark_read"
  ]

roleTools Dev = Just $ Set.fromList
  -- Workflow execution
  [ "file_pr"         -- File pull requests
  -- Code intelligence (LSP-backed)
  , "find_callers", "show_fields", "show_constructors", "teach-graph"
  -- Interactive (TUI-backed)
  , "confirm_action", "select_option", "request_guidance"
  ]

roleTools PM = Just $ Set.fromList
  -- Planning tools
  [ "pm_status", "pm_review_dag", "pm_propose"
  , "pm_approve_expansion", "pm_prioritize"
  -- Messaging
  , "send_message", "check_inbox", "read_message", "mark_read"
  , "exo_status"  -- Can view but not spawn
  ]

isToolAllowed :: Role -> Text -> Bool
isToolAllowed role toolName = case roleTools role of
  Nothing -> True
  Just allowed -> Set.member toolName allowed