module Tidepool.Control.RoleConfig
  ( Role(..)
  , roleFromText
  , roleTools
  , isToolAllowed
  ) where

import Data.Text (Text)
import qualified Data.Set as Set

data Role = PM | TL | Dev
  deriving (Show, Eq, Enum, Bounded)

roleFromText :: Text -> Maybe Role
roleFromText "pm" = Just PM
roleFromText "tl" = Just TL
roleFromText "dev" = Just Dev
roleFromText _ = Nothing

roleTools :: Role -> Maybe (Set.Set Text)
roleTools Dev = Nothing -- All tools allowed
roleTools PM = Just $ Set.fromList
  [ "pm_status", "pm_review_dag", "pm_propose", "pm_approve_expansion", "pm_prioritize"
  , "send_message", "check_inbox", "read_message", "mark_read"
  , "exo_status"
  ]
roleTools TL = Just $ Set.fromList
  [ "spawn_agents", "exo_status", "exo_complete", "file_pr"
  , "find_callers", "show_fields", "show_constructors", "teach-graph"
  , "send_message", "check_inbox", "read_message", "mark_read"
  , "confirm_action", "select_option", "request_guidance"
  ]

isToolAllowed :: Role -> Text -> Bool
isToolAllowed role toolName = case roleTools role of
  Nothing -> True
  Just allowed -> Set.member toolName allowed
