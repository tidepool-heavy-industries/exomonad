-- | Core domain types for Habitica API.
--
-- These types represent Habitica's domain model and are shared by
-- both request and response types.
module ExoMonad.Habitica.Types
  ( -- * ID Types
    TaskId (..),
    TodoId (..),
    ChecklistItemId (..),

    -- * Enums
    TaskType (..),
    Direction (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

-- ════════════════════════════════════════════════════════════════════════════
-- ID TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Opaque task ID from Habitica.
newtype TaskId = TaskId {unTaskId :: Text}
  deriving stock (Eq, Show)
  deriving newtype (ToJSON, FromJSON)

-- | Opaque todo ID from Habitica.
newtype TodoId = TodoId {unTodoId :: Text}
  deriving stock (Eq, Show)
  deriving newtype (ToJSON, FromJSON)

-- | Opaque checklist item ID from Habitica.
newtype ChecklistItemId = ChecklistItemId {unChecklistItemId :: Text}
  deriving stock (Eq, Show)
  deriving newtype (ToJSON, FromJSON)

-- ════════════════════════════════════════════════════════════════════════════
-- ENUMS
-- ════════════════════════════════════════════════════════════════════════════

-- | Habitica task types.
data TaskType
  = Habits
  | Dailys
  | Todos
  | Rewards
  deriving stock (Eq, Show, Generic)

-- | Score direction for tasks.
data Direction
  = Up
  | Down
  deriving stock (Eq, Show, Generic)
