{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Template context builders for MyTask node
module HumanDrivenDev.MyTaskContext
  ( MyTaskContext(..)
  , buildMyTaskContext
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import HumanDrivenDev.Types.MyTask (MyTaskInput(..), MyTaskMem(..))

-- | Context passed to Jinja template
--
-- IMPORTANT: Field names must match template variables exactly.
-- Template uses snake_case (e.g., {{ habitica_todos }})
data TaskRouterContext = TaskRouterContext
  { userMessage :: Text
    -- ^ User message
  , habiticaTodos :: [Text]
    -- ^ todo items (title only)
  , habiticaDailies :: Int
    -- ^ daily habit items (title only)
  , habiticaHabits :: [Text]
    -- ^ freeform habit items (title only)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)


-- | Context passed to Jinja template
--
-- IMPORTANT: Field names must match template variables exactly.
-- Template uses snake_case (e.g., {{ task_description }})
data MyTaskContext = MyTaskContext
  { taskDescription :: Text
    -- ^ What to accomplish
  , contextItems :: [Text]
    -- ^ Contextual information
  , attemptNumber :: Int
    -- ^ Current attempt (1-indexed for display)
  , previousAttempts :: [Text]
    -- ^ History of what was tried before
  , completedSubtasks :: [Text]
    -- ^ Subtasks already done
  , hasRetries :: Bool
    -- ^ Whether this is a retry
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Build template context from input and memory
buildMyTaskContext :: MyTaskInput -> MyTaskMem -> MyTaskContext
buildMyTaskContext input mem = MyTaskContext
  { taskDescription = input.mtiDescription
  , contextItems = input.mtiContext
  , attemptNumber = input.mtiAttemptCount + 1  -- 1-indexed for humans
  , previousAttempts = mem.mtmAttemptHistory
  , completedSubtasks = mem.mtmCompletedSubtasks
  , hasRetries = not . null $ mem.mtmAttemptHistory
  }
