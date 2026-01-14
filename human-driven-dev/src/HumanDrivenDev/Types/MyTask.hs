{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Task-specific types for MyTask node
module HumanDrivenDev.Types.MyTask
  ( -- * Input
    MyTaskInput(..)
    -- * Exit
  , MyTaskExit(..)
    -- * Memory
  , MyTaskMem(..)
  , emptyMyTaskMem
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Input to the MyTask node
data MyTaskInput = MyTaskInput
  { mtiDescription :: Text
    -- ^ What this task should accomplish
  , mtiContext :: [Text]
    -- ^ Contextual information
  , mtiAttemptCount :: Int
    -- ^ Retry attempt number (starts at 0)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)


-- | Placeholder for structured GUI tree
-- TODO: Define actual GUI structure (questions, choices, etc.)
data GUITree = GUITree
  { questions :: [Text]  -- Simplified for now
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data GUITreeResponse = GUITreeResponse {
  answers :: [Text]
}


-- | Input type for TaskRouter - what the LLM sees 
data TaskRouterInput
  = TaskRouterInput
      { triUserMessage :: Text
        -- ^ natural-language user message
      }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)


-- | Exit type for TaskRouter - what the LLM decides
data TaskRouterExit
  = -- | Insufficient information, need to ask user via GUI
    MoreDataNeeded
      { tredQuestions :: GUITree
        -- ^ Structured GUI with followup questions
      }
  | -- | Extracted Habitica tasks/events from user input
    HabiticaTodo
      { htTasks :: [Text]
        -- ^ TODO items to create
      , htEvents :: [Text]
        -- ^ Events that already happened (to mark complete)
      }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Exit type for MyTask node
--
-- Each constructor represents a different outcome/transition
data MyTaskExit
  = -- | Task completed successfully
    TaskComplete
      { tceResult :: Text
      , tceDetails :: [Text]
      }
  | -- | Need to retry the task
    TaskRetry
      { treReason :: Text
      , treAdjustments :: [Text]
      }
  | -- | Need to spawn subtasks
    TaskSpawn
      { tseSubtasks :: [Text]
      , tseStrategy :: Text
      }
  | -- | Task failed unrecoverably
    TaskFailed
      { tfeError :: Text
      , tfeDiagnostics :: [Text]
      }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Node-private memory for MyTask
--
-- Persists across self-loops
data MyTaskMem = MyTaskMem
  { mtmAttemptHistory :: [Text]
    -- ^ Record of previous attempts
  , mtmCompletedSubtasks :: [Text]
    -- ^ Subtasks that finished
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Initial empty memory
emptyMyTaskMem :: MyTaskMem
emptyMyTaskMem = MyTaskMem
  { mtmAttemptHistory = []
  , mtmCompletedSubtasks = []
  }
