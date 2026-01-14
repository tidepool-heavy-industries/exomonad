{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Types for Habitica Task Router
module HumanDrivenDev.HabiticaRouter.Types
  ( HabiticaRouterInput(..)
  , HabiticaRouterExit(..)
  , HabiticaRouterMem(..)
  , emptyHabiticaRouterMem
  , GUITree(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Input to the Habitica Router
data HabiticaRouterInput = HabiticaRouterInput
  { hriUserMessage :: Text
    -- ^ The natural language message from the user
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Placeholder for structured GUI tree for follow-up questions
data GUITree = GUITree
  { questions :: [Text]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Exit outcomes for the Habitica Router
data HabiticaRouterExit
  = -- | Need more information from the user
    MoreDataNeeded
      { hreQuestions :: GUITree
      }
  | -- | Successfully extracted tasks
    HabiticaTodo
      { htTasks :: [Text]
        -- ^ TODO items to create
      , htEvents :: [Text]
        -- ^ Events that already happened (to mark complete)
      }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Persistent memory for the router (across interaction loops)
data HabiticaRouterMem = HabiticaRouterMem
  { hrmHistory :: [Text]
    -- ^ Previous user messages or assistant responses
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

emptyHabiticaRouterMem :: HabiticaRouterMem
emptyHabiticaRouterMem = HabiticaRouterMem { hrmHistory = [] }
