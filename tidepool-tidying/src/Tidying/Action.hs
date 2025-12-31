{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Tidying.Action
  ( -- * Action (DECIDE output)
    Action(..)

    -- * Action classification
  , isQuestion
  , isInstruction
  , needsLLM
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import GHC.Generics (Generic)

import Tidying.Types (ItemName, Location, AnxietyTrigger, CategoryName)

-- | Actions the agent can take
data Action
  -- Questions (surveying)
  = AskFunction              -- ^ "What do you need to DO in this space?"
  | AskAnchors               -- ^ "What's definitely staying?"
  | AskWhatIsIt              -- ^ "What is it?"
  | AskWhereLive             -- ^ "Desk or elsewhere?"
  | AskItemDecision ItemName -- ^ "Trash, keep, or not sure?" for specific item

  -- Instructions (sorting)
  | FirstInstruction         -- ^ Initial momentum-building action
  | InstructTrash            -- ^ "Trash. Next."
  | InstructPlace Location   -- ^ "Put it on [shelf]. Next."
  | InstructUnsure           -- ^ "Unsure pile, floor right. Next."
  | InstructNext             -- ^ "Next thing."
  | InstructBag              -- ^ "Bag the trash by the door."

  -- Splitting (NonEmpty guarantees at least one category)
  | InstructSplit (NonEmpty CategoryName) -- ^ "Split: [cables] here, [papers] there."

  -- Decision support
  | DecisionAid ItemName     -- ^ Reframe using function for specific item
  | EnergyCheck              -- ^ "Keep going or stop?"

  -- Pivoting (first param = anxiety trigger, second = alternative area)
  | PivotAway AnxietyTrigger Location -- ^ Avoid trigger, do alternative

  -- Completion
  | AckProgress Text         -- ^ Acknowledge + context (free-form message)
  | Summary                  -- ^ Session summary
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Is this a question action?
isQuestion :: Action -> Bool
isQuestion AskFunction = True
isQuestion AskAnchors = True
isQuestion AskWhatIsIt = True
isQuestion AskWhereLive = True
isQuestion (AskItemDecision _) = True
isQuestion EnergyCheck = True
isQuestion _ = False

-- | Is this an instruction action?
isInstruction :: Action -> Bool
isInstruction FirstInstruction = True
isInstruction InstructTrash = True
isInstruction (InstructPlace _) = True
isInstruction InstructUnsure = True
isInstruction InstructNext = True
isInstruction InstructBag = True
isInstruction (InstructSplit _) = True
isInstruction _ = False

-- | Does this action need LLM generation or is it canned?
needsLLM :: Action -> Bool
needsLLM FirstInstruction = True   -- needs photo analysis
needsLLM (DecisionAid _) = True    -- needs reframe
needsLLM (PivotAway _ _) = True    -- needs context
needsLLM Summary = True            -- needs summary
needsLLM (AckProgress _) = True    -- needs context
needsLLM (InstructSplit _) = True  -- needs pile analysis
needsLLM _ = False                 -- canned responses
