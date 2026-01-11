{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FieldSelectors #-}

-- | Node-private memory types for V3 protocol.
-- Prefix convention: lowercase acronym of type name
module TypesFirstDev.V3.Types.Memory
  ( TDDMem(..)
  , emptyTDDMem
  , ImplMem(..)
  , emptyImplMem
  , AttemptRecord(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import Tidepool.StructuredOutput (StructuredOutput)

-- | TDD node memory (shared between TDDWriteTests and TDDReviewImpl).
-- Prefix: tm
data TDDMem = TDDMem
  { tmConversationId  :: Text    -- ^ Resume from parent's context
  , tmCoveredCriteria :: [Text]  -- ^ CriterionIds with passing tests
  , tmPendingTests    :: [Text]  -- ^ Tests written but not yet passed
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Empty TDD memory for initialization.
emptyTDDMem :: Text -> TDDMem
emptyTDDMem convId = TDDMem
  { tmConversationId = convId
  , tmCoveredCriteria = []
  , tmPendingTests = []
  }

-- | Impl node memory.
-- Prefix: im
data ImplMem = ImplMem
  { imConversationId :: Text             -- ^ Resume from parent's context
  , imPassedTests    :: [Text]           -- ^ Tests that now pass
  , imAttemptHistory :: [AttemptRecord]  -- ^ History of attempts
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Empty Impl memory for initialization.
emptyImplMem :: Text -> ImplMem
emptyImplMem convId = ImplMem
  { imConversationId = convId
  , imPassedTests = []
  , imAttemptHistory = []
  }

-- | Record of a single impl attempt.
-- Prefix: ar
data AttemptRecord = AttemptRecord
  { arStrategy :: Text  -- ^ What approach was tried
  , arOutcome  :: Text  -- ^ "failed: <reason>" or "partial: <progress>"
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)
