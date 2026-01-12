{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FieldSelectors #-}

-- | Node-private memory types for TDD protocol.
-- Prefix convention: lowercase acronym of type name
module TypesFirstDev.Types.Memory
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
import Tidepool.Effect.Session (SessionId)

import TypesFirstDev.Types.Nodes (ImplInput)

-- | TDD node memory (shared between TDDWriteTests and TDDReviewImpl).
-- Prefix: tm
data TDDMem = TDDMem
  { tmConversationId  :: Text    -- ^ Resume from parent's context
  , tmCoveredCriteria :: [Text]  -- ^ CriterionIds with passing tests
  , tmPendingTests    :: [Text]  -- ^ Tests written but not yet passed
  , tmReviewCritiques :: [Text]  -- ^ Critiques from TDDReviewImpl (for MoreTests route)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Empty TDD memory for initialization.
emptyTDDMem :: Text -> TDDMem
emptyTDDMem convId = TDDMem
  { tmConversationId = convId
  , tmCoveredCriteria = []
  , tmPendingTests = []
  , tmReviewCritiques = []
  }

-- | Impl node memory.
-- Prefix: im
data ImplMem = ImplMem
  { imConversationId :: Text                  -- ^ Resume from parent's context
  , imSessionId      :: Maybe SessionId       -- ^ Current session ID (for retry continuation)
  , imPassedTests    :: [Text]                -- ^ Tests that now pass
  , imAttemptHistory :: [AttemptRecord]       -- ^ History of attempts
  , imImplInput      :: Maybe ImplInput       -- ^ Stored input for routing in after handler
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Empty Impl memory for initialization.
emptyImplMem :: Text -> ImplMem
emptyImplMem convId = ImplMem
  { imConversationId = convId
  , imSessionId = Nothing
  , imPassedTests = []
  , imAttemptHistory = []
  , imImplInput = Nothing
  }

-- | Record of a single impl attempt.
-- Prefix: ar
data AttemptRecord = AttemptRecord
  { arStrategy :: Text  -- ^ What approach was tried
  , arOutcome  :: Text  -- ^ "failed: <reason>" or "partial: <progress>"
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)
