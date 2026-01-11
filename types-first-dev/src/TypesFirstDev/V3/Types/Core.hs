{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FieldSelectors #-}

-- | Core entry types for V3 protocol.
-- Prefix convention: lowercase acronym of type name (e.g., Spec -> s)
module TypesFirstDev.V3.Types.Core
  ( Spec(..)
  , Constraints(..)
  , Criterion(..)
  , ParentContext(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import Tidepool.StructuredOutput (StructuredOutput)

-- | Specification for a work item. Entry point for the V3 graph.
-- Prefix: s
data Spec = Spec
  { sId                   :: Text              -- ^ Unique identifier
  , sDescription          :: Text              -- ^ What we're building
  , sAcceptanceCriteria   :: [Criterion]       -- ^ Must-have features
  , sTargetPath           :: FilePath          -- ^ Where to write implementation
  , sTestPath             :: FilePath          -- ^ Where to write tests
  , sComplexityConstraints :: Maybe Constraints -- ^ Optional O(n), space limits
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Complexity constraints for implementation.
-- Prefix: cn (constraints)
data Constraints = Constraints
  { cnTime  :: Maybe Text  -- ^ e.g. "O(n log n)"
  , cnSpace :: Maybe Text  -- ^ e.g. "O(1)"
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Single acceptance criterion.
-- Prefix: c
data Criterion = Criterion
  { cId   :: Text  -- ^ Unique ID within spec
  , cText :: Text  -- ^ Human-readable requirement
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Context passed to child nodes from parent.
-- Prefix: pc
data ParentContext = ParentContext
  { pcInterface        :: Text  -- ^ Parent's interface definitions
  , pcAssignedCriteria :: Text  -- ^ Which criteria this child must satisfy
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)
