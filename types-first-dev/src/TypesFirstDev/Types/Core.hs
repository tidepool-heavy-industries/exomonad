{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FieldSelectors #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Core entry types for TDD protocol.
-- Prefix convention: lowercase acronym of type name (e.g., Spec -> s)
-- JSON parsing uses snake_case without prefix (e.g., sDescription -> description)
module TypesFirstDev.Types.Core
  ( Spec(..)
  , defaultSpec
  , Constraints(..)
  , Criterion(..)
  , ParentContext(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON, Value(..), genericParseJSON)
import Data.Aeson.TH (deriveJSON)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Tidepool.StructuredOutput (StructuredOutput)
import TypesFirstDev.Types.JsonOpts (jsonOpts)

-- ════════════════════════════════════════════════════════════════════════════
-- Types that Spec depends on must be defined first (TH staging)
-- ════════════════════════════════════════════════════════════════════════════

-- | Complexity constraints for implementation.
-- Prefix: cn (constraints)
data Constraints = Constraints
  { cnTime  :: Maybe Text  -- ^ e.g. "O(n log n)"
  , cnSpace :: Maybe Text  -- ^ e.g. "O(1)"
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (StructuredOutput)

$(deriveJSON jsonOpts ''Constraints)

-- | Single acceptance criterion.
-- Prefix: c
data Criterion = Criterion
  { cId   :: Text  -- ^ Unique ID within spec
  , cText :: Text  -- ^ Human-readable requirement
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (StructuredOutput, ToJSON)

-- | Custom FromJSON that accepts either a plain string or {id, text} object.
-- Plain strings get auto-generated IDs based on content hash.
instance FromJSON Criterion where
  parseJSON (String t) = pure $ Criterion
    { cId = "c-" <> T.take 8 (T.pack $ show $ abs $ hash t)
    , cText = t
    }
    where
      -- Simple hash for ID generation
      hash :: Text -> Int
      hash = T.foldl' (\h c -> 33 * h + fromEnum c) 5381
  parseJSON v = genericParseJSON jsonOpts v

-- ════════════════════════════════════════════════════════════════════════════
-- Main types
-- ════════════════════════════════════════════════════════════════════════════

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
  deriving anyclass (StructuredOutput)

$(deriveJSON jsonOpts ''Spec)

-- | Context passed to child nodes from parent.
-- Prefix: pc
data ParentContext = ParentContext
  { pcInterface        :: Text  -- ^ Parent's interface definitions
  , pcAssignedCriteria :: Text  -- ^ Which criteria this child must satisfy
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (StructuredOutput)

$(deriveJSON jsonOpts ''ParentContext)

-- | Default spec for testing and placeholder values.
defaultSpec :: Spec
defaultSpec = Spec
  { sId = "test-spec"
  , sDescription = "Test specification"
  , sAcceptanceCriteria = []
  , sTargetPath = "src/Test"
  , sTestPath = "test/Test"
  , sComplexityConstraints = Nothing
  }
