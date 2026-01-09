{-# LANGUAGE DeriveGeneric #-}
-- | Test data types for TH schema generation tests.
-- These are in a separate module because TH splices can only see
-- definitions from earlier in the module or from imported modules.
module Text.Ginger.TH.TestTypes where

import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)

-- Simple record
data SimpleRecord = SimpleRecord
  { srName :: Text
  , srAge :: Int
  } deriving (Show, Eq, Generic)

-- Nested record
data NestedRecord = NestedRecord
  { nrUser :: SimpleRecord
  , nrActive :: Bool
  } deriving (Show, Eq, Generic)

-- Sum type
data ContentType
  = TextContent { ctBody :: Text }
  | ImageContent { ctUrl :: Text, ctAlt :: Text }
  deriving (Show, Eq, Generic)

-- Sum type with shared field
data Animal
  = Dog { animalName :: Text, animalBreed :: Text }
  | Cat { animalName :: Text, animalColor :: Text }
  deriving (Show, Eq, Generic)

-- Recursive type
data Tree = Node
  { treeValue :: Int
  , treeChildren :: [Tree]
  } deriving (Show, Eq, Generic)

-- Record with list
data WithList = WithList
  { wlItems :: [Text]
  , wlCount :: Int
  } deriving (Show, Eq, Generic)

-- Record with Maybe
data WithMaybe = WithMaybe
  { wmRequired :: Text
  , wmOptional :: Maybe Text
  } deriving (Show, Eq, Generic)

-- Record with Vector
data WithVector = WithVector
  { wvItems :: Vector Text
  } deriving (Show, Eq, Generic)

-- Newtype wrapper
newtype UserId = UserId { unUserId :: Int }
  deriving (Show, Eq, Generic)

-- Type synonym (used in a record)
type Email = Text

data WithTypeSynonym = WithTypeSynonym
  { wtsEmail :: Email
  , wtsName :: Text
  } deriving (Show, Eq, Generic)

-- | Non-record sum type (no named fields)
-- This is opaque because it uses positional constructors
data Status
  = Active
  | Inactive
  | Pending Int  -- with payload
  deriving (Show, Eq, Generic)

-- | Record with an opaque nested type
-- The template should be able to access `name` but not `status.payload` etc.
data WithOpaqueField = WithOpaqueField
  { wofName :: Text
  , wofStatus :: Status  -- opaque: non-record sum type
  } deriving (Show, Eq, Generic)

-- | Non-record single constructor (positional)
data Point = Point Int Int
  deriving (Show, Eq, Generic)

-- | Record with multiple opaque nested types
data ComplexWithOpaque = ComplexWithOpaque
  { cwoTitle :: Text
  , cwoPoint :: Point          -- opaque: positional constructor
  , cwoMaybePoint :: Maybe Point  -- Maybe wraps opaque
  , cwoPoints :: [Point]       -- list of opaque
  } deriving (Show, Eq, Generic)

-- | Mixed sum type: some constructors have records, some don't
data MixedContent
  = TextBlock { mcText :: Text }
  | ImageBlock { mcUrl :: Text }
  | Divider  -- nullary, no fields
  deriving (Show, Eq, Generic)

-- | For testing genericToGVal - simple sum type
data TestStatus
  = TestBlocked Text
  | TestPursuing Int
  | TestAchieved
  deriving (Show, Eq, Generic)

-- | For testing genericToGVal - record sum type
data TestEvent
  = TestAttack { teAttacker :: Text, teTarget :: Text }
  | TestHeal { teHealer :: Text, teAmount :: Int }
  | TestWait
  deriving (Show, Eq, Generic)

-- | Empty context type for testing templates that don't need context
data EmptyContext = EmptyContext
  deriving (Show, Eq, Generic)
