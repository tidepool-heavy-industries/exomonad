-- | BD (Beads) effect for querying issue/task tracking data.
--
-- Effect type only - executors live in tidepool-bd-executor.
-- Enables graphs to read bead info, dependencies, and labels.
--
-- = Example Usage
--
-- @
-- import Tidepool.Effects.BD (BD, getBead, getDeps, getLabels)
--
-- myHandler :: Member BD effs => Text -> Eff effs ()
-- myHandler beadId = do
--   maybeBead <- getBead beadId
--   case maybeBead of
--     Nothing -> logInfo "Bead not found"
--     Just bead -> do
--       deps <- getDeps beadId
--       labels <- getLabels beadId
--       -- process bead info...
-- @
module Tidepool.Effects.BD
  ( -- * Effect
    BD(..)
  , getBead
  , getDeps
  , getBlocking
  , getLabels

    -- * Types
  , BeadInfo(..)
  , BeadStatus(..)
  , BeadType(..)
  , DependencyInfo(..)
  , DependencyType(..)
  ) where

import Control.Monad.Freer (Eff, Member, send)
import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , withObject
  , withText
  , object
  , (.=)
  , (.:)
  , (.:?)
  , (.!=)
  )
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)


-- ════════════════════════════════════════════════════════════════════════════
-- TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Bead status.
data BeadStatus
  = StatusOpen
  | StatusInProgress
  | StatusClosed
  | StatusHooked
  | StatusBlocked
  deriving (Show, Eq, Generic, Enum, Bounded)

instance ToJSON BeadStatus where
  toJSON StatusOpen       = "open"
  toJSON StatusInProgress = "in_progress"
  toJSON StatusClosed     = "closed"
  toJSON StatusHooked     = "hooked"
  toJSON StatusBlocked    = "blocked"

instance FromJSON BeadStatus where
  parseJSON = withText "BeadStatus" $ \case
    "open"        -> pure StatusOpen
    "in_progress" -> pure StatusInProgress
    "closed"      -> pure StatusClosed
    "hooked"      -> pure StatusHooked
    "blocked"     -> pure StatusBlocked
    t             -> fail $ "Unknown bead status: " ++ show t


-- | Bead type (issue_type in JSON).
data BeadType
  = TypeTask
  | TypeBug
  | TypeFeature
  | TypeEpic
  | TypeMergeRequest
  | TypeMessage
  | TypeMolecule
  | TypeAgent
  | TypeOther Text
  deriving (Show, Eq, Generic)

instance ToJSON BeadType where
  toJSON TypeTask         = "task"
  toJSON TypeBug          = "bug"
  toJSON TypeFeature      = "feature"
  toJSON TypeEpic         = "epic"
  toJSON TypeMergeRequest = "merge-request"
  toJSON TypeMessage      = "message"
  toJSON TypeMolecule     = "molecule"
  toJSON TypeAgent        = "agent"
  toJSON (TypeOther t)    = toJSON t

instance FromJSON BeadType where
  parseJSON = withText "BeadType" $ \case
    "task"          -> pure TypeTask
    "bug"           -> pure TypeBug
    "feature"       -> pure TypeFeature
    "epic"          -> pure TypeEpic
    "merge-request" -> pure TypeMergeRequest
    "message"       -> pure TypeMessage
    "molecule"      -> pure TypeMolecule
    "agent"         -> pure TypeAgent
    t               -> pure $ TypeOther t


-- | Dependency type between beads.
data DependencyType
  = DepParentChild  -- ^ Parent-child relationship
  | DepBlocks       -- ^ This bead blocks another
  | DepDependsOn    -- ^ This bead depends on another
  deriving (Show, Eq, Generic)

instance ToJSON DependencyType where
  toJSON DepParentChild = "parent-child"
  toJSON DepBlocks      = "blocks"
  toJSON DepDependsOn   = "depends-on"

instance FromJSON DependencyType where
  parseJSON = withText "DependencyType" $ \case
    "parent-child" -> pure DepParentChild
    "blocks"       -> pure DepBlocks
    "depends-on"   -> pure DepDependsOn
    t              -> fail $ "Unknown dependency type: " ++ show t


-- | Dependency info (simplified bead info for deps/dependents).
data DependencyInfo = DependencyInfo
  { diId          :: Text
  , diTitle       :: Text
  , diStatus      :: BeadStatus
  , diPriority    :: Int
  , diType        :: BeadType
  , diDepType     :: DependencyType
  }
  deriving (Show, Eq, Generic)

instance ToJSON DependencyInfo where
  toJSON d = object
    [ "id"              .= d.diId
    , "title"           .= d.diTitle
    , "status"          .= d.diStatus
    , "priority"        .= d.diPriority
    , "issue_type"      .= d.diType
    , "dependency_type" .= d.diDepType
    ]

instance FromJSON DependencyInfo where
  parseJSON = withObject "DependencyInfo" $ \v ->
    DependencyInfo
      <$> v .: "id"
      <*> v .: "title"
      <*> v .: "status"
      <*> v .: "priority"
      <*> v .: "issue_type"
      <*> v .: "dependency_type"


-- | Full bead information.
--
-- Matches the JSON output from @bd show --json@.
data BeadInfo = BeadInfo
  { biId          :: Text
  , biTitle       :: Text
  , biDescription :: Maybe Text
  , biStatus      :: BeadStatus
  , biPriority    :: Int
  , biType        :: BeadType
  , biAssignee    :: Maybe Text
  , biCreatedAt   :: Maybe UTCTime
  , biCreatedBy   :: Maybe Text
  , biUpdatedAt   :: Maybe UTCTime
  , biParent      :: Maybe Text
  , biDependencies :: [DependencyInfo]
  , biDependents   :: [DependencyInfo]
  }
  deriving (Show, Eq, Generic)

instance ToJSON BeadInfo where
  toJSON b = object
    [ "id"           .= b.biId
    , "title"        .= b.biTitle
    , "description"  .= b.biDescription
    , "status"       .= b.biStatus
    , "priority"     .= b.biPriority
    , "issue_type"   .= b.biType
    , "assignee"     .= b.biAssignee
    , "created_at"   .= b.biCreatedAt
    , "created_by"   .= b.biCreatedBy
    , "updated_at"   .= b.biUpdatedAt
    , "parent"       .= b.biParent
    , "dependencies" .= b.biDependencies
    , "dependents"   .= b.biDependents
    ]

instance FromJSON BeadInfo where
  parseJSON = withObject "BeadInfo" $ \v ->
    BeadInfo
      <$> v .:  "id"
      <*> v .:  "title"
      <*> v .:? "description"
      <*> v .:  "status"
      <*> v .:  "priority"
      <*> v .:  "issue_type"
      <*> v .:? "assignee"
      <*> v .:? "created_at"
      <*> v .:? "created_by"
      <*> v .:? "updated_at"
      <*> v .:? "parent"
      <*> v .:? "dependencies" .!= []
      <*> v .:? "dependents" .!= []


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT
-- ════════════════════════════════════════════════════════════════════════════

-- | BD (Beads) effect for querying issue/task tracking.
--
-- Read-only queries against the beads database.
data BD r where
  -- | Get full bead info by ID.
  GetBead :: Text -> BD (Maybe BeadInfo)

  -- | Get beads that this bead depends on (blockers).
  GetDeps :: Text -> BD [BeadInfo]

  -- | Get beads that this bead is blocking.
  GetBlocking :: Text -> BD [BeadInfo]

  -- | Get labels attached to a bead.
  GetLabels :: Text -> BD [Text]


-- ════════════════════════════════════════════════════════════════════════════
-- SMART CONSTRUCTORS
-- ════════════════════════════════════════════════════════════════════════════

-- | Get bead info by ID.
getBead :: Member BD effs => Text -> Eff effs (Maybe BeadInfo)
getBead = send . GetBead

-- | Get dependencies (beads this one depends on).
getDeps :: Member BD effs => Text -> Eff effs [BeadInfo]
getDeps = send . GetDeps

-- | Get blocking beads (beads this one is blocking).
getBlocking :: Member BD effs => Text -> Eff effs [BeadInfo]
getBlocking = send . GetBlocking

-- | Get labels for a bead.
getLabels :: Member BD effs => Text -> Eff effs [Text]
getLabels = send . GetLabels
