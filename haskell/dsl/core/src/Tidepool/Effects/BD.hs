-- | BD (Beads) effect for issue/task tracking.
--
-- Effect type only - interpreters live in tidepool-bd-interpreter.
-- Enables graphs to read and write bead info, dependencies, and labels.
--
-- = Example Usage
--
-- @
-- import Tidepool.Effects.BD
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
--
-- createTask :: Member BD effs => Text -> Eff effs Text
-- createTask parentId = do
--   newId <- createBead $ defaultCreateInput
--     { cbiTitle = "Subtask"
--     , cbiParent = Just parentId
--     }
--   addLabel newId "auto-created"
--   pure newId
-- @
module Tidepool.Effects.BD
  ( -- * Effect
    BD(..)

    -- * Read Operations
  , getBead
  , getDeps
  , getBlocking
  , getLabels
  , getChildren
  , listByStatus
  , listByType

    -- * Write Operations
  , createBead
  , updateBead
  , closeBead
  , reopenBead
  , addLabel
  , removeLabel
  , addDep
  , removeDep

    -- * Input Types
  , CreateBeadInput(..)
  , defaultCreateInput
  , UpdateBeadInput(..)
  , emptyUpdateInput

    -- * Data Types
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
-- INPUT TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Input for creating a new bead.
data CreateBeadInput = CreateBeadInput
  { cbiTitle       :: Text              -- ^ Required: bead title
  , cbiDescription :: Maybe Text        -- ^ Optional description
  , cbiType        :: BeadType          -- ^ Bead type (default: Task)
  , cbiPriority    :: Int               -- ^ Priority 0-4 (default: 2)
  , cbiParent      :: Maybe Text        -- ^ Parent bead ID for hierarchy
  , cbiLabels      :: [Text]            -- ^ Initial labels
  , cbiAssignee    :: Maybe Text        -- ^ Assignee
  , cbiDeps        :: [(Text, DependencyType)]  -- ^ Dependencies: (beadId, depType)
  }
  deriving (Show, Eq, Generic)

-- | Default create input with just a title.
defaultCreateInput :: CreateBeadInput
defaultCreateInput = CreateBeadInput
  { cbiTitle       = ""
  , cbiDescription = Nothing
  , cbiType        = TypeTask
  , cbiPriority    = 2
  , cbiParent      = Nothing
  , cbiLabels      = []
  , cbiAssignee    = Nothing
  , cbiDeps        = []
  }


-- | Input for updating an existing bead.
--
-- All fields are optional - 'Nothing' means "don't change".
data UpdateBeadInput = UpdateBeadInput
  { ubiTitle       :: Maybe Text        -- ^ New title
  , ubiDescription :: Maybe Text        -- ^ New description
  , ubiStatus      :: Maybe BeadStatus  -- ^ New status
  , ubiPriority    :: Maybe Int         -- ^ New priority
  , ubiAssignee    :: Maybe Text        -- ^ New assignee
  }
  deriving (Show, Eq, Generic)

-- | Empty update input (no changes).
emptyUpdateInput :: UpdateBeadInput
emptyUpdateInput = UpdateBeadInput
  { ubiTitle       = Nothing
  , ubiDescription = Nothing
  , ubiStatus      = Nothing
  , ubiPriority    = Nothing
  , ubiAssignee    = Nothing
  }


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT
-- ════════════════════════════════════════════════════════════════════════════

-- | BD (Beads) effect for issue/task tracking.
--
-- Supports both read and write operations against the beads database.
data BD r where
  -- Read operations
  -- | Get full bead info by ID.
  GetBead :: Text -> BD (Maybe BeadInfo)

  -- | Get beads that this bead depends on (blockers).
  GetDeps :: Text -> BD [BeadInfo]

  -- | Get beads that this bead is blocking.
  GetBlocking :: Text -> BD [BeadInfo]

  -- | Get labels attached to a bead.
  GetLabels :: Text -> BD [Text]

  -- | Get child beads (beads with this as parent).
  GetChildren :: Text -> BD [BeadInfo]

  -- | List beads by status.
  ListByStatus :: BeadStatus -> BD [BeadInfo]

  -- | List beads by type.
  ListByType :: BeadType -> BD [BeadInfo]

  -- Write operations
  -- | Create a new bead, returns the generated ID.
  CreateBead :: CreateBeadInput -> BD Text

  -- | Update an existing bead.
  UpdateBead :: Text -> UpdateBeadInput -> BD ()

  -- | Close a bead (set status to closed).
  CloseBead :: Text -> BD ()

  -- | Reopen a bead (set status to open).
  ReopenBead :: Text -> BD ()

  -- | Add a label to a bead.
  AddLabel :: Text -> Text -> BD ()

  -- | Remove a label from a bead.
  RemoveLabel :: Text -> Text -> BD ()

  -- | Add a dependency between beads.
  AddDep :: Text -> Text -> DependencyType -> BD ()

  -- | Remove a dependency between beads.
  RemoveDep :: Text -> Text -> BD ()


-- ════════════════════════════════════════════════════════════════════════════
-- SMART CONSTRUCTORS - READ
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

-- | Get child beads (beads with this as parent).
getChildren :: Member BD effs => Text -> Eff effs [BeadInfo]
getChildren = send . GetChildren

-- | List beads by status.
listByStatus :: Member BD effs => BeadStatus -> Eff effs [BeadInfo]
listByStatus = send . ListByStatus

-- | List beads by type.
listByType :: Member BD effs => BeadType -> Eff effs [BeadInfo]
listByType = send . ListByType


-- ════════════════════════════════════════════════════════════════════════════
-- SMART CONSTRUCTORS - WRITE
-- ════════════════════════════════════════════════════════════════════════════

-- | Create a new bead, returns the generated ID.
--
-- Example:
--
-- @
-- newId <- createBead $ defaultCreateInput
--   { cbiTitle = "Fix bug in login"
--   , cbiType = TypeBug
--   , cbiPriority = 1
--   , cbiLabels = ["urgent", "auth"]
--   }
-- @
createBead :: Member BD effs => CreateBeadInput -> Eff effs Text
createBead = send . CreateBead

-- | Update an existing bead.
--
-- Example:
--
-- @
-- updateBead "bd-123" $ emptyUpdateInput
--   { ubiStatus = Just StatusInProgress
--   , ubiAssignee = Just "alice"
--   }
-- @
updateBead :: Member BD effs => Text -> UpdateBeadInput -> Eff effs ()
updateBead beadId input = send $ UpdateBead beadId input

-- | Close a bead (set status to closed).
closeBead :: Member BD effs => Text -> Eff effs ()
closeBead = send . CloseBead

-- | Reopen a closed bead (set status to open).
reopenBead :: Member BD effs => Text -> Eff effs ()
reopenBead = send . ReopenBead

-- | Add a label to a bead.
addLabel :: Member BD effs => Text -> Text -> Eff effs ()
addLabel beadId label = send $ AddLabel beadId label

-- | Remove a label from a bead.
removeLabel :: Member BD effs => Text -> Text -> Eff effs ()
removeLabel beadId label = send $ RemoveLabel beadId label

-- | Add a dependency between beads.
--
-- @addDep fromId toId depType@ creates a dependency where @fromId@ depends on @toId@.
addDep :: Member BD effs => Text -> Text -> DependencyType -> Eff effs ()
addDep fromId toId depType = send $ AddDep fromId toId depType

-- | Remove a dependency between beads.
removeDep :: Member BD effs => Text -> Text -> Eff effs ()
removeDep fromId toId = send $ RemoveDep fromId toId
