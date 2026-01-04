---
name: bd-integration
description: Use when working with the BD (Beads) effect for issue/task tracking, reading/writing bead info, dependencies, labels, or hierarchies.
---

# BD (Beads) Integration

BD is a git-native issue tracker. The Tidepool BD integration provides typed read/write access to beads from agent code.

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│  tidepool-core/src/Tidepool/Effects/BD.hs                   │
│  - Effect type (BD)                                          │
│  - Types (BeadInfo, BeadStatus, BeadType, DependencyInfo)   │
│  - Input types (CreateBeadInput, UpdateBeadInput)           │
│  - Smart constructors (read + write operations)             │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│  tidepool-native-gui/bd-executor/                           │
│  - BDConfig (database path, quiet mode)                     │
│  - runBDIO (CLI executor)                                   │
│  - runBD (pure handler for testing - read-only)             │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│  bd CLI                                                      │
│  - bd show/create/update/close/reopen                        │
│  - bd label add/remove/list                                  │
│  - bd dep add/remove                                         │
│  - bd list --parent                                          │
└─────────────────────────────────────────────────────────────┘
```

## Read Operations

```haskell
import Tidepool.Effects.BD

-- Get full bead info by ID
getBead :: Member BD effs => Text -> Eff effs (Maybe BeadInfo)

-- Get beads this one depends on (blockers)
getDeps :: Member BD effs => Text -> Eff effs [BeadInfo]

-- Get beads that depend on this one (blocking)
getBlocking :: Member BD effs => Text -> Eff effs [BeadInfo]

-- Get labels attached to a bead
getLabels :: Member BD effs => Text -> Eff effs [Text]

-- Get child beads (beads with this as parent)
getChildren :: Member BD effs => Text -> Eff effs [BeadInfo]
```

## Write Operations

```haskell
-- Create a new bead, returns generated ID
createBead :: Member BD effs => CreateBeadInput -> Eff effs Text

-- Update an existing bead
updateBead :: Member BD effs => Text -> UpdateBeadInput -> Eff effs ()

-- Close a bead (status → closed)
closeBead :: Member BD effs => Text -> Eff effs ()

-- Reopen a bead (status → open)
reopenBead :: Member BD effs => Text -> Eff effs ()

-- Add/remove labels
addLabel :: Member BD effs => Text -> Text -> Eff effs ()
removeLabel :: Member BD effs => Text -> Text -> Eff effs ()

-- Add/remove dependencies
addDep :: Member BD effs => Text -> Text -> DependencyType -> Eff effs ()
removeDep :: Member BD effs => Text -> Text -> Eff effs ()
```

## Input Types

### CreateBeadInput

```haskell
data CreateBeadInput = CreateBeadInput
  { cbiTitle       :: Text              -- Required: bead title
  , cbiDescription :: Maybe Text        -- Optional description
  , cbiType        :: BeadType          -- Bead type (default: Task)
  , cbiPriority    :: Int               -- Priority 0-4 (default: 2)
  , cbiParent      :: Maybe Text        -- Parent bead ID for hierarchy
  , cbiLabels      :: [Text]            -- Initial labels
  , cbiAssignee    :: Maybe Text        -- Assignee
  , cbiDeps        :: [(Text, DependencyType)]  -- Dependencies
  }

-- Convenience default
defaultCreateInput :: CreateBeadInput
```

### UpdateBeadInput

```haskell
data UpdateBeadInput = UpdateBeadInput
  { ubiTitle       :: Maybe Text        -- New title
  , ubiDescription :: Maybe Text        -- New description
  , ubiStatus      :: Maybe BeadStatus  -- New status
  , ubiPriority    :: Maybe Int         -- New priority
  , ubiAssignee    :: Maybe Text        -- New assignee
  }

-- Convenience default (no changes)
emptyUpdateInput :: UpdateBeadInput
```

## Data Types

### BeadStatus

```haskell
data BeadStatus
  = StatusOpen
  | StatusInProgress
  | StatusClosed
  | StatusHooked      -- Awaiting external event
  | StatusBlocked     -- Blocked by dependency
```

### BeadType

```haskell
data BeadType
  = TypeTask
  | TypeBug
  | TypeFeature
  | TypeEpic
  | TypeMergeRequest
  | TypeMessage
  | TypeMolecule
  | TypeAgent
  | TypeOther Text    -- Extensible
```

### BeadInfo

```haskell
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
```

### DependencyInfo / DependencyType

```haskell
data DependencyInfo = DependencyInfo
  { diId      :: Text
  , diTitle   :: Text
  , diStatus  :: BeadStatus
  , diPriority :: Int
  , diType    :: BeadType
  , diDepType :: DependencyType
  }

data DependencyType
  = DepParentChild  -- Hierarchical
  | DepBlocks       -- This blocks another
  | DepDependsOn    -- This depends on another
```

## Usage Patterns

### Reading Bead Context

```haskell
import Tidepool.Effects.BD
import Tidepool.BD.Executor

getBeadContext :: Member BD effs => Text -> Eff effs BeadContext
getBeadContext beadId = do
  maybeBead <- getBead beadId
  case maybeBead of
    Nothing -> error "Bead not found"
    Just bead -> do
      deps <- getDeps beadId
      blocking <- getBlocking beadId
      labels <- getLabels beadId
      children <- getChildren beadId
      pure BeadContext { .. }
```

### Creating Child Tasks

```haskell
breakdownEpic :: Member BD effs => Text -> [Text] -> Eff effs [Text]
breakdownEpic parentId subtaskTitles = do
  forM subtaskTitles $ \title -> do
    createBead $ defaultCreateInput
      { cbiTitle = title
      , cbiParent = Just parentId
      , cbiType = TypeTask
      }
```

### Managing Work Status

```haskell
claimAndWork :: Member BD effs => Text -> Eff effs ()
claimAndWork beadId = do
  -- Claim it
  updateBead beadId $ emptyUpdateInput
    { ubiStatus = Just StatusInProgress
    , ubiAssignee = Just "agent/polecat-1"
    }
  addLabel beadId "in-progress"

  -- ... do work ...

  -- Complete
  removeLabel beadId "in-progress"
  closeBead beadId
```

### Running with Real CLI

```haskell
main :: IO ()
main = do
  let config = BDConfig
        { bcBeadsDir = Just "/project/.beads/beads.db"
        , bcQuiet = True
        }
  runM $ runBDIO config $ do
    -- Create a task
    taskId <- createBead $ defaultCreateInput
      { cbiTitle = "Investigate bug"
      , cbiType = TypeBug
      , cbiPriority = 1
      }

    -- Add context
    addLabel taskId "needs-triage"

    -- Query it
    maybeBead <- getBead taskId
    case maybeBead of
      Just bead -> putStrLn $ "Created: " <> bead.biTitle
      Nothing -> putStrLn "Failed to create"
```

## Common Issues

### Database Path

The `--db` flag expects the SQLite file path, not the directory:

```haskell
-- Correct
bcBeadsDir = Just "/project/.beads/beads.db"

-- Wrong (won't find database)
bcBeadsDir = Just "/project/.beads"
```

### Parent vs Dependency

- `cbiParent` creates a **hierarchical** relationship (shows in `getChildren`)
- `cbiDeps` creates **dependencies** (shows in `getDeps`/`getBlocking`)

```haskell
-- Child task (hierarchical)
createBead $ defaultCreateInput
  { cbiTitle = "Subtask"
  , cbiParent = Just epicId
  }

-- Dependent task (blocks/depends-on)
createBead $ defaultCreateInput
  { cbiTitle = "Depends on other"
  , cbiDeps = [(blockerId, DepDependsOn)]
  }
```

## Test Suite

Located at `tidepool-native-gui/bd-executor/test/Main.hs`:

- **JSON parsing tests** (23): All types round-trip correctly
- **Mock CLI tests** (5): Executor handles edge cases
- **Real CLI read tests** (6): Read operations against real `bd`
- **Real CLI write tests** (5): Write operations against real `bd`

Run with:
```bash
cabal test tidepool-bd-executor:bd-executor-test
```
