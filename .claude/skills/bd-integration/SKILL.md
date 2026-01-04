---
name: bd-integration
description: Use when working with the BD (Beads) effect for issue/task tracking, querying bead info, dependencies, or labels.
---

# BD (Beads) Integration

BD is a git-native issue tracker. The Tidepool BD integration provides typed access to beads from agent code.

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│  tidepool-core/src/Tidepool/Effects/BD.hs                   │
│  - Effect type (BD)                                          │
│  - Types (BeadInfo, BeadStatus, BeadType, DependencyInfo)   │
│  - Smart constructors (getBead, getDeps, getBlocking, etc.) │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│  tidepool-native-gui/bd-executor/                           │
│  - BDConfig (database path, quiet mode)                     │
│  - runBDIO (CLI executor)                                   │
│  - runBD (pure handler for testing)                         │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│  bd CLI                                                      │
│  - bd show --json <id>                                       │
│  - bd label list --json <id>                                 │
│  - bd dep list <id>                                          │
└─────────────────────────────────────────────────────────────┘
```

## Effect Operations

```haskell
import Tidepool.Effects.BD (BD, getBead, getDeps, getBlocking, getLabels)

-- Get full bead info by ID
getBead :: Member BD effs => Text -> Eff effs (Maybe BeadInfo)

-- Get beads this one depends on (blockers)
getDeps :: Member BD effs => Text -> Eff effs [BeadInfo]

-- Get beads that depend on this one (blocking)
getBlocking :: Member BD effs => Text -> Eff effs [BeadInfo]

-- Get labels attached to a bead
getLabels :: Member BD effs => Text -> Eff effs [Text]
```

## Types

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
  , biDependencies :: [DependencyInfo]  -- What this blocks on
  , biDependents   :: [DependencyInfo]  -- What depends on this
  }
```

### DependencyInfo

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

## Executor Configuration

```haskell
import Tidepool.BD.Executor (BDConfig(..), runBDIO, defaultBDConfig)

data BDConfig = BDConfig
  { bcBeadsDir :: Maybe FilePath  -- SQLite file path (.beads/beads.db)
  , bcQuiet    :: Bool            -- Suppress stderr warnings
  }

defaultBDConfig :: BDConfig
defaultBDConfig = BDConfig { bcBeadsDir = Nothing, bcQuiet = True }
```

## Usage Patterns

### Basic Query

```haskell
import Tidepool.Effects.BD
import Tidepool.BD.Executor

example :: IO ()
example = do
  let config = defaultBDConfig
  runM $ runBDIO config $ do
    maybeBead <- getBead "gt-hda.2.2"
    case maybeBead of
      Nothing -> pure ()
      Just bead -> do
        deps <- getDeps bead.biId
        labels <- getLabels bead.biId
        -- Process bead info...
```

### With Specific Database

```haskell
-- Point to a specific beads database
let config = BDConfig
      { bcBeadsDir = Just "/path/to/project/.beads/beads.db"
      , bcQuiet = True
      }
```

### Pure Handler (Testing)

```haskell
import Tidepool.BD.Executor (runBD)

testExample :: IO (Maybe BeadInfo, [Text])
testExample = runM $ runBD mockGetBead mockGetDeps mockGetBlocking mockGetLabels $ do
  bead <- getBead "any-id"
  labels <- getLabels "any-id"
  pure (bead, labels)
  where
    mockGetBead _ = pure $ Just BeadInfo { biId = "mock-1", ... }
    mockGetDeps _ = pure []
    mockGetBlocking _ = pure []
    mockGetLabels _ = pure ["label1", "label2"]
```

## JSON Schema

The executor parses JSON from `bd show --json` and `bd label list --json`:

```json
// bd show --json returns array with single element
[{
  "id": "gt-hda.1",
  "title": "Foundation Layer",
  "description": "Build the integration layer",
  "status": "open",
  "priority": 1,
  "issue_type": "epic",
  "assignee": "tidepool/polecats/slit",
  "created_at": "2026-01-03T21:20:49Z",
  "parent": "gt-hda",
  "dependencies": [{
    "id": "gt-hda",
    "title": "Parent Epic",
    "status": "hooked",
    "priority": 2,
    "issue_type": "epic",
    "dependency_type": "parent-child"
  }],
  "dependents": [{
    "id": "gt-hda.2",
    "title": "Child Task",
    "status": "open",
    "priority": 2,
    "issue_type": "task",
    "dependency_type": "blocks"
  }]
}]

// bd label list --json returns array of strings
["infrastructure", "urgent", "blocked"]
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

### Optional Fields

The `dependencies` and `dependents` fields are optional in JSON. The `FromJSON`
instance handles this correctly with `.!= []`:

```haskell
instance FromJSON BeadInfo where
  parseJSON = withObject "BeadInfo" $ \v ->
    BeadInfo
      <$> v .:  "id"
      -- ...
      <*> v .:? "dependencies" .!= []  -- Optional, defaults to []
      <*> v .:? "dependents" .!= []    -- Optional, defaults to []
```

### bd CLI Availability

Tests skip gracefully when `bd` is not in PATH:

```haskell
isBdAvailable :: IO Bool
isBdAvailable = do
  result <- try $ readProcessWithExitCode "bd" ["--help"] ""
  pure $ case result of
    Left (_ :: SomeException) -> False
    Right (ExitSuccess, _, _) -> True
    Right _ -> False
```

## Test Suite

Located at `tidepool-native-gui/bd-executor/test/Main.hs`:

- **JSON parsing tests** (23): All types round-trip correctly
- **Mock CLI tests** (5): Executor handles edge cases
- **Real CLI tests** (6): Integration with actual `bd` CLI

Run with:
```bash
cabal test tidepool-bd-executor:bd-executor-test
```
