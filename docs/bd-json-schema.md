# BD CLI JSON Output Schemas

This document describes the JSON output formats from `bd` (beads) CLI commands.
Use these schemas for the BD effect in tidepool graphs.

## Command Support

| Command | --json Support | Notes |
|---------|---------------|-------|
| `bd show <id>` | Yes | Full issue with nested dependencies |
| `bd list` | Yes | List of issues with counts |
| `bd ready` | Yes | Issues with no blockers |
| `bd blocked` | Yes | Blocked issues with blocker info |
| `bd stats` | Yes | Summary statistics |
| `bd dep list <id>` | Yes | Dependencies of an issue |
| `bd dep tree <id>` | Yes | Dependency tree with depth info |
| `bd graph <id>` | Yes | Graph layout with layers |

## Core Issue Schema

The base issue structure appears in most commands:

```typescript
interface Issue {
  // Required fields (always present)
  id: string;              // e.g., "gt-xyz", "ti-abc.1"
  title: string;
  status: Status;
  priority: number;        // 0-4 (0=P0/critical, 4=P4/backlog)
  issue_type: IssueType;
  created_at: string;      // RFC3339 timestamp with timezone
  created_by: string;      // Actor name (e.g., "mayor", "inanna")
  updated_at: string;      // RFC3339 timestamp

  // Optional fields (omitted if not set)
  description?: string;    // Markdown content
  assignee?: string;       // Agent address (e.g., "tidepool/polecats/slit")
  labels?: string[];       // Array of label strings
  parent?: string;         // Parent issue ID
  closed_at?: string;      // RFC3339 timestamp (only on closed issues)
  close_reason?: string;   // Reason for closure (only on closed issues)
}

type Status =
  | "open"
  | "in_progress"
  | "closed"
  | "hooked"       // Assigned to an agent
  | "blocked"      // Has unresolved blockers
  | "deferred"
  | "tombstone";   // Soft-deleted

type IssueType =
  | "task"
  | "bug"
  | "feature"
  | "epic"
  | "molecule"      // Workflow/swarm coordinator
  | "message"       // Mail bead
  | "agent"         // Agent status bead
  | "merge-request";
```

## Command-Specific Schemas

### `bd show <id> --json`

Returns an array with a single issue including full dependency information:

```typescript
interface ShowResult extends Issue {
  dependencies?: DependencyInfo[];  // Issues this depends on
  dependents?: DependencyInfo[];    // Issues that depend on this
}

interface DependencyInfo extends Issue {
  dependency_type: DependencyType;
}

type DependencyType =
  | "parent-child"  // Hierarchical relationship
  | "blocks"        // Blocking dependency
  | "relates-to";   // Bidirectional link
```

Example output:
```json
[
  {
    "id": "gt-hda.1",
    "title": "Foundation - LSP/BD Integration Layer",
    "status": "open",
    "priority": 1,
    "issue_type": "epic",
    "created_at": "2026-01-03T21:20:49.587218824-08:00",
    "created_by": "mayor",
    "updated_at": "2026-01-03T21:38:43.789022383-08:00",
    "labels": ["infrastructure"],
    "dependencies": [
      {
        "id": "gt-hda",
        "title": "[Epic] Urchin Context Infrastructure",
        "status": "hooked",
        "priority": 2,
        "issue_type": "epic",
        "assignee": "tidepool/nux",
        "dependency_type": "parent-child"
      }
    ],
    "dependents": [
      {
        "id": "gt-hda.2",
        "title": "Target 1 - Context Construction",
        "status": "open",
        "priority": 2,
        "issue_type": "epic",
        "dependency_type": "blocks"
      }
    ],
    "parent": "gt-hda"
  }
]
```

### `bd list --json`

Returns array of issues with count summaries instead of full dependency objects:

```typescript
interface ListResult extends Issue {
  dependency_count?: number;   // Number of issues this depends on
  dependent_count?: number;    // Number of issues depending on this
}
```

### `bd blocked --json`

Returns blocked issues with blocker information:

```typescript
interface BlockedResult extends Issue {
  blocked_by_count: number;
  blocked_by: string[];  // Array of blocking issue IDs
}
```

### `bd stats --json`

Returns summary statistics:

```typescript
interface StatsResult {
  summary: {
    total_issues: number;
    open_issues: number;
    in_progress_issues: number;
    closed_issues: number;
    blocked_issues: number;
    deferred_issues: number;
    ready_issues: number;
    tombstone_issues: number;
    pinned_issues: number;
    epics_eligible_for_closure: number;
    average_lead_time_hours: number;
  };
  recent_activity: {
    hours_tracked: number;
    commit_count: number;
    issues_created: number;
    issues_closed: number;
    issues_updated: number;
    issues_reopened: number;
    total_changes: number;
  };
}
```

### `bd dep tree <id> --json`

Returns flattened tree with depth information:

```typescript
interface TreeResult extends Issue {
  depth: number;        // 0 = root, 1 = direct dependency, etc.
  parent_id: string;    // ID of the root issue
  truncated: boolean;   // True if subtree was cut off (depth limit)
}
```

### `bd graph <id> --json`

Returns graph with layout information for visualization:

```typescript
interface GraphResult {
  issues: Issue[];
  layout: {
    Nodes: Record<string, NodeLayout>;
    Layers: string[][];  // Array of ID arrays per layer
    MaxLayer: number;
    RootID: string;
  };
  root: Issue;
}

interface NodeLayout {
  Issue: Issue;
  Layer: number;      // Y-axis position
  Position: number;   // X-axis position within layer
  DependsOn: string[] | null;
}
```

## Notes for BD Effect Implementation

### Parsing Approach

1. **Timestamps**: Use `time` library's `parseTimeM` with RFC3339 format
2. **Optional fields**: Use `Maybe` types, rely on aeson's `(.:?)` operator
3. **Enums**: Define ADT for Status/IssueType, derive FromJSON

### Recommended Haskell Types

```haskell
data BeadInfo = BeadInfo
  { biId          :: Text
  , biTitle       :: Text
  , biDescription :: Maybe Text
  , biStatus      :: BeadStatus
  , biPriority    :: Int
  , biType        :: BeadType
  , biAssignee    :: Maybe Text
  , biCreatedAt   :: UTCTime
  , biCreatedBy   :: Text
  , biUpdatedAt   :: UTCTime
  , biClosedAt    :: Maybe UTCTime
  , biCloseReason :: Maybe Text
  , biLabels      :: [Text]
  , biParent      :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON)

data BeadStatus
  = StatusOpen
  | StatusInProgress
  | StatusClosed
  | StatusHooked
  | StatusBlocked
  | StatusDeferred
  | StatusTombstone
  deriving (Generic, FromJSON, ToJSON)

data BeadType
  = TypeTask
  | TypeBug
  | TypeFeature
  | TypeEpic
  | TypeMolecule
  | TypeMessage
  | TypeAgent
  | TypeMergeRequest
  deriving (Generic, FromJSON, ToJSON)
```

### Executor Strategy

Shell out to `bd` with `--json` flag, parse response:

```haskell
runBD :: BD a -> IO a
runBD (GetBead beadId) = do
  output <- readProcess "bd" ["show", T.unpack beadId, "--json"] ""
  case eitherDecode (BSL.pack output) of
    Left err -> throwIO $ BDParseError err
    Right [issue] -> pure (Just issue)
    Right [] -> pure Nothing
    Right _ -> throwIO $ BDUnexpectedResponse "multiple issues"
```

## Edge Cases

1. **Empty description**: Field is omitted entirely, not `null`
2. **No labels**: Field is omitted, not empty array
3. **Unicode**: Angle brackets escaped as `\u003c`/`\u003e` in JSON
4. **Timestamps**: Always include timezone offset (e.g., `-08:00`)
5. **Large descriptions**: No truncation in JSON output
