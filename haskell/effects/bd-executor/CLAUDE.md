# BD Executor - Beads Task Tracking Integration

Interprets the `BD` effect by calling the `bd` CLI tool for task/issue tracking.

## When to Read This

Read this if you're:
- Integrating beads task tracking into an agent
- Understanding how agents read/write issues
- Debugging bead operations
- Working with urchin's context generation

## Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│ Agent Effect                                                         │
│   getBead "gt-abc.1.2" / createBead { title = "New task" }          │
└──────────────────────────────────────┬──────────────────────────────┘
                                       │ BD effect
                                       ▼
┌─────────────────────────────────────────────────────────────────────┐
│ BD Executor (subprocess)                                             │
│   bd show gt-abc.1.2 --json                                         │
│   bd create -t task "New task" --json                               │
└──────────────────────────────────────┬──────────────────────────────┘
                                       │ JSON stdout
                                       ▼
┌─────────────────────────────────────────────────────────────────────┐
│ .beads/ directory (git-native storage)                               │
└─────────────────────────────────────────────────────────────────────┘
```

## Usage

```haskell
import Tidepool.BD.Executor (runBDIO, BDConfig(..))
import Tidepool.Effects.BD

config :: BDConfig
config = BDConfig
  { bcBeadsDir = Nothing  -- Use default .beads/
  , bcQuiet = True        -- Suppress CLI output
  }

main :: IO ()
main = runM $ runBDIO config $ do
  -- Read operations
  maybeBead <- getBead "gt-abc.1.2"
  deps <- getBlockers "gt-abc.1.2"
  ready <- getReady  -- Issues with no blockers

  -- Write operations
  newId <- createBead $ defaultCreateInput { cbiTitle = "New task" }
  addLabel newId "auto-created"
  closeBead newId
```

## Effect Operations

### Read Operations

| Operation | CLI Command | Returns |
|-----------|-------------|---------|
| `getBead id` | `bd show <id> --json` | `Maybe Bead` |
| `getBlockers id` | `bd deps <id> --json` | `[BeadId]` |
| `getBlocking id` | `bd blocking <id> --json` | `[BeadId]` |
| `getLabels id` | `bd labels <id> --json` | `[Text]` |
| `getChildren id` | `bd children <id> --json` | `[BeadId]` |
| `getReady` | `bd ready --json` | `[Bead]` |
| `listByStatus s` | `bd list --status=<s> --json` | `[Bead]` |
| `listByType t` | `bd list --type=<t> --json` | `[Bead]` |

### Write Operations

| Operation | CLI Command | Effect |
|-----------|-------------|--------|
| `createBead input` | `bd create ...` | Creates new bead |
| `updateBead id patch` | `bd update <id> ...` | Updates bead fields |
| `closeBead id` | `bd close <id>` | Marks as closed |
| `reopenBead id` | `bd reopen <id>` | Reopens closed bead |
| `addLabel id label` | `bd label add <id> <label>` | Adds label |
| `removeLabel id label` | `bd label rm <id> <label>` | Removes label |
| `addDep id dep` | `bd dep add <id> <dep>` | Adds dependency |
| `removeDep id dep` | `bd dep rm <id> <dep>` | Removes dependency |

## Configuration

```haskell
data BDConfig = BDConfig
  { bcBeadsDir :: Maybe FilePath  -- Override .beads/ location
  , bcQuiet :: Bool               -- Suppress CLI stderr
  }

defaultBDConfig :: BDConfig
defaultBDConfig = BDConfig Nothing True
```

## Key Modules

| Module | Purpose |
|--------|---------|
| `Executor.hs` | Effect interpreter, CLI subprocess calls |
| `GitExecutor.hs` | Git operations for beads sync |
| `Prime/Graph.hs` | Context generation graph (urchin) |
| `Prime/Runner.hs` | urchin prime runner |

## Integration with urchin

The `Prime/` modules support `urchin prime` context generation:

```bash
urchin prime  # Generates context from beads + git + LSP
```

This is used by coding agents to bootstrap with project context.

## Related Documentation

- [effects/CLAUDE.md](../CLAUDE.md) - Effect interpreter pattern
- [Root CLAUDE.md](../../../CLAUDE.md) - Beads overview in project context
