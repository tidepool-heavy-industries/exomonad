# Habitica Executor - Gamification API Integration

Interprets the `Habitica` effect by making HTTP calls to the Habitica API for task gamification.

## When to Read This

Read this if you're:
- Building agents with gamification features
- Understanding task completion rewards
- Debugging Habitica API issues
- Working with the Habitica effect types

## Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│ Agent Effects                                                        │
│   fetchTodos / scoreTask taskId Up / createTodo "New task"          │
└──────────────────────────────────────┬──────────────────────────────┘
                                       │ Habitica effect
                                       ▼
┌─────────────────────────────────────────────────────────────────────┐
│ Habitica Executor (HTTP client)                                      │
│   GET /api/v3/tasks/user                                            │
│   POST /api/v3/tasks/:id/score/:direction                           │
│   POST /api/v3/tasks/user                                           │
└──────────────────────────────────────┬──────────────────────────────┘
                                       │ JSON/HTTP
                                       ▼
┌─────────────────────────────────────────────────────────────────────┐
│ Habitica API (https://habitica.com/api/v3)                           │
└─────────────────────────────────────────────────────────────────────┘
```

## Usage

```haskell
import Tidepool.Habitica.Executor (runHabitica, HabiticaConfig(..))
import Tidepool.Effects.Habitica

config :: HabiticaConfig
config = HabiticaConfig
  { hcBaseUrl = "https://habitica.com/api/v3"
  , hcUserId = "your-user-id"
  , hcApiToken = "your-api-token"
  }

main :: IO ()
main = runM $ runHabitica config $ do
  -- Fetch all todos
  todos <- fetchTodos

  -- Score a task (complete it)
  result <- scoreTask (TaskId "task-id") Up

  -- Create new todo
  newTodo <- createTodo "Complete documentation"

  pure (todos, result, newTodo)
```

## Effect Operations

| Operation | API Endpoint | Returns |
|-----------|--------------|---------|
| `fetchTodos` | `GET /tasks/user?type=todos` | `[FetchedTodo]` |
| `scoreTask id dir` | `POST /tasks/:id/score/:dir` | `ScoreResult` |
| `createTodo text` | `POST /tasks/user` | `TodoId` |
| `fetchUser` | `GET /user` | `UserInfo` |
| `scoreChecklist todoId itemId` | `POST /tasks/:id/checklist/:itemId/score` | `()` |

## Configuration

```haskell
data HabiticaConfig = HabiticaConfig
  { hcBaseUrl  :: Text  -- API base URL
  , hcUserId   :: Text  -- x-api-user header
  , hcApiToken :: Text  -- x-api-key header
  }

defaultHabiticaConfig :: HabiticaConfig
-- Base URL set, credentials empty (must be filled in)
```

## Getting Credentials

1. Log in to [Habitica](https://habitica.com)
2. Go to Settings → API
3. Copy User ID and API Token
4. Store in environment variables or config file

```bash
export HABITICA_USER_ID="your-user-id"
export HABITICA_API_TOKEN="your-api-token"
```

## Error Handling

API errors are wrapped in `HabiticaError`:

```haskell
data HabiticaError
  = HabiticaNetworkError Text
  | HabiticaAPIError Int Text  -- Status code + message
  | HabiticaParseError Text
```

## Key Modules

| Module | Purpose |
|--------|---------|
| `Executor.hs` | Effect interpreter, HTTP client |

## Gamification Pattern

Habitica integration enables reward loops for agent work:

1. Agent completes a task (e.g., fixes bug)
2. Agent calls `scoreTask taskId Up`
3. User gets XP/gold in Habitica
4. Positive reinforcement for using agents

## Related Documentation

- [effects/CLAUDE.md](../CLAUDE.md) - Effect interpreter pattern
- [effects/habitica/CLAUDE.md](../habitica/CLAUDE.md) - Effect types (standalone)
