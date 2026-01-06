# tidepool-habitica/ - Type-Safe Habitica API Types

Haskell package providing type-safe Habitica API types and operations.

## What This Is

A types-only library that defines:
1. **Domain types** - Tasks, users, checklists, todos
2. **GADT operations** - Type-safe API operation specifications
3. **Response parsing** - Aeson instances for Habitica JSON responses

The key insight: **Operations are fully typed**. `GetTasks Dailys` returns `[HabiticaTask]`, `CreateTodo text` returns `TodoId`. If it compiles, the payload shape is correct.

## Files

| File | Purpose |
|------|---------|
| `Types.hs` | Core domain types (`TaskId`, `TodoId`, `TaskType`, `Direction`) |
| `Op.hs` | GADT operations (`HabiticaOp a` - typed by return value) |
| `Response.hs` | API response types (`UserInfo`, `HabiticaTask`, `ScoreResult`) |
| `Error.hs` | Error types for API failures |
| `Habitica.hs` | Re-exports for convenient imports |

## Operations (Op.hs)

```haskell
data HabiticaOp a where
  GetUser           :: HabiticaOp UserInfo
  GetTasks          :: TaskType -> HabiticaOp [HabiticaTask]
  FetchTodos        :: HabiticaOp [FetchedTodo]
  ScoreTask         :: TaskId -> Direction -> HabiticaOp ScoreResult
  CreateTodo        :: Text -> HabiticaOp TodoId
  AddChecklistItem  :: TodoId -> Text -> HabiticaOp ChecklistItemId
```

## Usage

This package defines types only. Execution happens in:
- `tidepool-native-gui/habitica-executor/` - Native HTTP execution
- `deploy/src/handlers/habitica.ts` - Cloudflare Worker execution

```haskell
import Tidepool.Habitica (HabiticaOp(..), TaskType(..))

-- Define what to do (types-only)
op :: HabiticaOp [HabiticaTask]
op = GetTasks Dailys
```

## Related Documentation

- [tidepool-native-gui/CLAUDE.md](../tidepool-native-gui/CLAUDE.md) - Native executors including habitica-executor
- [deploy/CLAUDE.md](../deploy/CLAUDE.md) - Cloudflare Worker Habitica handler
- [tidepool-core/CLAUDE.md](../tidepool-core/CLAUDE.md) - Habitica effect in `Effects/Habitica.hs`
