# exomonad-habitica/ - Type-Safe Habitica API Types

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
- `exomonad-native-gui/habitica-interpreter/` - Native HTTP execution
...
- [exomonad-native-gui/CLAUDE.md](../exomonad-native-gui/CLAUDE.md) - Native interpreters including habitica-interpreter
- [deploy/CLAUDE.md](../deploy/CLAUDE.md) - Cloudflare Worker Habitica handler
- [exomonad-core/CLAUDE.md](../exomonad-core/CLAUDE.md) - Habitica effect in `Effects/Habitica.hs`
