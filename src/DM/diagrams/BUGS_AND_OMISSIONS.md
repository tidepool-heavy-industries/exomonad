# Bugs, Gaps, and Omissions

Issues found during diagram-vs-code audit. Some are bugs, some are dead code, some are design debt.

---

## FIXED (2025-12-29)

### MoodAftermath Flow Restored
- Removed inline `diceAction` structured output handling from `Loop.hs`
- `spend_die` tool now populates `pendingOutcome` for `resolve` to read
- Flow is now: Scene → engage → Action → spend_die → resolve → Aftermath → accept → Scene
- Aftermath template will now render with proper `ActionToAftermathContext`

### Duplicate Dice Mechanisms Resolved
- Removed inline `diceAction` processing from Loop.hs
- Only `spend_die` tool remains as the canonical dice mechanism
- Removed `DiceAction` type from Output.hs (dead code)
- Updated action template to guide LLM to call `spend_die` tool

### PendingOutcome Now Populated
- `SpendDie` tool now sets `pendingOutcome` before returning
- `resolve` tool can read dice context for aftermath

### Dead Code Removed: EntryFromDowntime
- Removed `EntryFromDowntime` from `SceneEntryContext` sum type
- Removed `DowntimeToSceneContext` type
- Removed `emptyDowntimeToSceneContext`
- Removed template handling in `scene/main.jinja`
- Downtime phase is not implemented; if added later, type can be restored

### Dead Code Removed: Unused Tool Filter Lists
- Removed `bargainToolNames` and `actionToolNames` from Tools.hs
- Per-mood tool filtering was never implemented
- Template guidance steers tool usage (enforcement remains a future option)

---

## Gap: ctxHiddenClocks (in mood templates)

Hidden clocks are built (`Context.hs:213-214`) but only consumed by legacy `dm_turn.jinja`:

| Template | Uses ctxHiddenClocks? |
|----------|----------------------|
| dm_turn.jinja | Yes (line 42) |
| scene/main.jinja | No |
| action/main.jinja | No |
| aftermath/main.jinja | No |
| trauma/main.jinja | No |
| bargain/main.jinja | No |

### Impact
DM can't reference hidden threats in mood-specific templates.

### Fix
Add ctxHiddenClocks section to mood templates where relevant (scene, action).

---

## Gap: Precarity flags hardcoded

`calculatePrecarity` supports `hunted` and `recovering` flags (`Context.hs:165`):
```haskell
calculatePrecarity ps hunted recovering
```

But `buildDMContext` always passes `False False` (`Context.hs:226`):
```haskell
precarity = calculatePrecarity world.player False False
```

### Impact
Precarity never accounts for hunted/recovering status even if the character has those conditions.

### Fix
Add `hunted :: Bool` and `recovering :: Bool` to `PlayerState` or derive from wanted level / trauma.

---

## Gap: Tool filtering not implemented

Per-mood tool filtering is defined in diagrams but not enforced in code:
- All tools are available in all moods
- Template guidance steers usage contextually
- LLM could theoretically call wrong tools (template voice prevents this)

### Options
1. Implement filtering (pass different tool lists per mood)
2. Accept template-guidance-only approach (current)

---

## Gap: ctxTone underused

`ctxTone` is set from `world.tone` but only displayed in one place:
- `templates/scene/main.jinja:283`: `**Tone:** {{ ctxTone }}`

Not used for prose modulation anywhere. The `Tone` type exists (`State.hs`) but doesn't affect template behavior.

---

## Summary Table

| Issue | Severity | Type | Status |
|-------|----------|------|--------|
| ctxHiddenClocks not in mood templates | Low | Gap | Open |
| Precarity flags hardcoded | Low | Missing feature | Open |
| Tool filtering not implemented | Low | Design choice | Open |
| ctxTone underused | Low | Gap | Open |
