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

### ctxHiddenClocks Added to Mood Templates
- Added `<hidden_threats>` section to `scene/main.jinja`
- Added `<hidden_threats>` section to `action/main.jinja`
- DM can now reference hidden clocks in narration without revealing mechanical state

### Precarity Signature Simplified
- Removed unused `hunted :: Bool` and `recovering :: Bool` params from `calculatePrecarity`
- Removed unused params from `precarityScore`
- `buildDMContext` call site simplified
- If hunted/recovering mechanics are added later, add fields to PlayerState

### ctxTone Now Modulates Prose
- Added `<tone_modulation>` section to `scene/main.jinja`
- Added `<tone_modulation>` section to `action/main.jinja`
- Added `<tone_modulation>` section to `aftermath/main.jinja`
- Each Tone value (Tense, ToneNeutral, Comedic, Dark, Hopeful, Mysterious) provides prose guidance

### Per-Mood Tool Filtering Implemented
- Added per-mood tool lists: `sceneToolList`, `actionToolList`, `aftermathToolList`, `traumaToolList`, `bargainToolList`
- Added `toolsForMood :: DMMood -> [Value]` function in Tools.hs
- Loop.hs now uses `toolsForMood mood` instead of `dmTools`
- LLM only sees tools appropriate for current mood

---

## Summary Table

| Issue | Severity | Type | Status |
|-------|----------|------|--------|
| (empty) | | | |

All known issues have been addressed.
