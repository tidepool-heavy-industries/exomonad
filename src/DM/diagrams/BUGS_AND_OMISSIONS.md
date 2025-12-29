# Bugs, Gaps, and Omissions

Issues found during diagram-vs-code audit. Some are bugs, some are dead code, some are design debt.

---

## MEDIUM BUGS (Active)

### continueScene Field is Dead Code
**Severity: MEDIUM** | **Location:** Output.hs:63, Templates.hs (multiple)

`TurnOutput.continueScene` is parsed but never checked anywhere. LLM might set `continueScene: false` expecting it to end the scene, but it has no effect.

**Fix:** Remove from TurnOutput and schema, or implement scene-ending logic. Requires touching multiple files.

---

### Bargain Return Doesn't Reset Stress
**Severity: MEDIUM** | **Location:** Tools.hs:630-640

When accepting a bargain (not trauma type), stress stays at current value. If player is at 9 stress and accepts a bargain, they return to previous mood and could immediately trigger trauma on next turn. Creates "bounce" behavior.

**Fix:** Either reset stress to 0 after any bargain, or run trauma check after `accept_bargain`.

---

## LOW PRIORITY (Gaps/Design Debt)

### BetweenScenes Handler Missing
`PhaseBetweenScenes` exists and `Retreat` tool transitions to it, but no handler processes it. Game would hang.

### Unused MoodVariantContext Fields
These fields are defined and populated but never referenced in templates:
- `mvcAdvantageSource` (controlled actions)
- `mvcPotentialTrauma` (desperate actions, disasters)
- `mvcEscapeRoute` (setback outcomes)
- `mvcTraumaType`, `mvcWhatBroke` (trauma variant)

Could be surfaced for richer narrative context or removed.

### Visionary Draft Template Bug
`aftermath/visionary_draft.jinja` line 4 references `mvcWhatHappened` which doesn't exist. Use `mvcWhatAchieved` or `mvcWhatWentWrong`.

---

## FIXED (2025-12-29)

### pendingOutcome Now Cleared After Use
- Added `modify @WorldState $ \s -> s { pendingOutcome = Nothing }` in Resolve tool
- Added clearing in Accept tool's state modification
- Stale die/tier data no longer persists across action sequences

### Trauma Check Takes Priority Over Bargain
- In SpendDie, now check `stress >= 9` BEFORE checking empty pool
- If stress hits 9 from dice outcome, transition to MoodTrauma immediately
- Bargain only triggers if stress < 9 AND pool empty

### unresolvedThreats Now Capped
- Added `take 5` to unresolvedThreats in applyTurnOutput
- Matches recentCosts which already had `take 3`
- Prevents unbounded growth in long sessions

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
| continueScene dead code | MEDIUM | Dead code | Open |
| Bargain return stress bounce | MEDIUM | Design | Open |
| BetweenScenes handler missing | LOW | Incomplete | Open |
| Unused mvc fields | LOW | Dead code | Open |
| Visionary draft template bug | LOW | Bug | Open |
