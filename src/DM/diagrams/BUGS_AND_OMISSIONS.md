# Bugs, Gaps, and Omissions

Issues found during diagram-vs-code audit. Some are bugs, some are dead code, some are design debt.

---

## MEDIUM BUGS (Active)

*None*

---

## LOW PRIORITY (Gaps/Design Debt)

*None*

---

## FIXED (2025-12-29)

### continueScene Dead Code Removed
- Removed `continueScene` field from TurnOutput type in Output.hs
- Removed from FromJSON parser and emptyTurnOutput default
- Removed from sceneOutputSchema, actionOutputSchema, aftermathOutputSchema in Templates.hs
- Removed from trauma/main.jinja and _shared/output_format.jinja templates

### Bargain Return Now Resets Stress
- Added `stress = 0` reset after any bargain acceptance in Tools.hs
- Prevents "bounce" behavior where player at 9 stress could immediately trigger trauma
- All bargain cost types (heat, clock, faction, etc.) now give stress relief

### Visionary Draft Template Fixed
- Changed `mvcWhatHappened` to `mvcWhatAchieved` in aftermath/visionary_draft.jinja line 4
- Template now correctly references existing MVC field

### MVC Fields Now Surfaced in Templates
- Added `mvcAdvantageSource` to action/main.jinja for controlled position context
- Added `mvcPotentialTrauma` to action/main.jinja for desperate position warning
- Added `mvcEscapeRoute` to aftermath/main.jinja for setback outcomes
- Added `mvcTraumaType` and `mvcWhatBroke` to trauma/main.jinja for trauma context

### BetweenScenes Handler Verified Present
- `handleBetweenScenes` fully implements the between-scenes flow
- Handles heat escalation, threat clock ticking, clock consequences
- Generates LLM transition narration
- Presents options: BSLayLow, BSRecover, BSWorkGoal, BSNewScene, BSEndSession
- `applyBetweenScenesChoice` applies chosen option and creates new scene

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
| All issues fixed | — | — | Complete |
