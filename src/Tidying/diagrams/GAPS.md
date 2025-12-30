# Diagrams vs Actual System: Gap Analysis

No gaps. All documented gaps have been fixed.

---

## Recently Fixed (Phase 6)

### 1. Dead Code: `isOverwhelmedSignal` ✅

**Problem:** Function defined in State.hs but never called in routing logic.

**Fix:** Wired into Loop.hs:105-110 as extraction post-processor. Now upgrades intent to `IntentHelp` when overwhelm signals detected but LLM didn't classify as help.

---

### 2. TidyingEvent Count Typo ✅

**Problem:** tool-execution.md said "11 Events" but there were 12.

**Fix:** Updated header to "All 12 Events".

---

### 3. Choice.choiceReveals Field Inconsistency ✅

**Problem:** Tools.hs:197 referenced `Q.choiceReveals` which didn't exist in Question.hs.

**Fix:** Removed stale field reference from Tools.hs:197. This was dead code from when reveals feature was removed.

---

### 4. Missing Transition Paths in state-machine.md ✅

**Problem:** Surveying → Sorting triggers table was incomplete.

**Fix:** Added all fast-track paths:
- Buried chaos
- hasBlockedFunction + hasFunction
- overwhelmed signal + hasFunction
- hasFunction + hasAnchors

---

### 5. Canned Response Table Inaccuracies ✅

**Problem:** AskItemDecision incorrectly listed as LLM-generated; 5 canned responses missing.

**Fix:** Updated table with all 11 canned responses and correct text from Act.hs.

---

## Previously Completed Work

### Code Cleanup (Phases 1-3)
- Removed legacy Situation-based routing system
- Removed dead code: `hasFirstTarget`, `asLastAnxiety`
- Simplified Question DSL (removed ConditionalQ, QuestionGroup, reveals)

### Diagram Fixes (Phase 4)
- Updated `ActiveState` in state-machine.md (removed `asLastAnxiety`)

### Diagram Additions (Phase 5)
- **ItemDisposition** enum → tool-execution.md
- **Overwhelm signals** list → ooda-loop.md
- **Split suggestion patterns** → state-machine.md
- **TidyingEvent full list** → tool-execution.md
- **Constants** reference → state-machine.md
- **Tool flows** (AskSpaceFunction, ConfirmDone) already in tool-execution.md
