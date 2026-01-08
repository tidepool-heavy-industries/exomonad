# Baseline Evaluation Plan

## Experiment: URL Shortener

Single experiment, multiple branches, quality-focused evaluation.

## Branches Under Test

| Branch | Hypothesis | Key Metric |
|--------|------------|------------|
| `wt/cutter` | Sonnet > Haiku for code quality | type_design, idiomaticity |
| (branch 2) | TBD | TBD |
| (branch 3) | TBD | TBD |
| (branch 4) | TBD | TBD |

## Rubric Definitions

### Automated (Binary/Numeric)

| Metric | Pass Condition |
|--------|----------------|
| `compiles` | `cabal build` exits 0 |
| `tests_pass` | `cabal test` exits 0 |
| `endpoints` | 3 endpoints detected in API type |

### Quality (1-5 Scale)

| Dimension | 1 (Poor) | 3 (Adequate) | 5 (Excellent) |
|-----------|----------|--------------|---------------|
| **spec_fidelity** | Missing endpoints, wrong behavior | All endpoints present, mostly correct | Exact match to acceptance criteria |
| **type_design** | Raw String everywhere | Some newtypes, basic records | Full newtype wrappers, smart constructors |
| **test_quality** | No real tests | Unit tests for happy path | Property-based, covers edge cases |
| **impl_quality** | Undefined/stubs remain | Works but fragile | Clean, handles all edges |
| **coherence** | Types don't match impl | Reasonable fit | Types drive impl perfectly |
| **idiomaticity** | Fights the language | Standard patterns | Exemplary Haskell |

### Weights (for aggregate score)

```
spec_fidelity:  25%  (must implement what was asked)
type_design:    20%  (this is "types-first" after all)
test_quality:   15%
impl_quality:   15%
coherence:      15%  (key for multi-agent)
idiomaticity:   10%
```

## Run Protocol

### Per Branch

```bash
# 1. Checkout branch
git checkout <branch>

# 2. Ensure zellij session exists
zellij --session types-first-baseline

# 3. Run baseline (in another terminal)
cd types-first-dev
./scripts/run-baseline.sh <branch>-run1

# 4. (Optional) Run 2 more for variance
./scripts/run-baseline.sh <branch>-run2
./scripts/run-baseline.sh <branch>-run3
```

### After All Runs

```bash
# Compare automated scores
./scripts/compare-runs.sh cutter-run1 branch2-run1 branch3-run1

# Quality scoring (manual or LLM judge)
# Review: ~/tidepool-labs/dev-runs/baseline/*/artifacts/
```

## Narrative Extraction

For each run, capture:

### Types Phase
- What data types were defined?
- What API shape was chosen?
- Any interesting design decisions?

### Impl Agent
- Storage strategy (IORef/Map/etc)?
- Hash function for short codes?
- Error handling approach?

### Tests Agent
- Unit vs property-based?
- Which acceptance criteria covered?
- Any generated edge cases?

### Coherence Check
- Do tests import and use the actual types?
- Does impl satisfy the type signatures?
- Any mismatches between what tests expect and impl provides?

## Success Criteria

A branch "wins" if it:
1. Passes automated checks (compiles + tests pass)
2. Has higher aggregate quality score
3. Shows improvement on its hypothesized metric

## Output Artifacts

Per run:
```
~/tidepool-labs/dev-runs/baseline/<run>/
├── run-info.json          # Metadata
├── automated-scores.json  # Compiles, tests, endpoints
├── workflow.log           # Full execution trace
├── build.log              # cabal build output
├── final-build.log        # Post-merge build
├── final-test.log         # Post-merge test
└── artifacts/
    ├── src/UrlShortener.hs  # Generated impl
    ├── test/Main.hs         # Generated tests
    ├── git-log.txt          # Commit history
    └── git-diff.txt         # Changes made
```
