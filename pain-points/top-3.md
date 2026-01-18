# Top 3 Tools to Build First

If I could only have 3 tools, these would deliver the most value.

---

## 1. find-constructors

**Why #1:**
- **High pain (6/10)**, high frequency (5-10x per session)
- **Critical for refactoring:** Adding a field? Find all constructor calls
- **Immediate value:** Works with pure LSP + heuristics
- **Builds foundation:** Establishes pattern for context-based filtering

**Impact:**
- Save 2-3 minutes per search × 5-10 searches = **15-30 min/day**
- Reduce errors when refactoring (missing a constructor call breaks build)

**Implementation estimate:** 1-2 days

**Example usage:**
```bash
$ find-constructors ExploreState

Handlers.hs:123:  ExploreState { esTopic = "test", esFrontier = [] }
Types.hs:87:      defaultExploreState = ExploreState "test" [] Set.empty 20
Runner.hs:156:    initial = ExploreState query mempty mempty budget

Total: 3 constructor calls
```

**Refactoring scenario:**
```diff
  data ExploreState = ExploreState
    { esTopic    :: Text
    , esFrontier :: [SymbolKey]
    , esVisited  :: Set SymbolKey
    , esBudget   :: Int
+   , esDepth    :: Int  -- NEW FIELD
    }
```

Tool finds 3 constructor sites that need updating. Without it, easy to miss one.

---

## 2. find-effects

**Why #2:**
- **Highest pain (8/10)** across all workflows
- **Essential for effect-heavy code:** Tidepool uses freer-simple extensively
- **Prevents propagation errors:** Know what constraints to add
- **Educational:** Understand transitive dependencies

**Impact:**
- Save 5-10 minutes per search × 3-5 searches = **15-50 min/day**
- Reduce constraint errors (missing `Member Foo effs` breaks compilation)
- Understand architecture (what does this function really need?)

**Implementation estimate:** 3-5 days (more complex than find-constructors)

**Example usage:**
```bash
$ find-effects handleScoutTool

Direct: [Log]
Via exploreEff: [LSP, Memory ExploreState, Goto "finalize" TeachingDoc]
Via runExploration: [Log]

Total: [LSP, Log, Memory ExploreState, Goto "finalize" TeachingDoc]
Call depth: 3

Interpreters needed:
  - runLSP (for LSP)
  - runLog (for Log)
  - runMemory (for Memory ExploreState)
  - runGraph (for Goto)
```

**Refactoring scenario:**
```haskell
-- I want to add a cache. What do I need?
cacheResults :: ??? => FilePath -> Eff effs CachedData
cacheResults path = do
  -- Uses file I/O, state, and logging...
  -- What constraints do I need?
```

Run `find-effects` on functions I'm calling → know exactly what constraints to add.

---

## 3. find-callers

**Why #3:**
- **Moderate pain (5/10)**, very high frequency (10+ per session)
- **Better than raw LSP:** Filters noise (imports, type sigs)
- **Quick to implement:** Pure filtering, no recursion
- **Enables impact analysis:** Who depends on this function?

**Impact:**
- Save 1-2 minutes per search × 10+ searches = **10-20 min/day**
- Faster code navigation
- Safer refactoring (know all call sites)

**Implementation estimate:** 1 day

**Example usage:**
```bash
$ find-callers exploreEff

Actual call sites:

Handlers.hs:203:  result <- exploreEff query
Runner.hs:87:     runM $ runLSP session $ exploreEff q
Test.hs:42:       testResult = runPure $ exploreEff testQuery

Total: 3 call sites (filtered 8 other references)
```

**vs raw LSP findReferences:**
```
Memory.hs:135:  exploreEff :: Member LSP effs => Query -> Eff effs Result  [DEFINITION]
Handlers.hs:5:  import Scout (exploreEff)                                  [IMPORT]
Handlers.hs:203: result <- exploreEff query                                [CALL]
README.md:42:    Use `exploreEff` to explore the codebase                  [COMMENT]
Test.hs:15:     import Scout (exploreEff)                                  [IMPORT]
Test.hs:42:     testResult = runPure $ exploreEff testQuery                [CALL]
Runner.hs:12:   import Scout (exploreEff)                                  [IMPORT]
Runner.hs:87:   runM $ runLSP session $ exploreEff q                       [CALL]
Types.hs:23:    -- See exploreEff for implementation                       [COMMENT]
Export.hs:8:    module Scout (exploreEff) where                            [EXPORT]
Bench.hs:67:    benchmark "exploreEff" $ nf exploreEff testQuery           [STRING]
```

**11 results → 3 relevant.** Tool saves manual filtering.

---

## Why This Order?

### 1. find-constructors first:
- Simplest (pure LSP + heuristics)
- Immediate ROI
- Validates infrastructure
- Builds confidence

### 2. find-effects second:
- Highest pain point
- More complex (recursion, call graphs)
- Leverages patterns from find-constructors
- Big productivity win

### 3. find-callers third:
- Quick implementation (1 day)
- Frequent use (10+ per day)
- Easy win after complex find-effects
- Momentum for Tier 2/3 tools

---

## Combined Impact

**Time saved per day:** 40-100 minutes
**Errors prevented:** Missing constructor calls, constraint propagation bugs
**Developer experience:** Faster navigation, better understanding of codebase

**ROI:** 3 tools × 2-5 days implementation = 1-2 weeks investment
**Payoff:** 40-100 min/day × 5 days = 3-8 hours/week saved
**Break-even:** 2-3 weeks

---

## Next Steps After Top 3

Once these are working:

1. **Gather metrics:** How often are they used? Which is most valuable?
2. **Identify gaps:** Where do heuristics fail? False positives/negatives?
3. **User feedback:** What other workflows are painful?
4. **Build Tier 2:** find-pattern-matches, show-fields, show-constructors

The Top 3 establish the foundation. Everything else builds on their patterns.
