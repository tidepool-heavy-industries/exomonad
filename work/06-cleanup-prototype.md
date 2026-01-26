# Task 06: Cleanup Prototype Code

**Epic:** [00-epic-docgen-graph.md](00-epic-docgen-graph.md)
**Depends On:** [05-e2e-mcp-testing.md](05-e2e-mcp-testing.md)
**Blocks:** None (final task)

## Goal

Remove or deprecate the ScoutGemma prototype code once the graph-based implementation is validated. Clean up the codebase to avoid confusion.

## Context

### Code to Remove/Deprecate

After E2E testing confirms the graph implementation works:

| File | Current Purpose | Action |
|------|-----------------|--------|
| `Scout/DocGen/Gemma.hs` | ScoutGemma effect + Ollama HTTP | Remove entirely |
| `Scout/DocGen.hs` | Hand-rolled BFS exploreLoop | Remove or keep as reference |
| `Scout/Tools.hs` | SelectSymbolsTool (unused) | Remove |
| `Scout/Heuristics.hs` | Pattern-based scoring | Keep if useful, else remove |
| `Scout/EdgeTypes.hs` | Edge classification | Evaluate - may be useful |

### Code to Keep

| File | Purpose |
|------|---------|
| `Scout/DocGen/Types.hs` | LSPSymbol, TeachingDoc, etc. - still used |
| `Scout/DocGen/Teacher.hs` | FineTrainingTeacher - still used |
| `Scout/LSP.hs` | LSP helpers - still used |
| `Scout/Templates.hs` | May have useful content for Graph/Templates.hs |

## Acceptance Criteria

- [ ] **No dead code** - Removed files are gone, not commented out
- [ ] **No duplicate implementations** - One way to do DocGen (graph)
- [ ] **Imports cleaned up** - No unused imports in remaining files
- [ ] **Build succeeds** - `cabal build` passes
- [ ] **Tests pass** - Any existing tests still work
- [ ] **CLAUDE.md updated** - Documentation reflects new structure

## Subtasks

### 6.1 Audit Current Usage

Before deleting, verify nothing depends on the prototype:

```bash
# Find usages of ScoutGemma
grep -r "ScoutGemma" src/

# Find usages of exploreLoop
grep -r "exploreLoop" src/

# Find usages of runScoutGemmaHTTP
grep -r "runScoutGemmaHTTP" src/
```

### 6.2 Remove ScoutGemma

```bash
# Remove the file
rm src/ExoMonad/Control/Scout/DocGen/Gemma.hs

# Remove from cabal file
# Edit exomonad-control-server.cabal, remove from exposed-modules

# Remove imports from other files
# Edit files that imported Gemma.hs
```

### 6.3 Remove Hand-Rolled BFS

If `Scout/DocGen.hs` is superseded by graph:

```bash
# Option A: Remove entirely
rm src/ExoMonad/Control/Scout/DocGen.hs

# Option B: Rename to preserve as reference
mv src/ExoMonad/Control/Scout/DocGen.hs src/ExoMonad/Control/Scout/DocGen.hs.bak

# Option C: Keep but mark deprecated
# Add comment: "-- DEPRECATED: Use Scout/Graph.hs instead"
```

### 6.4 Remove Unused Tools

```bash
# If SelectSymbolsTool is not used by graph
rm src/ExoMonad/Control/Scout/Tools.hs
```

### 6.5 Consolidate Types

Move any types from removed files that are still needed:

```haskell
-- If extractCandidates was useful, move to Graph/Handlers.hs or a utility module
-- If TeachState is superseded by ExploreState, remove it
```

### 6.6 Update Exports

```haskell
-- In exomonad-control-server.cabal
exposed-modules:
  -- Remove:
  -- ExoMonad.Control.Scout.DocGen
  -- ExoMonad.Control.Scout.DocGen.Gemma
  -- ExoMonad.Control.Scout.Tools

  -- Keep/Add:
  ExoMonad.Control.Scout.Graph
  ExoMonad.Control.Scout.Graph.Types
  ExoMonad.Control.Scout.Graph.Templates
  ExoMonad.Control.Scout.Graph.Handlers
  ExoMonad.Control.Scout.Graph.Runner
  ExoMonad.Control.Scout.DocGen.Types  -- Still needed
  ExoMonad.Control.Scout.DocGen.Teacher
  ExoMonad.Control.Scout.LSP
```

### 6.7 Update CLAUDE.md

```markdown
## Key Modules

| Module | LOC | Purpose |
|--------|-----|---------|
| **Scout/Graph.hs** | ~100 | DocGen graph definition |
| **Scout/Graph/Handlers.hs** | ~200 | Node handler implementations |
| **Scout/Graph/Runner.hs** | ~80 | Graph execution entry point |
| **Scout/Graph/Templates.hs** | ~60 | LLM node templates |
| **Scout/Graph/Types.hs** | ~100 | Graph-specific types |
| **Scout/DocGen/Types.hs** | 188 | LSPSymbol, TeachingDoc, etc. |
| **Scout/DocGen/Teacher.hs** | 94 | FineTrainingTeacher instance |
...
```

### 6.8 Final Build and Test

```bash
# Clean build
cabal clean
cabal build exomonad-control-server

# Run any tests
cabal test exomonad-control-server

# Verify E2E still works
./test/e2e/test-e2e.sh
```

## Files to Remove

| File | Reason |
|------|--------|
| `Scout/DocGen/Gemma.hs` | Replaced by LLM effect in graph |
| `Scout/DocGen.hs` | Replaced by Graph/Runner.hs |
| `Scout/Tools.hs` | Unused (execution via graph, not tool) |
| `Scout/Heuristics.hs` | Evaluate - may not be needed |
| `Scout/EdgeTypes.hs` | Evaluate - may not be needed |

## Files to Modify

| File | Changes |
|------|---------|
| `exomonad-control-server.cabal` | Update exposed-modules |
| `Handler/MCP.hs` | Remove old imports |
| `CLAUDE.md` | Update module inventory |

## Verification

```bash
# No references to removed modules
grep -r "Scout.DocGen.Gemma" src/ && echo "FAIL" || echo "PASS"
grep -r "ScoutGemma" src/ && echo "FAIL" || echo "PASS"
grep -r "exploreLoop" src/ && echo "FAIL" || echo "PASS"

# Build succeeds
cabal build exomonad-control-server

# No unused imports (hlint)
hlint src/

# E2E test passes
./test/e2e/test-e2e.sh
```

## Notes

- Don't delete until E2E is passing (Task 05)
- Git history preserves the prototype if needed later
- Consider a "deprecation branch" if you want easy comparison
